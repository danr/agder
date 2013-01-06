{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
module User where

import Data.Aeson
import GHC.Generics
import Control.Monad
import Control.Monad.STM
import Control.Monad.Random
import Control.Monad.Random.Class
import Control.Concurrent.STM.TVar
import Data.Map (Map)
import Data.String
import Data.Word8
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as T
import qualified Data.Map as M

newtype T = T { d :: Text }
  deriving (Eq, Ord, Generic, Show)

instance FromJSON T
instance ToJSON T

newtype User = User Text
  deriving (Eq, Ord, Generic, Show, IsString)

newtype Salt = Salt ByteString
  deriving (Eq, Generic, Show, IsString)

newtype Hash = Hash ByteString
  deriving (Eq, Generic, Show, IsString)

data Credentials
    = Credentials
        { cred_user :: User
        , cred_hash :: Hash
        }
    | Anonymous
  deriving (Generic, Show)

instance ToJSON Salt
instance ToJSON User
instance ToJSON Hash
instance ToJSON Credentials

instance FromJSON User
instance FromJSON Hash
instance FromJSON Credentials

data UserDB = UserDB
    { salts  :: TVar (Map User Salt)
    , hashes :: TVar (Map User Hash)
    }

data UserStatus
    = CredentialsOK
    | WrongPassword
    | UsernameExists
    | SuccessfulCreation
  deriving (Eq, Ord, Bounded, Enum, Show, Generic)

instance ToJSON UserStatus
instance FromJSON UserStatus

data IncomingSolution = IncomingSolution
    { credentials :: Credentials
    , solution    :: Text
    }
  deriving (Show, Generic)

instance ToJSON IncomingSolution
instance FromJSON IncomingSolution

newUserDB :: IO UserDB
newUserDB = liftM2 UserDB (newTVarIO M.empty) (newTVarIO M.empty)

getSalt :: UserDB -> User -> IO Salt
getSalt udb u = do
    let isOk x = isAsciiLower x || isAsciiUpper x || isDigit x
    salt <- (Salt. B.pack . take 20 . filter isOk) `fmap` getRandoms
    atomically $ do
        salts_ <- readTVar (salts udb)
        case M.lookup u salts_ of
            Just old_salt -> return old_salt
            Nothing -> do
                modifyTVar (salts udb) (M.insert u salt)
                return salt

checkHash :: UserDB -> Credentials -> IO UserStatus
checkHash _ Anonymous = return CredentialsOK
checkHash udb (Credentials u h) = atomically $ do
    h_in_db <- M.lookup u `fmap` readTVar (hashes udb)
    return $ case fmap (h ==) h_in_db of
        Just True -> CredentialsOK
        _         -> WrongPassword
                     -- ^ wrong password is sent even if username is missing

setHash :: UserDB -> Credentials -> IO UserStatus
setHash udb Anonymous = return CredentialsOK
setHash udb (Credentials u h) = atomically $ do
    m <- readTVar (hashes udb)
    if M.member u m
        then return UsernameExists
        else do
            modifyTVar (hashes udb) (M.insert u h)
            return SuccessfulCreation

