{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM
import Data.Aeson hiding (json)
import Data.Monoid
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy.Encoding
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static
import Web.Scotty

import Problems
import ProcessQueue
import User


main :: IO ()
main = do
    print (encode (IncomingSolution (Credentials "apa" "bepa") "bepa"))

    print (encode [minBound :: UserStatus .. maxBound])

    pq <- newQueue (Just 60)

    forkIO $ runQueue pq

    user_db <- newUserDB

    scotty 3000 $ do

        middleware logStdout
        middleware $ staticPolicy $ mconcat
            [ addBase "frontend"
            , noDots
            , foldr1 (<|>) (map hasSuffix
                ["html", "js", "css", "jpg", "txt"])
            ] <|> only (zip ["","/"] (repeat "frontend/index.html"))

        get "/problems" $ json =<< liftIO getProblemList

        get "/problem/:problem" $ \problem -> do
            ok <- liftIO (doesProblemExist problem)
            unless ok next
            header "Content-type" "text/plain; charset=utf-8"
            json =<< liftIO (getProblem problem)

        post "/salt" $ do
            T user <- jsonData
            liftIO $ print user
            json =<< liftIO (getSalt user_db (User user))

        post "/register" $ do
            Just creds <- jsonData
            json =<< liftIO (setHash user_db creds)

        post "/login" $ do
            Just creds <- jsonData
            json =<< liftIO (checkHash user_db creds)

        post "/solve/:problem" $ \problem -> do

            prob_ok <- liftIO (doesProblemExist problem)
            unless prob_ok next

            IncomingSolution creds content <- jsonData

            user_status <- liftIO (checkHash user_db creds)
            let add_user_status (Object o)
                    = Object (H.insert "user_status" (toJSON user_status) o)

            if user_status == CredentialsOK

                then json . add_user_status =<<
                        liftIO (solveProblem pq problem content)

                else json user_status

