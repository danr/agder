{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Problems where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Digest.Pure.SHA
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import System.Directory
import System.Exit
import System.FilePath
import System.Process

import ProcessQueue

problems :: FilePath
problems = "problems"

solutions :: FilePath
solutions = "solutions"

agdaExt :: FilePath -> FilePath
agdaExt p = p `addExtension` "agda"

verifier :: FilePath
verifier = agdaExt "Verifier"

definitions :: FilePath
definitions = agdaExt "Definitions"

description :: FilePath
description = "README.md"

problemDir :: String -> FilePath
problemDir p = problems </> p

solutionsDir :: String -> FilePath
solutionsDir p = solutions </> p

getProblemList :: IO [FilePath]
getProblemList = filter (('.' /=) . head) `fmap` getDirectoryContents problems

doesProblemExist :: String -> IO Bool
doesProblemExist p = (p `elem`) `fmap` getProblemList

assertProblemExists :: String -> IO ()
assertProblemExists p = doesProblemExist p >>= flip unless
    (error "Problem does not exist!")

maybeReadFile :: FilePath -> IO (Maybe Text)
maybeReadFile file = doesFileExist file >>= \ok -> if ok
    then Just `fmap` T.readFile file
    else return Nothing

getMaybeDefinitions,getMaybeDescription :: String -> IO (Maybe Text)
[getMaybeDefinitions,getMaybeDescription] = map
    (\file p -> maybeReadFile (problemDir p </> file))
    [ definitions , description ]

getProblem :: String -> IO Value
getProblem p = do
    assertProblemExists p
    problem <- T.readFile (problemDir p </> agdaExt p)
    m_defns <- getMaybeDefinitions p
    m_desc <- getMaybeDescription p
    return $ object $
        [ "problem" .= toJSON problem ] ++
        [ "description" .= toJSON desc | Just desc <- [m_desc] ] ++
        [ "definitions" .= toJSON defns | Just defns <- [m_defns] ]

data Attempt = Attempt
    { attempt_hash :: String
    , attempt_problem :: String
    }
  deriving (Eq,Show)

solveProblem :: ProcessQueue Attempt -> String -> Text -> IO Value
solveProblem pq p t = do
    assertProblemExists p

    let bs         = T.encodeUtf8 t
        hash       = showDigest (sha1 bs)
        d          = problemDir p
        d_hash     = solutionsDir p </> hash
        d_verifier = d_hash </> verifier

    createDirectoryIfMissing True d_hash

    T.writeFile (d_hash </> agdaExt p) t

    copyFile (d </> verifier) d_verifier

    m_defns <- getMaybeDefinitions p
    case m_defns of
        Just defns -> T.writeFile (d_hash </> definitions) defns
        Nothing -> return ()

    let attempt = Attempt
            { attempt_hash = hash
            , attempt_problem = p
            }

    enqueue pq attempt "agda" ["--safe", "-i" ++ d_hash, d_verifier] ""

    Result{..} <- getResultWithTag pq attempt

    let exit_value = case res_exit_code of
            ExitSuccess -> 0
            ExitFailure i -> toInteger i

    return $ object
        [ "exitcode" .= toJSON exit_value
        , "stdout" .= toJSON res_stdout
        , "stderr" .= toJSON res_stderr
        , "time" .= toJSON res_time
        ]

