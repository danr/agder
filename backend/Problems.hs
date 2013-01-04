{-# LANGUAGE OverloadedStrings #-}
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

problems :: FilePath
problems = "problems"

solutions :: FilePath
solutions = "solutions"

getProblemList :: IO [FilePath]
getProblemList = filter (('.' /=) . head) `fmap` getDirectoryContents problems

doesProblemExist :: String -> IO Bool
doesProblemExist p = (p `elem`) `fmap` getProblemList

assertProblemExists :: String -> IO ()
assertProblemExists p = doesProblemExist p >>= flip unless
    (error "Problem does not exist!")

problemDir :: String -> FilePath
problemDir p = problems </> p

solutionsDir :: String -> FilePath
solutionsDir p = solutions </> p

agdaExt :: FilePath -> FilePath
agdaExt p = p `addExtension` "agda"

getProblem :: String -> IO Value
getProblem p = do
    assertProblemExists p
    problem <- T.readFile (problemDir p </> agdaExt p)
    definitions_list <- do
        let defns_file = problemDir p </> agdaExt "Definitions"
        ok <- doesFileExist defns_file
        sequence [ T.readFile defns_file | ok ]
    return $ object $
        [ "problem" .= toJSON problem ] ++
        [ "definitions" .= toJSON definitions | definitions <- definitions_list ]

verifier :: FilePath
verifier = "Verifier.agda"

solveProblem :: String -> Text -> IO Value
solveProblem p t = do
    assertProblemExists p

    let bs         = T.encodeUtf8 t
        hash       = showDigest (sha1 bs)
        d          = problemDir p
        d_hash     = solutionsDir p </> hash
        d_verifier = d_hash </> verifier

    createDirectoryIfMissing True d_hash
    T.writeFile (d_hash </> agdaExt p) t

    copyFile (d </> verifier) d_verifier

    let defns_file = d </> agdaExt "Definitions"
    defns_exists <- doesFileExist defns_file
    when defns_exists $ copyFile defns_file (d_hash </> agdaExt "Defintions")

    (exc, out, err) <-
        readProcessWithExitCode
            "agda" ["--safe", "-i" ++ d_hash, d_verifier] ""
    let exit_value = case exc of
            ExitSuccess -> 0
            ExitFailure i -> toInteger i
    return $ object
        [ "exitcode" .= toJSON exit_value
        , "stdout" .= toJSON out
        , "stderr" .= toJSON err
        ]

