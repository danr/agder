{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy.Encoding
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Scotty

import Problems

main :: IO ()
main = scotty 3000 $ do

    middleware logStdoutDev

    get "/problems" $ json =<< liftIO getProblemList

    get "/problem/:problem" $ \problem -> do
        ok <- liftIO (doesProblemExist problem)
        unless ok next
        header "Content-type" "text/plain; charset=utf-8"
        text =<< liftIO (getProblem problem)

    post "/solve/:problem" $ \problem -> do
        ok <- liftIO (doesProblemExist problem)
        unless ok next
        content <- do
            files_content <- files
            case files_content of
                [(c,_)] -> return c
                _       -> decodeUtf8 `fmap` body
        json =<< liftIO (solveProblem problem content)

