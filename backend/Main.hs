{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy.Encoding
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static
import Web.Scotty
import Data.Monoid

import Problems

main :: IO ()
main = scotty 3000 $ do

    middleware logStdout
    middleware $ staticPolicy $
        mconcat [ addBase "frontend"
                , noDots
                , foldr1 (<|>) (map hasSuffix ["html", "js", "css", "jpg", "txt"])
                ]
        <|> only (zip ["","/"] (repeat "frontend/index.html"))

    get "/problems" $ json =<< liftIO getProblemList

    get "/problem/:problem" $ \problem -> do
        ok <- liftIO (doesProblemExist problem)
        unless ok next
        header "Content-type" "text/plain; charset=utf-8"
        json =<< liftIO (getProblem problem)

    post "/solve/:problem" $ \problem -> do
        ok <- liftIO (doesProblemExist problem)
        unless ok next
        content <- do
            files_content <- files
            case files_content of
                [(c,_)] -> return c
                _       -> decodeUtf8 `fmap` body
        json =<< liftIO (solveProblem problem content)

