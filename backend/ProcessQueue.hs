{-# LANGUAGE RecordWildCards, PatternGuards #-}
module ProcessQueue
    ( ProcessQueue
    , Result(..)
    , newQueue
    , runQueue
    , enqueue
    , listen
    , getResultWithTag
    ) where

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.STM
import Data.Function
import System.Exit
import System.Process
import System.TimeIt

data Desc tag = Desc
    { desc_tag :: tag
    , desc_cmd :: String
    , desc_args :: [String]
    , desc_stdin :: String
    }

-- | The result from a process
data Result tag = Result
    { res_tag       :: tag
    , res_time      :: Double
    -- ^ Run time in seconds
    , res_exit_code :: ExitCode
    , res_stdout    :: String
    , res_stderr    :: String
    }
  deriving Show

data ProcessQueue tag = PQ
    { queue         :: TChan (Desc tag)
    , results       :: TChan (Result tag)
    , maybe_timeout :: Maybe Int
    }

-- | Creates a new queue, given an optional timeout in seconds for every command
newQueue :: Maybe Int -> IO (ProcessQueue tag)
newQueue maybe_timeout = do
    queue <- newTChanIO
    results <- newTChanIO
    return PQ {..}

-- | Runs the queue, writing results on a channel available from @listen@
runQueue :: ProcessQueue tag -> IO ()
runQueue PQ{..} = forever $ do
    Desc{..} <- atomically $ readTChan queue
    (time, (exc, out, err)) <- timeItT $ uncurry readProcessWithExitCode
        (cmd_and_args desc_cmd desc_args) desc_stdin
    atomically $ writeTChan results $ Result
        { res_tag       = desc_tag
        , res_time      = time
        , res_exit_code = exc
        , res_stdout    = out
        , res_stderr    = err
        }
  where
    cmd_and_args cmd args
        | Just timeout <- maybe_timeout = ("timeout",[show timeout,cmd] ++ args)
        | otherwise = (cmd,args)

-- | Enqueues a new job on a queue, given a description tag, command, arguments
--   and stdin
enqueue :: ProcessQueue tag -> tag -> String -> [String] -> String -> IO ()
enqueue PQ{..} desc_tag desc_cmd desc_args desc_stdin = atomically $
    writeTChan queue $ Desc {..}

-- | Gets a duplicate of the channel of results from a process queue
listen :: ProcessQueue tag -> IO (TChan (Result tag))
listen = atomically . dupTChan . results

-- | Waits indefinitely for a result from a specific tag
getResultWithTag :: Eq tag => ProcessQueue tag -> tag -> IO (Result tag)
getResultWithTag pq t = do
    ch <- listen pq
    fix $ \loop -> do
        r <- atomically $ readTChan ch
        if res_tag r == t then return r else loop

