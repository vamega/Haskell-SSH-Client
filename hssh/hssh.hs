module Main where

import System.Environment (getArgs, getProgName)
import System.IO
import Network.SSH.Client

main :: IO ()
main = do
    args <- getArgs
    print args
    case args of
        [host, port] -> sshClient host (read port :: Int)
        _ -> usageExit
  where
    usageExit = do
        name <- getProgName
        putStrLn $ "Usage : " ++ name ++ " host port"
