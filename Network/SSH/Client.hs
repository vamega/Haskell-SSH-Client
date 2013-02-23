{-# LANGUAGE OverloadedStrings #-}
module Network.SSH.Client (sshClient) where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class (lift)
import Data.ByteString.Char8 as BS
import Control.Proxy((>->))
import qualified Control.Proxy as P
import System.IO as IO
import Network
import Prelude


sshClient :: HostName -> Int -> IO ()
sshClient host port = withSocketsDo $ do
    h <- connectTo host (PortNumber $ fromIntegral port)
    hSetBuffering h LineBuffering
    P.runProxy $ hGetLineS h >-> sshClientProxy >-> hPutD h
    hClose h

sshClientReact :: BS.ByteString -> IO BS.ByteString
sshClientReact bs = do
    Prelude.putStrLn $ BS.unpack bs
    return $ BS.pack "foo"

-- | A 'Producer' that sends lines from a handle downstream as a bytestring
-- | Modified from https://github.com/Gabriel439/Haskell-Pipes-ByteString-Library/blob/master/Control/Proxy/ByteString.hs
hGetLineS :: (P.Proxy p) => Handle -> () -> P.Producer p BS.ByteString IO ()
hGetLineS h () = P.runIdentityP go where
    go = do
        eof <- lift $ IO.hIsEOF h
        if eof
            then return ()
            else do
                bs <- lift $ BS.hGetLine h
                unless (BS.null bs) $ P.respond  bs >> go

-- | A proxy that sends an input bytestring into a handle
hPutD :: (P.Proxy p) => Handle -> x -> p x BS.ByteString x BS.ByteString IO r
hPutD h =  P.useD (BS.hPut h)

sshClientProxy :: (P.Proxy p) => x -> p x BS.ByteString x BS.ByteString IO r
sshClientProxy = P.useD sshClientReact

