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
import Network.SSH.Transport

sshClient :: HostName -> Int -> IO ()
sshClient host port = withSocketsDo $ do
    h <- connectTo host (PortNumber $ fromIntegral port)
    hSetBuffering h LineBuffering

    runHandlePipeline handshakePipeline h
    runPacketPipeline sshKeyExchangeProxy h

    hClose h
    where
        runHandlePipeline hf h =  P.runProxy $ hGetLineS h >-> hf >-> hPutD h
        handshakePipeline = unpackFromByteString
                            >-> P.putStrLnD
                                >-> sshHandshakeProxy
                                    >-> P.putStrLnD
                                        >-> packToByteString
        packetPipeline pf = unpackFromPacket
                            >-> P.putStrLnD
                                >-> pf
                                    >-> P.putStrLnD
                                        >-> packToPacket
        runPacketPipeline pf h = runHandlePipeline (packetPipeline pf)  h


sshHandshakeProxy :: (P.Proxy p) => x -> p x String x String IO ()
sshHandshakeProxy = P.mapMD $ \_ -> return $ sshVersion ++ "\r\n"


unpackFromPacket :: (P.Proxy p) => x -> p x BS.ByteString x String IO ()
unpackFromPacket =  P.mapD unpackPacket

packToPacket :: (P.Proxy p) => x -> p x String x BS.ByteString IO ()
packToPacket =  P.mapD packPacket

sshKeyExchangeProxy :: (P.Proxy p) => x -> p x String x String IO ()
sshKeyExchangeProxy = undefined


sshClientReact :: String -> IO String
sshClientReact bs = return $ sshVersion ++ "\r\n"

sshClientProxy :: (P.Proxy p) => x -> p x String x String IO ()
sshClientProxy = P.mapMD sshClientReact

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
hPutD :: (P.Proxy p) => Handle -> x -> p x BS.ByteString x BS.ByteString IO ()
hPutD h =  P.useD (BS.hPut h)

unpackFromByteString :: (P.Proxy p) => x -> p x BS.ByteString x String IO ()
unpackFromByteString =  P.mapD BS.unpack

packToByteString :: (P.Proxy p) => x -> p x String x BS.ByteString IO ()
packToByteString =  P.mapD BS.pack
