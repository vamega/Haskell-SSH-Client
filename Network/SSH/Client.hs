{-# LANGUAGE OverloadedStrings #-}
module Network.SSH.Client (sshClient) where

import Prelude
import System.IO as IO
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class (lift)
import Control.Proxy((>->))
import qualified Control.Proxy as P
import Network.SSH.Transport

data SSHMode = VersionHandShake | KeySwap

data SSHContext = SSHContext {
    sshMode :: SSHMode
}

type SocketDatum = [IO BS.ByteString]
data SocketStreamFlag = Read | Write | Rewind
data SocketPacketizeFlag = PacketizeAll | PacketizeNew | NoPackets

data SocketContext = SocketContext {
    socketReadDatum :: SocketDatum,
    socketWriteDatum :: SocketDatum,
    socketStream :: SocketStreamFlag,
    socketPacketize :: SocketPacketizeFlag
}

sshClient :: HostName -> String -> IO ()
sshClient hostname port = withSocketsDo $ do
    addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock KeepAlive 1
    connect sock (addrAddress serveraddr)

    let initSSHContext = SSHContext { sshMode = VersionHandShake }
    P.runProxy $ socketHandler sock 4096 >-> sshClientProxy initSSHContext

    sClose sock

sshVersionHandShake :: String
sshVersionHandShake  = sshVersion ++ "\r\n"

sshClientProxy :: (P.Proxy p) => SSHContext -> () -> P.Client p SocketContext SocketContext IO ()
sshClientProxy context () = P.runIdentityP (clientLoop context) where
    clientLoop context =  case sshMode context of
                                VersionHandShake -> versionHandShake
                                _ -> clientMain
        where
            versionHandShake = do
                let versionHandShakeDatum = return $ BS.pack sshVersion
                let initSocketContext = SocketContext { socketReadDatum = [],
                                                        socketWriteDatum = [versionHandShakeDatum],
                                                        socketStream = Write,
                                                        socketPacketize = NoPackets }
                isc <- P.request initSocketContext
                rem_datum <- preVersionMsgs isc
                P.request $ isc { socketReadDatum = rem_datum, socketStream = Rewind}
                clientLoop $ context { sshMode = KeySwap }
                where
                    preVersionMsgs sc = do
                        sc2 <- P.request $ (sc :: SocketContext) { socketStream = Read }
                        msgBS <- lift $ head $ socketReadDatum (sc2 :: SocketContext)
                        rem_datum <- lift $ printSSHPreVersionMsg msgBS
                        case rem_datum of
                            [] -> preVersionMsgs $ sc2 { socketReadDatum = [] }
                            _ -> return rem_datum
                    printSSHPreVersionMsg :: BS.ByteString -> IO SocketDatum
                    printSSHPreVersionMsg bs = do
                        let (preMsg,versionHandShakeBegin) = case (BS.take 4 bs) == BS.pack "SSH-" of
                                                        True  -> (BS.empty,bs)
                                                        False -> BS.breakSubstring (BS.pack "\r\nSSH-") bs
                        case (BS.length versionHandShakeBegin) == (BS.length bs) of
                            True -> BS.putStr "Server Version Handshake: "
                                    >> BS.putStrLn versionHandShakeBegin
                                    >> return [return bs]
                            False -> case (BS.length preMsg) == (BS.length bs)  of
                                        True -> BS.putStrLn "Server Pre Start Messages: "
                                                >> BS.putStr preMsg
                                                >> return []
                                        False -> do
                                                    BS.putStrLn preMsg
                                                    let (serverVersionHandShake,nonVersionHandShake) = BS.breakSubstring (BS.pack "\r\n") (BS.drop 2 versionHandShakeBegin)
                                                    case (BS.length serverVersionHandShake) == ((BS.length versionHandShakeBegin) - 2) of
                                                        True  -> BS.putStr "Server Version Handshake: "
                                                                 >> BS.putStrLn serverVersionHandShake
                                                                 >> return [return (BS.drop 2 nonVersionHandShake)]
                                                        False -> error "Incorrect Server Version  Handshake"

            clientMain = do
                return ()


socketHandler :: (P.Proxy p) => Socket -> Int -> SocketContext -> P.Server p SocketContext SocketContext IO ()
socketHandler sock recvBufSize = P.runIdentityK socketLoop where
    socketLoop isc = do
        case (socketStream isc) of
            Read ->   do
                            let r = recv sock recvBufSize
                            osc <- P.respond $ isc { socketReadDatum = r:(socketReadDatum isc) }
                            socketLoop osc
            Write ->  do
                            case (socketWriteDatum isc) of
                                [] -> P.respond isc >>= socketLoop
                                wl -> do
                                            w  <- lift $ head wl
                                            lift $ sendAll sock w
                                            osc <- P.respond $ isc { socketWriteDatum = [] }
                                            socketLoop osc
            Rewind -> do
                            let osc = case (socketPacketize isc) of
                                            PacketizeAll -> undefined
                                            _ -> isc
                            P.respond osc
                            socketLoop osc
