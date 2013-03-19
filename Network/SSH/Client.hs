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

data SSHMode = HandShake | KeySwap

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

    let initSSHContext = SSHContext { sshMode = HandShake }
    P.runProxy $ socketHandler sock 4096 >-> sshClientProxy initSSHContext

    sClose sock

sshHandShake :: String
sshHandShake  = sshVersion ++ "\r\n"

sshClientProxy :: (P.Proxy p) => SSHContext -> () -> P.Client p SocketContext SocketContext IO ()
sshClientProxy context () = P.runIdentityP (clientLoop context) where
    clientLoop context =  case sshMode context of
                                HandShake -> handShake
                                _ -> clientMain
        where
            handShake = do
                let handShakeDatum = return $ BS.pack sshHandShake
                let initSocketContext = SocketContext { socketReadDatum = [],
                                                        socketWriteDatum = [handShakeDatum],
                                                        socketStream = Write,
                                                        socketPacketize = NoPackets }
                isc <- P.request initSocketContext
                rem_datum <- takeFullSplashPage isc
                P.request $ isc { socketReadDatum = rem_datum, socketStream = Rewind}
                clientLoop $ context { sshMode = KeySwap }
                where
                    takeFullSplashPage sc = do
                        sc2 <- P.request $ (sc :: SocketContext) { socketStream = Read }
                        spashBS <- lift $ head $ socketReadDatum (sc2 :: SocketContext)
                        rem_datum <- lift $ printSSHSplashPage spashBS
                        case rem_datum of
                            [] -> takeFullSplashPage $ sc2 { socketReadDatum = [] }
                            _ -> return rem_datum
                    printSSHSplashPage :: BS.ByteString -> IO SocketDatum
                    printSSHSplashPage bs = do
                        let (splash,handShakeBegin) = case (BS.take 4 bs) == BS.pack "SSH-" of
                                                        True  -> (BS.empty,bs)
                                                        False -> BS.breakSubstring (BS.pack "\r\nSSH-") bs
                        case (BS.length handShakeBegin) == (BS.length bs) of
                            True -> BS.putStr "Server Handshake: "
                                    >> BS.putStrLn handShakeBegin
                                    >> return [return bs]
                            False -> case (BS.length splash) == (BS.length bs)  of
                                        True -> BS.putStrLn "Server Splash: "
                                                >> BS.putStr splash
                                                >> return []
                                        False -> do
                                                    BS.putStrLn splash
                                                    let (serverHandShake,nonHandShake) = BS.breakSubstring (BS.pack "\r\n") (BS.drop 2 handShakeBegin)
                                                    case (BS.length serverHandShake) == ((BS.length handShakeBegin) - 2) of
                                                        True  -> BS.putStr "Server Handshake: "
                                                                 >> BS.putStrLn serverHandShake
                                                                 >> return [return (BS.drop 2 nonHandShake)]
                                                        False -> error "Incorrect Server Handshake"

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
