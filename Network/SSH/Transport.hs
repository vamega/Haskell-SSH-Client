module Network.SSH.Transport where
import Data.ByteString.Char8 as BS


sshVersion :: String
sshVersion = "SSH-2.0-HaskellSSHClient_0.1"

unpackPacket :: BS.ByteString -> String
unpackPacket = undefined


packPacket :: String -> BS.ByteString
packPacket = undefined
