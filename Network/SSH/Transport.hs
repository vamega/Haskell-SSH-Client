module Network.SSH.Transport where
import Data.ByteString.Char8 as BS

sshVersion :: String
sshVersion = "SSH-2.0-HaskellSSHClient_0.1"


supportedKeyExchangeMethods :: [String]
supportedKeyExchangeMethods =
    ["diffie-hellman-group1-sha1",
     "diffie-hellman-group14-sha1"]

supportedPublicKeyAlgorithms :: [String]
supportedPublicKeyAlgorithms = ["ssh-dss"]

supportedCiphers :: [(String)]
supportedCiphers = [ ("3des-cbc ")
    			   , ("aes128-cbc")]

supportedMACAlgorithms :: [(String)]
supportedMACAlgorithms = [("hmac-sha1")]

supportedCompressionMethods :: [String]
supportedCompressionMethods = ["none"]


data PacketContext = PacketContext {
	compressionType :: String,
	encryptionType :: String,
	macType :: String
}

--data SSHPacket = SSHPacket {
--	packet_lenght :: {-# UNPACK #-} !Word32,
--	padding_length :: # UNPACK # !Word8,
--	payload ::  [!Word8],
--	random_padding ::  [!Word8],
--	mac ::  [!Word8]
--}

unpackPacket :: BS.ByteString -> String
unpackPacket bs = undefined

packPacket :: String -> BS.ByteString
packPacket = undefined


