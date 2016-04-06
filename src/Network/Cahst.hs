{-# LANGUAGE OverloadedStrings #-}

module Network.Cahst (main) where

import qualified Network.Cahst.Message    as M
import qualified Network.Cahst.Protobuf   as P

import           Control.Concurrent       (threadDelay)
import           Control.Monad            (forever)

import qualified Data.Aeson
import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Encode
import qualified Data.Binary              as Binary
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import           Data.Default
import qualified Data.Int                 as Int
import           Data.Serialize.Put       (runPutLazy)
import qualified Data.Text                as T
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder

import qualified Network.Socket           as Sock
import qualified Network.TLS              as TLS
import           Network.TLS.Extra.Cipher (ciphersuite_all)

main :: IO ()
main = do
    addr <- getAddr
    s <- Sock.socket Sock.AF_INET Sock.Stream 0
    Sock.connect s addr

    tlsCtx <- TLS.contextNew s tlsParams

    TLS.handshake tlsCtx

    threadDelay 1000000
    send tlsCtx M.Connect
    threadDelay 1000000
    send tlsCtx M.Ping
    threadDelay 1000000
    --send tlsCtx $ M.ReceiverMessage M.GetStatus 9027
    send tlsCtx $ M.ReceiverMessage (M.Launch "YouTube") 17618

    forever $ do
        x <- TLS.recvData tlsCtx
        putStrLn $ show x

  where

    tlsParams = (TLS.defaultParamsClient
                 "192d2f31-7845-49cc-0993-d40e05d1e4e8"
                 BS.empty)
        { TLS.clientSupported = tlsCiphers
        , TLS.clientHooks = tlsHooks }

    -- Support any ciphers, who cares
    tlsCiphers = def { TLS.supportedCiphers = ciphersuite_all }

    -- Don't verify the server cert at all. It's self-signed.
    tlsHooks = def { TLS.onServerCertificate = (\_ _ _ _ -> return []) }

getAddr :: IO Sock.SockAddr
getAddr = do
    xs <- Sock.getAddrInfo Nothing (Just "192.168.1.103") (Just "8009")
    return $ Sock.addrAddress $ head xs

send :: (M.Message m, Aeson.ToJSON m) => TLS.Context -> m -> IO ()
send ctx m = do
    let payload = messagePayload m
    let len = fromIntegral $ LBS.length payload
    _ <- TLS.sendData ctx $ Binary.encode (len :: Int.Int32)
    _ <- TLS.sendData ctx payload
    putStrLn $ "Send: " ++ show payload
    return ()

messagePayload :: (M.Message m, Aeson.ToJSON m) => m -> LBS.ByteString
messagePayload m = runPutLazy $ P.encode $ P.CastMessage
    { P.protocol_version = P.putField P.CASTV2_1_0
    , P.source_id        = P.putField "sender-0"
    , P.destination_id   = P.putField "receiver-0"
    , P.namespace        = P.putField $ T.pack $ M.nsStr $ M.messageNs m
    , P.payload_type     = P.putField P.StringPayloadType
    , P.payload_utf8     = P.putField $ Just $ encodeJsonText m
    , P.payload_binary   = P.putField Nothing
    }

encodeJsonText :: Aeson.ToJSON a => a -> T.Text
encodeJsonText = Data.Text.Lazy.toStrict .
                 Data.Text.Lazy.Builder.toLazyText .
                 Data.Aeson.Encode.encodeToTextBuilder .
                 Data.Aeson.toJSON
