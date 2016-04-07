{-# LANGUAGE OverloadedStrings #-}

module Network.Cahst (main) where

import qualified Network.Cahst.Connection as C
import qualified Network.Cahst.Message    as M
import qualified Network.Cahst.Protobuf   as P

import qualified Data.Binary              as Binary
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Int                 as Int
import           Data.Monoid              ((<>))
import qualified Data.Serialize.Get       as G

main :: IO ()
main = do
    c <- C.newConnection

    C.send c M.Connect
    C.send c M.getStatus
    C.send c $ M.launch "YouTube"
    C.send c $ M.setVolume 0.6

    loop c mempty

loop :: C.Connection -> BS.ByteString -> IO ()
loop c bs1 = do
    bs2 <- C.recv c
    putStrLn $ "Recv bytes: " <> show bs2
    let bs = bs1 <> bs2
    let len = fromIntegral $ getLengthPrefix bs
    let (body, rest) = BS.splitAt len $ BS.drop 4 bs
    if BS.length body /= len
      then loop c bs
      else
        case G.runGet P.decodeMessage body of
          Left err -> putStrLn err
          Right result -> do
            putStrLn $ "Recv message: " <> show (result :: P.CastMessage)
            loop c rest

getLengthPrefix :: BS.ByteString -> Int.Int32
getLengthPrefix bs = Binary.decode firstBytes
  where firstBytes :: LBS.ByteString
        firstBytes = LBS.take 4 $ LBS.fromStrict bs
