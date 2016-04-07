{-# LANGUAGE OverloadedStrings #-}

module Network.Cahst (main) where

import qualified Network.Cahst.Message as M
import qualified Network.Cahst.Socket  as S

import           Control.Monad         (forever)

import qualified Network.TLS           as TLS

main :: IO ()
main = do
    ctx <- S.newTlsContext

    TLS.sendData ctx $ S.jsonPayload M.Connect
    TLS.sendData ctx $ S.jsonPayload $ M.ReceiverMessage M.getStatus 1
    TLS.sendData ctx $ S.jsonPayload $ M.ReceiverMessage (M.launch "YouTube") 2

    forever $ do
        x <- TLS.recvData ctx
        putStrLn $ show x
