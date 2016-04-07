{-# LANGUAGE OverloadedStrings #-}

module Network.Cahst (main) where

import qualified Network.Cahst.Message as M
import qualified Network.Cahst.Socket  as S

import           Control.Monad         (forever)

main :: IO ()
main = do
    c <- S.newConnection

    S.send c M.Connect
    S.send c M.getStatus
    S.send c $ M.launch "YouTube"

    forever $ do
        x <- S.recv c
        putStrLn $ show x
