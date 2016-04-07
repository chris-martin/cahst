{-# LANGUAGE OverloadedStrings #-}

module Network.Cahst (main) where

import qualified Network.Cahst.Connection as C
import qualified Network.Cahst.Message    as M

import           Control.Monad            (forever)

main :: IO ()
main = do
    c <- C.newConnection

    C.send c M.Connect
    C.send c M.getStatus
    --C.send c $ M.launch "YouTube"
    C.send c $ M.setVolume 0.6

    forever $ do
        x <- C.recv c
        putStrLn $ show x
