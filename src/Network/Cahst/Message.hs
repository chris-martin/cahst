-- | Thanks to https://github.com/thibauts/node-castv2

{-# LANGUAGE OverloadedStrings #-}

module Network.Cahst.Message
     (
     -- * Namespaces
       Namespace(..)
     , nsStr

     -- * Messages
     , Message(..)
     , ConnectionMessage(..)
     , HeartbeatMessage(..)
     , ReceiverMessage(..)
     , ReceiverCommand(..)
     ) where

import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Text  (Text)

data Namespace = ConnectionNS | HeartbeatNS | ReceiverNS | AuthNS

nsStr :: Namespace -> String
nsStr ConnectionNS = "urn:x-cast:com.google.cast.tp.connection"
nsStr HeartbeatNS  = "urn:x-cast:com.google.cast.tp.heartbeat"
nsStr ReceiverNS   = "urn:x-cast:com.google.cast.receiver"
nsStr AuthNS       = "urn:x-cast:com.google.cast.tp.deviceauth"

class Message a where
    messageNs :: a -> Namespace

data ConnectionMessage = Connect | Close

instance Message ConnectionMessage where
    messageNs _ = ConnectionNS

instance ToJSON ConnectionMessage where
    toJSON Connect = object
        [ "type" .= ("CONNECT" :: Text) ]
    toJSON Close = object
        [ "type" .= ("CLOSE" :: Text) ]

data HeartbeatMessage = Ping | Pong

instance Message HeartbeatMessage where
    messageNs _ = HeartbeatNS

instance ToJSON HeartbeatMessage where
    toJSON Ping = object
        [ "type" .= ("PING" :: Text) ]
    toJSON Pong = object
        [ "type" .= ("PONG" :: Text) ]

data ReceiverCommand =
    Launch Text | Stop Text | GetStatus
    | GetAppAvailability [Text]

data ReceiverMessage = ReceiverMessage ReceiverCommand Int

instance Message ReceiverMessage where
    messageNs _ = ReceiverNS

instance ToJSON ReceiverMessage where
    toJSON (ReceiverMessage command requestId) =
        object $ ("requestId" .= requestId) : case command of
            Launch appId ->
                [ "type" .= ("LAUNCH" :: Text)
                , "appId" .= appId ]
            Stop sessionId ->
                [ "type" .= ("STOP" :: Text)
                , "sessionId" .= sessionId ]
            GetStatus ->
                [ "type" .= ("GET_STATUS" :: Text) ]
            GetAppAvailability appIds ->
                [ "type" .= ("GET_APP_AVAILABILITY" :: Text)
                , "appId" .= appIds ]
