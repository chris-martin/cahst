-- | Thanks to https://github.com/thibauts/node-castv2/blob/master/lib/cast_channel.proto

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.Cahst.Protobuf
  ( CastMessage(..)
  , PayloadType(..)
  , ProtocolVersion(..)

  -- * Auth
  , DeviceAuthMessage(..)
  , AuthChallenge(..)
  , AuthResponse(..)
  , AuthError(..)
  , AuthErrorType(..)

  -- * Re-export from Data.ProtocolBuffers
  , encode
  , putField
  ) where

import           Data.ByteString      (ByteString)
import           Data.ProtocolBuffers
import           Data.Text

import           GHC.Generics         (Generic)

data PayloadType = StringPayloadType | BinaryPayloadType
  deriving (Enum, Show)

data ProtocolVersion = CASTV2_1_0
  deriving (Enum, Show)

data CastMessage = CastMessage
  { protocol_version :: Required 1 (Enumeration ProtocolVersion)
  , source_id        :: Required 2 (Value Text)
  , destination_id   :: Required 3 (Value Text)
  , namespace        :: Required 4 (Value Text)
  , payload_type     :: Required 5 (Enumeration PayloadType)
  , payload_utf8     :: Optional 6 (Value Text)
  , payload_binary   :: Optional 7 (Value ByteString)
  } deriving (Generic, Show)

instance Encode CastMessage
instance Decode CastMessage

data AuthChallenge = AuthChallenge
  deriving (Generic, Show)

instance Encode AuthChallenge
instance Decode AuthChallenge

data AuthResponse = AuthResponse
  { signature               :: Required 1 (Value ByteString)
  , client_auth_certificate :: Required 2 (Value ByteString)
  , client_ca               :: Required 3 (Value ByteString)
  } deriving (Generic, Show)

instance Encode AuthResponse
instance Decode AuthResponse

data AuthErrorType = INTERNAL_ERROR | NO_TLS
  deriving (Enum, Show)

data AuthError = AuthError
  { error_type :: Required 1 (Enumeration AuthErrorType)
  } deriving (Generic, Show)

instance Encode AuthError
instance Decode AuthError

data DeviceAuthMessage = DeviceAuthMessage
  { challenge :: Optional 1 (Message AuthChallenge)
  , response  :: Optional 2 (Message AuthResponse)
  , error     :: Optional 3 (Message AuthError)
  } deriving (Generic, Show)

instance Encode DeviceAuthMessage
instance Decode DeviceAuthMessage
