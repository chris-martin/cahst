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
  { protocolVersion :: Required 1 (Enumeration ProtocolVersion)
  , sourceId        :: Required 2 (Value Text)
  , destinationId   :: Required 3 (Value Text)
  , namespace       :: Required 4 (Value Text)
  , payloadType     :: Required 5 (Enumeration PayloadType)
  , payloadUtf8     :: Optional 6 (Value Text)
  , payloadBinary   :: Optional 7 (Value ByteString)
  } deriving (Generic, Show)

instance Encode CastMessage
instance Decode CastMessage

data AuthChallenge = AuthChallenge
  deriving (Generic, Show)

instance Encode AuthChallenge
instance Decode AuthChallenge

data AuthResponse = AuthResponse
  { signature             :: Required 1 (Value ByteString)
  , clientAuthCertificate :: Required 2 (Value ByteString)
  , clientCa              :: Required 3 (Value ByteString)
  } deriving (Generic, Show)

instance Encode AuthResponse
instance Decode AuthResponse

data AuthErrorType = INTERNAL_ERROR | NO_TLS
  deriving (Enum, Show)

data AuthError = AuthError
  { errorType :: Required 1 (Enumeration AuthErrorType)
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
