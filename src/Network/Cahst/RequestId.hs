module Network.Cahst.RequestId
    ( RequestId(..)
    ) where

import           Control.Monad  (join)
import           Data.Aeson     (ToJSON (..))
import           Data.Bifunctor (bimap)
import           Data.Int       (Int32)
import           System.Random  (Random (..))

newtype RequestId = RequestId Int32 deriving (Eq, Show)

instance Bounded RequestId where
    minBound = RequestId 1
    maxBound = RequestId maxBound

instance Enum RequestId where
    toEnum = RequestId . fromIntegral
    fromEnum (RequestId i) = fromIntegral i

instance Random RequestId where
    randomR range = mapFst toEnum . randomR (join bimap fromEnum range)
    random = randomR (minBound, maxBound)

instance ToJSON RequestId where
    toJSON (RequestId i) = toJSON i

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)
