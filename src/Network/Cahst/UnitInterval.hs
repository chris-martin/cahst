module Network.Cahst.UnitInterval
    ( UnitInterval
    , fromSci
    , toSci
    ) where

import           Data.Aeson      (ToJSON (..))
import           Data.Scientific (Scientific, scientific)

newtype UnitInterval = UnitInterval { toSci :: Scientific }
    deriving (Eq, Ord, Show)

instance Bounded UnitInterval where
    minBound = UnitInterval zero
    maxBound = UnitInterval one

instance ToJSON UnitInterval where
    toJSON (UnitInterval i) = toJSON i

instance Num UnitInterval where
    (UnitInterval a) + (UnitInterval b) = fromSci $ a + b
    (UnitInterval a) - (UnitInterval b) = fromSci $ a - b
    (UnitInterval a) * (UnitInterval b) = fromSci $ a * b
    abs = id
    signum = fromSci . signum . toSci
    fromInteger = fromSci . fromInteger

instance Fractional UnitInterval where
    fromRational r = UnitInterval $ bound sci
        where sci = fromRational r
    recip _ = UnitInterval one

zero, one :: Scientific
zero = scientific 0 0
one  = scientific 1 0

bound :: Scientific -> Scientific
bound = max zero . min one

fromSci :: Scientific -> UnitInterval
fromSci = UnitInterval . bound
