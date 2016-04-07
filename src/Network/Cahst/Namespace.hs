module Network.Cahst.Namespace
    ( Namespace, Namespaced(..)
    ) where

import           Data.Text (Text)

type Namespace = Text

class Namespaced a where
    namespace :: a -> Namespace
