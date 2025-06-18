module Crypto.Hash.MerklePatriciaForestry.Internal.Types.Value (
  Value (..),
  valueFromString,
  valueFromText,
) where

import Crypto.Hash.MerklePatriciaForestry.Internal.Types.Key (Key (..), keyFromString, keyFromText)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.String (IsString (..))
import Data.Text (Text)
import GHC.Generics (Generic)

{- | A value in the trie. It is simply a `ByteString`.

It has an `IsString` instance, so you can use it as a string literal. Internally, strings are UTF-8 encoded.
-}
newtype Value = Value {unValue :: ByteString}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

instance IsString Value where
  fromString = valueFromString

valueFromString :: String -> Value
valueFromString = coerce . keyFromString

valueFromText :: Text -> Value
valueFromText = coerce . keyFromText