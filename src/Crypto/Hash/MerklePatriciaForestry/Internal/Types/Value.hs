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

newtype Value = Value {unValue :: ByteString}
  deriving stock (Show)

instance IsString Value where
  fromString = valueFromString

valueFromString :: String -> Value
valueFromString = coerce . keyFromString

valueFromText :: Text -> Value
valueFromText = coerce . keyFromText