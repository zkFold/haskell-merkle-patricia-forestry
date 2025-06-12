module Crypto.Hash.MerklePatriciaForestry.Internal.Types.Value (
  Value (..),
  valueFromString,
  valueFromText,
) where

import Crypto.Hash.MerklePatriciaForestry.Internal.Types.Key (Key (..), keyFromString, keyFromText)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Text (Text)

newtype Value = Value {unValue :: ByteString}
  deriving stock (Show)

valueFromString :: String -> Value
valueFromString = coerce . keyFromString

valueFromText :: Text -> Value
valueFromText = coerce . keyFromText