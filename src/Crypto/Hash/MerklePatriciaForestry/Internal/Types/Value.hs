module Crypto.Hash.MerklePatriciaForestry.Internal.Types.Value (
  Value,
  valueFromString,
  valueFromText,
) where

import Crypto.Hash.MerklePatriciaForestry.Internal.Types.Key (keyFromString, keyFromText)
import Data.ByteString (ByteString)
import Data.Text (Text)

type Value = ByteString

valueFromString :: String -> Value
valueFromString = keyFromString

valueFromText :: Text -> Value
valueFromText = keyFromText