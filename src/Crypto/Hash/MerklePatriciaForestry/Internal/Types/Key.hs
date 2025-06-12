module Crypto.Hash.MerklePatriciaForestry.Internal.Types.Key (
  Key,
  keyFromString,
  keyFromText,
) where

import Control.Arrow ((>>>))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

type Key = ByteString

keyFromString :: String -> Key
keyFromString = Text.pack >>> keyFromText

keyFromText :: Text -> Key
keyFromText = Text.encodeUtf8