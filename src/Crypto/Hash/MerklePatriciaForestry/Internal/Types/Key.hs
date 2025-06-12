module Crypto.Hash.MerklePatriciaForestry.Internal.Types.Key (
  Key (..),
  keyFromString,
  keyFromText,
) where

import Control.Arrow ((>>>))
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

{- | A key in the trie. It is simply a `ByteString`.

It has an `IsString` instance, so you can use it as a string literal. Internally, strings are UTF-8 encoded.
-}
newtype Key = Key {unKey :: ByteString}
  deriving stock (Show)

instance IsString Key where
  fromString = keyFromString

keyFromString :: String -> Key
keyFromString = Text.pack >>> keyFromText

keyFromText :: Text -> Key
keyFromText = coerce . Text.encodeUtf8