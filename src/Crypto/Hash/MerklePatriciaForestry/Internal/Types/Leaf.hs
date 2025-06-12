module Crypto.Hash.MerklePatriciaForestry.Internal.Types.Leaf (
  Leaf (leafKey, leafValue, leafSuffix, leafHash),
  mkLeaf,
  updateLeaf,
) where

import Crypto.Hash.MerklePatriciaForestry.Internal.Hash
import Crypto.Hash.MerklePatriciaForestry.Internal.Types.HexDigit
import Crypto.Hash.MerklePatriciaForestry.Internal.Types.Key (Key)
import Crypto.Hash.MerklePatriciaForestry.Internal.Types.Value (Value (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Function ((&))

data Leaf = Leaf
  { leafKey :: Key
  , leafValue :: Value
  , leafSuffix :: [HexDigit]
  , leafHash :: ByteString
  }
  deriving stock (Show)

mkLeaf :: Key -> Value -> [HexDigit] -> Leaf
mkLeaf key val suffix =
  Leaf
    { leafKey = key
    , leafValue = val
    , leafSuffix = suffix
    , leafHash = digest (hashHead suffix <> hashTail suffix <> digest (unValue val))
    }

updateLeaf :: Leaf -> Value -> Leaf
updateLeaf Leaf{..} newVal =
  mkLeaf leafKey newVal leafSuffix

hashHead :: [HexDigit] -> ByteString
hashHead suffix =
  if even (length suffix)
    then
      BS.singleton 0xff
    else
      BS.singleton 0x00 <> BS.singleton (head suffix & unHexDigit & fromIntegral)

hashTail :: [HexDigit] -> ByteString
hashTail suffix =
  if even (length suffix)
    then
      hexDigitsToByteString suffix
    else
      hexDigitsToByteString (tail suffix)