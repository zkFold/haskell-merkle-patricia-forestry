module Crypto.Hash.MerklePatriciaForestry (
  module Crypto.Hash.MerklePatriciaForestry.Types,
) where

import Crypto.Hash.MerklePatriciaForestry.Types

import Control.Arrow ((>>>))
import Crypto.Hash.BLAKE2.BLAKE2b (hash)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as BS16
import Data.ByteString.Char8 qualified as BS8
import Data.Char (intToDigit)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Natural (Natural)

digestLength :: Natural
digestLength = 32

digestLengthInt :: Int
digestLengthInt = fromIntegral digestLength

-- | This is the hash of empty trie.
nullHash :: ByteString
nullHash = BS.replicate digestLengthInt 0

digest :: ByteString -> ByteString
digest = hash (fromIntegral digestLength) mempty

data MerklePatriciaForestry
  = MerklePatriciaForestryEmpty
  | MerklePatriciaForestryNode Natural MerklePatriciaForestryNode

-- | \(O(1)\). Is it empty?
null :: MerklePatriciaForestry -> Bool
null MerklePatriciaForestryEmpty = True
null (MerklePatriciaForestryNode _ _) = False

-- | \(O(1)\). The number of elements represented by this trie.
size :: MerklePatriciaForestry -> Natural
size MerklePatriciaForestryEmpty = 0
size (MerklePatriciaForestryNode n _) = n

-- | \(O(1)\). The hash of the root node.
rootHash :: MerklePatriciaForestry -> ByteString
rootHash MerklePatriciaForestryEmpty = nullHash
rootHash (MerklePatriciaForestryNode _ node) = nodeHash node

nodeHash = undefined

data MerklePatriciaForestryNode
  = MerklePatriciaForestryNodeLeaf Leaf
  | MerklePatriciaForestryNodeBranch Branch

-- | Insert a new key and value in the trie. If the key is already present in the trie, the associated value is replaced with the supplied value.
insert :: Key -> Value -> MerklePatriciaForestry -> MerklePatriciaForestry
insert key val mpf = case mpf of
  MerklePatriciaForestryEmpty ->
    MerklePatriciaForestryNode 1 (MerklePatriciaForestryNodeLeaf $ mkLeaf key val keyPath)
  MerklePatriciaForestryNode size node ->
    case node of
      MerklePatriciaForestryNodeLeaf leaf ->
        -- We update the value.
        if keyPath == leafSuffix leaf
          then
            MerklePatriciaForestryNode size (MerklePatriciaForestryNodeLeaf $ mkLeaf key val keyPath)
          else undefined
      MerklePatriciaForestryNodeBranch branch -> undefined
 where
  keyPath = intoPath key

data Leaf = Leaf
  { leafKey :: Key
  , leafValue :: Value
  , leafSuffix :: [HexDigit]
  , leafHash :: ByteString
  }

mkLeaf :: Key -> Value -> [HexDigit] -> Leaf
mkLeaf key val suffix =
  Leaf
    { leafKey = key
    , leafValue = val
    , leafSuffix = suffix
    , leafHash = digest (hashHead suffix <> hashTail suffix <> digest val)
    }

hashHead :: [HexDigit] -> ByteString
hashHead suffix =
  if even (length suffix)
    then
      BS.singleton 0xff
    else
      BS.singleton 0x00 <> BS8.singleton (head suffix & unHexDigit & fromIntegral & intToDigit)

hashTail :: [HexDigit] -> ByteString
hashTail suffix =
  if even (length suffix)
    then
      hexDigitsToByteString suffix
    else
      hexDigitsToByteString (tail suffix)

data Branch = Branch
  { branchHash :: ByteString
  , branchPrefix :: [HexDigit]
  , -- TODO: Shall we use a sparse vector here? But then we'll need to somehow also track the size of children.
    branchChildren :: Map HexDigit MerklePatriciaForestryNode
  }

emptyBranch :: Branch
emptyBranch =
  Branch
    { branchHash = nullHash
    , branchPrefix = []
    , branchChildren = mempty
    }

-- | Turn any key into a path of nibbles.
intoPath :: ByteString -> [HexDigit]
intoPath = digest >>> byteStringToHexDigits

type Key = ByteString

keyFromString :: String -> Key
keyFromString = Text.pack >>> keyFromText

keyFromText :: Text -> Key
keyFromText = Text.encodeUtf8

type Value = ByteString

valueFromString :: String -> Value
valueFromString = keyFromString

valueFromText :: Text -> Value
valueFromText = keyFromText

-- fromList :: [(Key, Value)] -> MerklePatriciaForestry
-- fromList kvs = go $ map (\(k, v) -> (k, v, intoPath k)) kvs
--  where
--   go :: [(Key, Value, ByteString)] -> MerklePatriciaForestry
--   go [] = MerklePatriciaForestry nullHash 0 ""
--   go [(k, v, p)] = case kvs of
--     [] -> MerklePatriciaForestry (digest v) 1 p
--     (k', v', p') : kvs' -> go kvs'

-- go ((k, v, p) : kvs) = case kvs of
--   [] -> MerklePatriciaForestry (digest v) 1 p
--   (k', v', p') : kvs' -> go kvs'
