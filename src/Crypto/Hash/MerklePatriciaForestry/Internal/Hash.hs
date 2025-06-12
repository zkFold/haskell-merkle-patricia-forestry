module Crypto.Hash.MerklePatriciaForestry.Internal.Hash (
  nullHash,
  digest,
) where

import Crypto.Hash.BLAKE2.BLAKE2b (hash)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import GHC.Natural (Natural)

digestLength :: Natural
digestLength = 32

digestLengthInt :: Int
digestLengthInt = fromIntegral digestLength

-- | This is the hash of empty trie.
nullHash :: ByteString
nullHash = BS.replicate digestLengthInt 0

-- | Obtain the blake2b hash of the given input.
digest :: ByteString -> ByteString
digest = hash digestLengthInt mempty