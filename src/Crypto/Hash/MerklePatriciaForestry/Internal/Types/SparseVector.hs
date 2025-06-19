module Crypto.Hash.MerklePatriciaForestry.Internal.Types.SparseVector (
  SparseVector,
  mkSparseVector,
  unSparseVector,
) where

import Crypto.Hash.MerklePatriciaForestry.Internal.Types.HexDigit (HexDigit, allHexDigits)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- | Vector of length 16. See 'mkSparseVector' for details relating construction.
newtype SparseVector a = SparseVector {unSparseVector :: [Maybe a]}

-- | Construct a sparse vector from a map. We put 'Nothing' for indices that are not present in the map.
mkSparseVector :: forall a. Map HexDigit a -> SparseVector a
mkSparseVector m = SparseVector $ map (`Map.lookup` m) allHexDigits