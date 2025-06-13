module Crypto.Hash.MerklePatriciaForestry.Internal.Types.Proof (
  Proof (..),
  ProofStep (..),
) where

import Crypto.Hash.MerklePatriciaForestry.Internal.Types.HexDigit
import Crypto.Hash.MerklePatriciaForestry.Internal.Types.Value
import Data.ByteString (ByteString)
import GHC.Natural (Natural)

data ProofStep where
  ProofStepLeaf ::
    { pslPrefixLength :: Natural
    , pslNeighbourKeyPath :: [HexDigit]
    , pslNeighbourValueDigest :: ByteString
    } ->
    ProofStep
  ProofStepFork ::
    { psfPrefixLength :: Natural
    , psfNeighbourPrefix :: [HexDigit]
    , psfNeighbourIx :: HexDigit
    , psfNeighbourMerkleRoot :: ByteString
    } ->
    ProofStep
  ProofStepBranch ::
    { psbPrefixLength :: Natural
    , psbMerkleProof :: [ByteString]
    } ->
    ProofStep

data Proof = Proof
  { proofPath :: [HexDigit]
  , proofValue :: Value
  , proofSteps :: [ProofStep]
  }
