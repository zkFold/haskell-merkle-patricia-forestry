module Crypto.Hash.MerklePatriciaForestry.Internal.Types.Proof (
  Proof (..),
  ProofStep (..),
  proofToAiken,
) where

import Control.Arrow ((>>>))
import Crypto.Hash.MerklePatriciaForestry.Internal.Types.HexDigit
import Crypto.Hash.MerklePatriciaForestry.Internal.Types.Value
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as BS16
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
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

byteStringToHex :: ByteString -> Text
byteStringToHex = BS16.encode >>> Text.decodeUtf8

-- | Serialize the proof as Aiken code. Mainly for debugging / testing.
proofToAiken :: Proof -> String
proofToAiken pf =
  let stepsAsString = map stepToString (proofSteps pf)
   in "[\n"
        <> indentLines 4 (mconcat stepsAsString)
        <> "]"
 where
  indentLines n s = unlines $ map (replicate n ' ' ++) (lines s)
  stepToString :: ProofStep -> String
  stepToString (ProofStepLeaf prefixLength neighbourKeyPath neighbourValueDigest) =
    "Leaf {\n"
      <> "  skip: "
      <> show prefixLength
      <> ",\n"
      <> "  key: #\""
      <> Text.unpack (hexDigitsToText neighbourKeyPath)
      <> "\",\n"
      <> "  value: #\""
      <> Text.unpack (byteStringToHex neighbourValueDigest)
      <> "\"\n"
      <> "},\n"
  stepToString (ProofStepFork prefixLength neighbourPrefix neighbourIx neighbourMerkleRoot) =
    let neighbour =
          "Neighbor {\n"
            <> "  nibble: "
            <> show (unHexDigit neighbourIx)
            <> ",\n"
            <> "  prefix: #\""
            <> Text.unpack (hexDigitsToText neighbourPrefix)
            <> "\",\n"
            <> "  root: #\""
            <> Text.unpack (byteStringToHex neighbourMerkleRoot)
            <> "\"\n"
            <> "}\n"
     in "Fork {\n"
          <> "  skip: "
          <> show prefixLength
          <> ",\n"
          <> "  neighbor: \n"
          <> indentLines 4 neighbour
          <> "},\n"
  stepToString (ProofStepBranch prefixLength merkleProof) =
    "Branch {\n"
      <> "  skip: "
      <> show prefixLength
      <> ",\n"
      <> "  neighbors: #\""
      <> Text.unpack (mconcat $ map byteStringToHex merkleProof)
      <> "\"\n"
      <> "},\n"