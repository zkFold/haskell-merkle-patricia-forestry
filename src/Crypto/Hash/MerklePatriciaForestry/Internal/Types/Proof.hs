module Crypto.Hash.MerklePatriciaForestry.Internal.Types.Proof (
  Proof (..),
  ProofStep (..),
) where

import Control.Arrow ((>>>))
import Crypto.Hash.MerklePatriciaForestry.Internal.Types.HexDigit
import Crypto.Hash.MerklePatriciaForestry.Internal.Types.Value
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as BS16
import Data.Text (Text)
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

instance Aeson.ToJSON ProofStep where
  toJSON (ProofStepLeaf prefixLength neighbourKeyPath neighbourValueDigest) =
    Aeson.object
      [ "skip" Aeson..= prefixLength
      , "type" Aeson..= Aeson.String "leaf"
      , "neighbor"
          Aeson..= Aeson.object
            [ "key" Aeson..= Aeson.String (hexDigitsToText neighbourKeyPath)
            , "value" Aeson..= Aeson.String (byteStringToHex neighbourValueDigest)
            ]
      ]
  toJSON (ProofStepFork prefixLength neighbourPrefix neighbourIx neighbourMerkleRoot) =
    Aeson.object
      [ "skip" Aeson..= prefixLength
      , "type" Aeson..= Aeson.String "fork"
      , "neighbor"
          Aeson..= Aeson.object
            [ "nibble" Aeson..= unHexDigit neighbourIx
            , "prefix" Aeson..= Aeson.String (hexDigitsToText neighbourPrefix)
            , "root" Aeson..= Aeson.String (byteStringToHex neighbourMerkleRoot)
            ]
      ]
  toJSON (ProofStepBranch prefixLength merkleProof) =
    Aeson.object
      [ "skip" Aeson..= prefixLength
      , "type" Aeson..= Aeson.String "branch"
      , "neighbors" Aeson..= Aeson.String (mconcat (map byteStringToHex merkleProof))
      ]

instance Aeson.ToJSON Proof where
  toJSON (Proof _path _value steps) = Aeson.toJSON steps