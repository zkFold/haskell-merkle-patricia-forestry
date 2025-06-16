module Crypto.Hash.MerklePatriciaForestry.Internal.Types.Proof (
  Proof (..),
  ProofStep (..),
  encodeProof,
  toAiken,
) where

import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Term qualified as CBOR
import Control.Arrow ((>>>))
import Crypto.Hash.MerklePatriciaForestry.Internal.Types.HexDigit
import Crypto.Hash.MerklePatriciaForestry.Internal.Types.Value
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
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

proofStepToTermEncoding :: ProofStep -> CBOR.Encoding
proofStepToTermEncoding (ProofStepLeaf prefixLength neighbourKeyPath neighbourValueDigest) =
  CBOR.encodeTerm $
    CBOR.TTagged 123 $
      CBOR.TListI $
        [ CBOR.TInt (fromIntegral prefixLength)
        , CBOR.TBytes (hexDigitsToByteStringSupportsOdd neighbourKeyPath)
        , CBOR.TBytes neighbourValueDigest
        ]
proofStepToTermEncoding (ProofStepFork prefixLength neighbourPrefix neighbourIx neighbourMerkleRoot) =
  CBOR.encodeTerm $
    CBOR.TTagged 122 $
      CBOR.TListI $
        [ CBOR.TInt (fromIntegral prefixLength)
        , CBOR.TTagged 121 $
            CBOR.TListI
              [ CBOR.TInt (fromIntegral (unHexDigit neighbourIx))
              , CBOR.TBytes (hexDigitsToByteStringSupportsOdd neighbourPrefix)
              , CBOR.TBytes neighbourMerkleRoot
              ]
        ]
proofStepToTermEncoding (ProofStepBranch prefixLength merkleProof) =
  let (bsa, bsb) = BS.splitAt 64 $ mconcat merkleProof
   in CBOR.encodeTag64 121
        <> ( CBOR.encodeListLenIndef
              <> ( CBOR.encodeInt (fromIntegral prefixLength)
                    <> ( CBOR.encodeBytesIndef
                          <> ( CBOR.encodeBytes bsa
                                <> CBOR.encodeBytes bsb
                             )
                          <> CBOR.encodeBreak
                       )
                 )
              <> CBOR.encodeBreak
           )

encodeProof :: Proof -> CBOR.Encoding
encodeProof pf = CBOR.encodeListLenIndef <> mconcat (map proofStepToTermEncoding (proofSteps pf)) <> CBOR.encodeBreak

-- | Serialize the proof as Aiken code. Mainly for debugging / testing.
toAiken :: Proof -> String
toAiken pf =
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