module Main (main) where

import Crypto.Hash.MerklePatriciaForestry.Internal
import Data.ByteString.Base16 qualified as BS16
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Merkle Patricia Forestry Tests"
        [ testGroup
            "Root Hash Tests"
            [ testCase "Empty trie has null hash" $
                rootHash MerklePatriciaForestryEmpty @?= nullHash
            , testCase "merkleRoot for empty children" $
                BS16.encode (merkleRoot mempty) @?= "209155a276ca3c2417e3876971dd587dd64ed9fcb8ef1fd6e7589ef4255c967f"
            , testCase "hash of leaf with custom suffix" $ do
                let key = "81"
                    -- This is to check for odd length leaf suffix case.
                    path = drop 3 (intoPath key)
                BS16.encode (leafHash (mkLeaf key (valueFromString "11") path)) @?= "11b7f8b67c436ab92110c52472f4207d8eb28a9ccaac78e2bf5042ff018116e9"
            , testCase "merkleRoot for single leaf child" $ do
                let key = "81"
                    val = "11"
                    leaf = mkLeaf key val (intoPath key)
                BS16.encode (merkleRoot (Map.fromList [(mkHexDigit 4 & fromJust, MerklePatriciaForestryNodeLeaf leaf)])) @?= "910dffb24e642c330024cc419b49df7f108ae9994e8072176734b24a152ded74"
            , testCase "merkleRoot for two leaf children" $ do
                let key1 = "81"
                    val1 = "11"
                    key2 = "189"
                    val2 = "11"
                    leaf1 = mkLeaf key1 val1 (intoPath key1)
                    leaf2 = mkLeaf key2 val2 (intoPath key2)
                BS16.encode (merkleRoot (Map.fromList [(mkHexDigit 0 & fromJust, MerklePatriciaForestryNodeLeaf leaf1), (mkHexDigit 15 & fromJust, MerklePatriciaForestryNodeLeaf leaf2)])) @?= "ed001e421fcc8a8e57ccb0317b8abdd1b19652ddb5cd8a60ca49cf447cf28a13"
            , testCase "branch hash for empty branch" $ do
                BS16.encode (branchHash emptyBranch) @?= "d068658a71d0f9019f2a35d4859fab5dbafa098535665d4464b50796f938fc82"
            , testCase "branch hash for empty branch but with some prefix" $ do
                BS16.encode (branchHash $ branchUpdateHash $ emptyBranch{branchPrefix = intoPath (keyFromString "81")}) @?= "f3409b1bfcdde906367569aae80897c6260b7fc9be40afaa018473e917eca338"
            , testCase "Insertion tests, 1" $ do
                let mpf =
                        MerklePatriciaForestryEmpty
                            & insert (keyFromString "81") (valueFromString "11")
                BS16.encode (rootHash mpf) @?= "d818270b8f337e0c13949453555e5a8343507d83130c96625f56b4941884e5af"
            , testCase "Insertion tests, 2" $ do
                let mpf =
                        MerklePatriciaForestryEmpty
                            & insert (keyFromString "81") (valueFromString "11")
                            & insert (keyFromString "189") (valueFromString "11")
                BS16.encode (rootHash mpf) @?= "01b252f957e3138467c540ba230723c16b32d2bfe7f33dd54e8a7ab5d7ca02e9"
            ]
        ]
