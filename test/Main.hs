module Main (main) where

import Codec.CBOR.Write qualified as CBOR
import Crypto.Hash.MerklePatriciaForestry.Internal
import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 qualified as BS16
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import GHC.IsList (IsList (fromList))
import GHC.Natural (Natural)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (lookup)

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
                BS16.encode (merkleRoot mempty allHexDigits) @?= "209155a276ca3c2417e3876971dd587dd64ed9fcb8ef1fd6e7589ef4255c967f"
            , testCase "hash of leaf with custom suffix" $ do
                let key = "81"
                    -- This is to check for odd length leaf suffix case.
                    path = drop 3 (intoPath key)
                BS16.encode (leafHash (mkLeaf key (valueFromString "11") path)) @?= "11b7f8b67c436ab92110c52472f4207d8eb28a9ccaac78e2bf5042ff018116e9"
            , testCase "merkleRoot for single leaf child" $ do
                let key = "81"
                    val = "11"
                    leaf = mkLeaf key val (intoPath key)
                BS16.encode (merkleRoot (Map.fromList [(mkHexDigit 4 & fromJust, MerklePatriciaForestryNodeLeaf leaf)]) allHexDigits) @?= "910dffb24e642c330024cc419b49df7f108ae9994e8072176734b24a152ded74"
            , testCase "merkleRoot for two leaf children" $ do
                let key1 = "81"
                    val1 = "11"
                    key2 = "189"
                    val2 = "11"
                    leaf1 = mkLeaf key1 val1 (intoPath key1)
                    leaf2 = mkLeaf key2 val2 (intoPath key2)
                BS16.encode (merkleRoot (Map.fromList [(mkHexDigit 0 & fromJust, MerklePatriciaForestryNodeLeaf leaf1), (mkHexDigit 15 & fromJust, MerklePatriciaForestryNodeLeaf leaf2)]) allHexDigits) @?= "ed001e421fcc8a8e57ccb0317b8abdd1b19652ddb5cd8a60ca49cf447cf28a13"
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
            , testCase "Insertion tests, fruitList" $ do
                let mpf = fromList fruitsList
                BS16.encode (rootHash mpf) @?= "4acd78f345a686361df77541b2e0b533f53362e36620a1fdd3a13e0b61a3b078"
            , testGroup
                "Lookup Tests"
                [ testCase "Lookup for empty trie" $ do
                    lookup "81" MerklePatriciaForestryEmpty @?= Nothing
                , testCase "Lookup for trie with single leaf" $ do
                    let mpf =
                            MerklePatriciaForestryEmpty
                                & insert "81" "11"
                    lookup "189" mpf @?= Nothing
                    lookup "81" mpf @?= Just "11"
                ]
            ]
        , testGroup
            "Proof Tests"
            [ testCase "JSON & CBOR for proof of mango" $ do
                let mpf :: MerklePatriciaForestry = fromList fruitsList
                    proof = generateProof "mango[uid: 0]" mpf & fromJust
                Aeson.toJSON proof
                    @?= Aeson.Array
                        ( fromList
                            [ Aeson.object
                                [ "neighbors" Aeson..= Aeson.String "c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb45fdf82687b1ab133324cebaf46d99d49f92720c5ded08d5b02f57530f2cc5a5f1508f13471a031a21277db8817615e62a50a7427d5f8be572746aa5f0d49841758c5e4a29601399a5bd916e5f3b34c38e13253f4de2a3477114f1b2b8f9f2f4d"
                                , "skip" Aeson..= (0 :: Natural)
                                , "type" Aeson..= Aeson.String "branch"
                                ]
                            , Aeson.object
                                [ "neighbor"
                                    Aeson..= Aeson.object
                                        [ "key" Aeson..= Aeson.String "09d23032e6edc0522c00bc9b74edd3af226d1204a079640a367da94c84b69ecc"
                                        , "value" Aeson..= Aeson.String "c29c35ad67a5a55558084e634ab0d98f7dd1f60070b9ce2a53f9f305fd9d9795"
                                        ]
                                , "skip" Aeson..= (0 :: Natural)
                                , "type" Aeson..= Aeson.String "leaf"
                                ]
                            ]
                        )
                BS16.encode (CBOR.toStrictByteString (proofToTermEncoding proof)) @?= "9fd8799f005f5840c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb45fdf82687b1ab133324cebaf46d99d49f92720c5ded08d5b02f57530f2cc5a5f58401508f13471a031a21277db8817615e62a50a7427d5f8be572746aa5f0d49841758c5e4a29601399a5bd916e5f3b34c38e13253f4de2a3477114f1b2b8f9f2f4dffffd87b9f00582009d23032e6edc0522c00bc9b74edd3af226d1204a079640a367da94c84b69ecc5820c29c35ad67a5a55558084e634ab0d98f7dd1f60070b9ce2a53f9f305fd9d9795ffff"
            , testCase "JSON for proof of kumquat" $ do
                let mpf :: MerklePatriciaForestry = fromList fruitsList
                    proof = generateProof "kumquat[uid: 0]" mpf
                Aeson.toJSON proof
                    @?= Aeson.Array
                        ( fromList
                            [ Aeson.object
                                [ "neighbors" Aeson..= Aeson.String "c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb47238ba5d16031b6bace4aee22156f5028b0ca56dc24f7247d6435292e82c039c3490a825d2e8deddf8679ce2f95f7e3a59d9c3e1af4a49b410266d21c9344d6d08434fd717aea47d156185d589f44a59fc2e0158eab7ff035083a2a66cd3e15b"
                                , "skip" Aeson..= (0 :: Natural)
                                , "type" Aeson..= Aeson.String "branch"
                                ]
                            , Aeson.object
                                [ "neighbor"
                                    Aeson..= Aeson.object
                                        [ "nibble" Aeson..= (0 :: Natural)
                                        , "prefix" Aeson..= Aeson.String "07"
                                        , "root" Aeson..= Aeson.String "a1ffbc0e72342b41129e2d01d289809079b002e54b123860077d2d66added281"
                                        ]
                                , "skip" Aeson..= (0 :: Natural)
                                , "type" Aeson..= Aeson.String "fork"
                                ]
                            ]
                        )
            ]
        ]

fruitsList :: [(Key, Value)]
fruitsList =
    [ ("apple[uid: 58]", "🍎")
    , ("apricot[uid: 0]", "🤷")
    , ("banana[uid: 218]", "🍌")
    , ("blueberry[uid: 0]", "🫐")
    , ("cherry[uid: 0]", "🍒")
    , ("coconut[uid: 0]", "🥥")
    , ("cranberry[uid: 0]", "🤷")
    , ("fig[uid: 68267]", "🤷")
    , ("grapefruit[uid: 0]", "🤷")
    , ("grapes[uid: 0]", "🍇")
    , ("guava[uid: 344]", "🤷")
    , ("kiwi[uid: 0]", "🥝")
    , ("kumquat[uid: 0]", "🤷")
    , ("lemon[uid: 0]", "🍋")
    , ("lime[uid: 0]", "🤷")
    , ("mango[uid: 0]", "🥭")
    , ("orange[uid: 0]", "🍊")
    , ("papaya[uid: 0]", "🤷")
    , ("passionfruit[uid: 0]", "🤷")
    , ("peach[uid: 0]", "🍑")
    , ("pear[uid: 0]", "🍐")
    , ("pineapple[uid: 12577]", "🍍")
    , ("plum[uid: 15492]", "🤷")
    , ("pomegranate[uid: 0]", "🤷")
    , ("raspberry[uid: 0]", "🤷")
    , ("strawberry[uid: 2532]", "🍓")
    , ("tangerine[uid: 11]", "🍊")
    , ("tomato[uid: 83468]", "🍅")
    , ("watermelon[uid: 0]", "🍉")
    , ("yuzu[uid: 0]", "🤷")
    ]