module Crypto.Hash.MerklePatriciaForestry (
  module Crypto.Hash.MerklePatriciaForestry.Types,
  MerklePatriciaForestry (..),
  nullHash,
  keyFromString,
  valueFromString,
  Crypto.Hash.MerklePatriciaForestry.null,
  size,
  rootHash,
  insert,
  delete,
) where

import Crypto.Hash.MerklePatriciaForestry.Types

import Control.Arrow ((>>>))
import Crypto.Hash.BLAKE2.BLAKE2b (hash)
import Crypto.Hash.MerklePatriciaForestry.Utils (commonPrefix)
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
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

nodeHash :: MerklePatriciaForestryNode -> ByteString
nodeHash (MerklePatriciaForestryNodeLeaf leaf) = leafHash leaf
nodeHash (MerklePatriciaForestryNodeBranch branch) = branchHash branch

data MerklePatriciaForestryNode
  = MerklePatriciaForestryNodeLeaf Leaf
  | MerklePatriciaForestryNodeBranch Branch

-- | Insert a new key and value in the trie. If the key is already present in the trie, the associated value is replaced with the supplied value.
insert :: Key -> Value -> MerklePatriciaForestry -> MerklePatriciaForestry
insert key val mpf = case mpf of
  MerklePatriciaForestryEmpty ->
    MerklePatriciaForestryNode 1 (MerklePatriciaForestryNodeLeaf $ mkLeaf key val keyPath)
  MerklePatriciaForestryNode trieSize node ->
    case node of
      MerklePatriciaForestryNodeLeaf leaf ->
        -- We update the value.
        if keyPath == leafSuffix leaf
          then
            MerklePatriciaForestryNode trieSize (MerklePatriciaForestryNodeLeaf $ updateLeaf leaf val)
          else
            emptyBranch{branchPrefix = commonPrefix [keyPath, leafSuffix leaf]}
              & branchInsert (leafKey leaf) (leafValue leaf) (leafSuffix leaf)
              & branchInsert key val keyPath
              & MerklePatriciaForestryNodeBranch
              & MerklePatriciaForestryNode (trieSize + 1)
      MerklePatriciaForestryNodeBranch branch ->
        let (newBranch, newElem) = insertIntoBranch keyPath branch key val
         in MerklePatriciaForestryNode (if newElem then trieSize + 1 else trieSize) newBranch
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

updateLeaf :: Leaf -> Value -> Leaf
updateLeaf Leaf{..} newVal =
  mkLeaf leafKey newVal leafSuffix

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

branchInsert :: Key -> Value -> [HexDigit] -> Branch -> Branch
branchInsert key val path branch = fst $ branchInsert' key val path branch

branchInsert' :: Key -> Value -> [HexDigit] -> Branch -> (Branch, Bool)
branchInsert' key val path branch =
  let pathMinusPrefix = drop (length (branchPrefix branch)) path
      childIx = head pathMinusPrefix -- Since all keys are of same length, prefix stored (if any) is always less than or ... (TODO: complete this sentence)
      subPath = tail pathMinusPrefix
   in if Map.notMember childIx (branchChildren branch)
        then
          let newLeaf = mkLeaf key val subPath
           in (updateBranchChild branch childIx (MerklePatriciaForestryNodeLeaf newLeaf), True)
        else
          let existingChild = branchChildren branch Map.! childIx
           in case existingChild of
                MerklePatriciaForestryNodeLeaf leaf ->
                  let cmnPrefix = commonPrefix [subPath, leafSuffix leaf]
                   in if cmnPrefix == leafSuffix leaf
                        then
                          -- Update the value of existing key.
                          let newLeaf = updateLeaf leaf val
                           in (updateBranchChild branch childIx (MerklePatriciaForestryNodeLeaf newLeaf), False)
                        else
                          let newBranch =
                                -- Create a new branch with common prefix.
                                emptyBranch{branchPrefix = cmnPrefix}
                                  -- Add original leaf.
                                  & branchInsert (leafKey leaf) (leafValue leaf) (leafSuffix leaf)
                                  -- Add new element.
                                  & branchInsert key val subPath
                           in (updateBranchChild branch childIx (MerklePatriciaForestryNodeBranch newBranch), True)
                MerklePatriciaForestryNodeBranch childBranch ->
                  first (updateBranchChild branch childIx) $ insertIntoBranch subPath childBranch key val

insertIntoBranch path branch key val =
  let cmnPrefix = commonPrefix [path, branchPrefix branch]
   in if cmnPrefix == branchPrefix branch
        then
          -- Insert new value in existing branch.
          let (newBranch, newElem) = branchInsert' key val path branch
           in (MerklePatriciaForestryNodeBranch newBranch, newElem)
        else
          -- Create a new branch node with common prefix.
          let newBranch = emptyBranch{branchPrefix = cmnPrefix}
              newOrigBranchPrefix = drop (length cmnPrefix) (branchPrefix branch)
              newChildBranch = branchUpdateHash $ branch{branchPrefix = drop 1 newOrigBranchPrefix}
              (newBranchFinal, newElem) = updateBranchChild newBranch (head newOrigBranchPrefix) (MerklePatriciaForestryNodeBranch newChildBranch) & branchInsert' key val path
           in (MerklePatriciaForestryNodeBranch newBranchFinal, newElem)

updateBranchChild :: Branch -> HexDigit -> MerklePatriciaForestryNode -> Branch
updateBranchChild branch childIx newChild =
  branchUpdateHash $
    branch
      { branchChildren = Map.insert childIx newChild (branchChildren branch)
      }

deleteBranchChild :: Branch -> HexDigit -> Branch
deleteBranchChild branch childIx =
  branchUpdateHash $
    branch
      { branchChildren = Map.delete childIx (branchChildren branch)
      }

merkleRoot :: Map HexDigit MerklePatriciaForestryNode -> ByteString
merkleRoot childrens =
  map (\hd -> maybe nullHash nodeHash (Map.lookup hd childrens)) hexDigits
    & go
 where
  go :: [ByteString] -> ByteString
  go [] = error "merkleRoot: absurd"
  go [merkleRootHash] = merkleRootHash
  go hashes = go (hashLevel hashes)
  hashLevel [] = error "merkleRoot.hashLevel: absurd, no elements"
  hashLevel [_] = error "merkleRoot.hashLevel: absurd, odd number of elements"
  hashLevel [a, b] = [digest (a <> b)]
  hashLevel (a : b : rest) = digest (a <> b) : hashLevel rest

branchUpdateHash :: Branch -> Branch
branchUpdateHash branch =
  branch
    { branchHash =
        digest
          ( (map (unHexDigit >>> fromIntegral >>> intToDigit) (branchPrefix branch) & BS8.pack)
              <> merkleRoot (branchChildren branch)
          )
    }

-- | Delete a key (and it's value) from the trie. If the key is not a member of the trie, the original trie is returned.
delete :: Key -> MerklePatriciaForestry -> MerklePatriciaForestry
delete _key MerklePatriciaForestryEmpty = MerklePatriciaForestryEmpty
delete key (MerklePatriciaForestryNode trieSize node) =
  case node of
    MerklePatriciaForestryNodeLeaf leaf ->
      if keyPath == leafSuffix leaf
        then MerklePatriciaForestryEmpty
        else MerklePatriciaForestryNode trieSize node
    MerklePatriciaForestryNodeBranch branch ->
      let (newNode, elemFound) = branchDelete keyPath branch
       in MerklePatriciaForestryNode (if elemFound then trieSize - 1 else trieSize) newNode
 where
  keyPath = intoPath key

branchDelete :: [HexDigit] -> Branch -> (MerklePatriciaForestryNode, Bool)
branchDelete keyPath branch =
  let pathMinusPrefix = drop (length (branchPrefix branch)) keyPath
      childIx = head pathMinusPrefix
      subPath = tail pathMinusPrefix
   in if Map.notMember childIx (branchChildren branch)
        then (MerklePatriciaForestryNodeBranch branch, False)
        else
          let existingChild = branchChildren branch Map.! childIx
           in case existingChild of
                MerklePatriciaForestryNodeLeaf leaf ->
                  if leafSuffix leaf /= subPath
                    then (MerklePatriciaForestryNodeBranch branch, False)
                    else
                      let newBranch = deleteBranchChild branch childIx
                       in ( -- It never makes sense for branch to have only one child. If this has occurred due to deletion, then we need to move the single child to parent.
                            if Map.size (branchChildren newBranch) == 1
                              then
                                let (newBranchChildIx, newBranchChild) = Map.findMin (branchChildren newBranch)
                                 in ( case newBranchChild of
                                        MerklePatriciaForestryNodeLeaf newBranchChildLeaf ->
                                          let newSuffix = branchPrefix newBranch <> [newBranchChildIx] <> leafSuffix newBranchChildLeaf
                                           in MerklePatriciaForestryNodeLeaf $ mkLeaf (leafKey newBranchChildLeaf) (leafValue newBranchChildLeaf) newSuffix
                                        MerklePatriciaForestryNodeBranch newBranchChildBranch ->
                                          let newPrefix = branchPrefix newBranch <> [newBranchChildIx] <> branchPrefix newBranchChildBranch
                                           in MerklePatriciaForestryNodeBranch $ branchUpdateHash $ newBranchChildBranch{branchPrefix = newPrefix}
                                    )
                              else MerklePatriciaForestryNodeBranch newBranch
                          , True
                          )
                MerklePatriciaForestryNodeBranch childBranch ->
                  let
                    (newChild, elemFound) = branchDelete subPath childBranch
                   in
                    (MerklePatriciaForestryNodeBranch $ updateBranchChild branch childIx newChild, elemFound)

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

{-
TODO:

1. Make `Key`, `Value` newtypes and move them to their own modules.
2. Move leaf, branch to it's own modules. Don't expose set fields of branch rather have safe setters that also update the hash.
3. notMember should be replaced with lookup, since we want element in case it exists.
4. Get rid of containers, vector (but maybe blake2 already depends on it!).
5. Write asymptotics for all functions.
6. Add haddock and useful comments to all functions.
7. Tests.
8. Corrected export list from main module with headings.
9. Make branch insert itself handle common prefix rather requiring caller to do it.
-}