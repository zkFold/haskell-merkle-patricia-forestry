module Crypto.Hash.MerklePatriciaForestry.Internal (
  module Crypto.Hash.MerklePatriciaForestry.Internal.Types,
  MerklePatriciaForestry (..),
  MerklePatriciaForestryNode (..),
  empty,
  Crypto.Hash.MerklePatriciaForestry.Internal.null,
  size,
  rootHash,
  insert,
  delete,
  Crypto.Hash.MerklePatriciaForestry.Internal.lookup,
  Branch (..),
  emptyBranch,
  branchUpdateHash,
  intoPath,
  nullHash,
  merkleRoot,
) where

import Control.Arrow ((>>>))
import Crypto.Hash.MerklePatriciaForestry.Internal.Hash
import Crypto.Hash.MerklePatriciaForestry.Internal.Types
import Crypto.Hash.MerklePatriciaForestry.Internal.Utils
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Natural (Natural)

data MerklePatriciaForestry
  = MerklePatriciaForestryEmpty
  | MerklePatriciaForestryNode Natural MerklePatriciaForestryNode
  deriving stock (Show)

data MerklePatriciaForestryNode
  = MerklePatriciaForestryNodeLeaf Leaf
  | MerklePatriciaForestryNodeBranch Branch
  deriving stock (Show)

-- | \(O(1)\). The empty trie.
empty :: MerklePatriciaForestry
empty = MerklePatriciaForestryEmpty

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
              & branchInsertInternal (leafKey leaf) (leafValue leaf) (leafSuffix leaf)
              & branchInsertInternal key val keyPath
              & MerklePatriciaForestryNodeBranch
              & MerklePatriciaForestryNode (trieSize + 1)
      MerklePatriciaForestryNodeBranch branch ->
        let (newBranch, newElem) = branchInsert key val keyPath branch
         in MerklePatriciaForestryNode (if newElem then trieSize + 1 else trieSize) newBranch
 where
  keyPath = intoPath key

data Branch = Branch
  { branchHash :: ByteString
  , branchPrefix :: [HexDigit]
  , -- TODO: Shall we use a sparse vector here? But then we'll need to somehow also track the size of children.
    branchChildren :: Map HexDigit MerklePatriciaForestryNode
  }
  deriving stock (Show)

emptyBranch :: Branch
emptyBranch =
  Branch
    { branchHash = nullHash
    , branchPrefix = []
    , branchChildren = mempty
    }
    & branchUpdateHash

branchInsertInternal :: Key -> Value -> [HexDigit] -> Branch -> Branch
branchInsertInternal key val path branch = fst $ branchInsertInternal' key val path branch

branchInsertInternal' :: Key -> Value -> [HexDigit] -> Branch -> (Branch, Bool)
branchInsertInternal' key val path branch =
  -- Here it is assumed that `branchPrefix` is the prefix of `path`. We maintain this invariant.
  let pathMinusPrefix = drop (length (branchPrefix branch)) path
      childIx = head pathMinusPrefix
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
                                  & branchInsertInternal (leafKey leaf) (leafValue leaf) (leafSuffix leaf)
                                  -- Add new element.
                                  & branchInsertInternal key val subPath
                           in (updateBranchChild branch childIx (MerklePatriciaForestryNodeBranch newBranch), True)
                MerklePatriciaForestryNodeBranch childBranch ->
                  first (updateBranchChild branch childIx) $ branchInsert key val subPath childBranch

branchInsert :: Key -> Value -> [HexDigit] -> Branch -> (MerklePatriciaForestryNode, Bool)
branchInsert key val path branch =
  let cmnPrefix = commonPrefix [path, branchPrefix branch]
   in if cmnPrefix == branchPrefix branch
        then
          -- Insert new value in existing branch.
          let (newBranch, newElem) = branchInsertInternal' key val path branch
           in (MerklePatriciaForestryNodeBranch newBranch, newElem)
        else
          -- If we are in this case, then `branchPrefix` is certainly not mempty and `cmnPrefix` is of length strictly less than `branchPrefix` and thus `head newOrigBranchPrefix` is well defined.
          -- Create a new branch node with common prefix.
          let newBranch = emptyBranch{branchPrefix = cmnPrefix}
              newOrigBranchPrefix = drop (length cmnPrefix) (branchPrefix branch)
              newChildBranch = branchUpdateHash $ branch{branchPrefix = drop 1 newOrigBranchPrefix}
              (newBranchFinal, newElem) = updateBranchChild newBranch (head newOrigBranchPrefix) (MerklePatriciaForestryNodeBranch newChildBranch) & branchInsertInternal' key val path
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
          ( (map (unHexDigit >>> fromIntegral) (branchPrefix branch) & BS.pack)
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
intoPath :: Key -> [HexDigit]
intoPath = unKey >>> digest >>> byteStringToHexDigits

lookup :: Key -> MerklePatriciaForestry -> Maybe Value
lookup _key MerklePatriciaForestryEmpty = Nothing
lookup key (MerklePatriciaForestryNode _ node) =
  case node of
    MerklePatriciaForestryNodeLeaf leaf ->
      if keyPath == leafSuffix leaf
        then Just (leafValue leaf)
        else Nothing
    MerklePatriciaForestryNodeBranch branch -> branchLookup keyPath branch
 where
  keyPath = intoPath key

branchLookup :: [HexDigit] -> Branch -> Maybe Value
branchLookup keyPath branch =
  let cmnPrefix = commonPrefix [keyPath, branchPrefix branch]
   in if cmnPrefix == branchPrefix branch
        then
          let pathMinusPrefix = drop (length (branchPrefix branch)) keyPath
              childIx = head pathMinusPrefix
              subPath = tail pathMinusPrefix
           in if Map.notMember childIx (branchChildren branch)
                then Nothing
                else
                  let existingChild = branchChildren branch Map.! childIx
                   in case existingChild of
                        MerklePatriciaForestryNodeLeaf leaf ->
                          if leafSuffix leaf /= subPath
                            then Nothing
                            else Just (leafValue leaf)
                        MerklePatriciaForestryNodeBranch childBranch -> branchLookup subPath childBranch
        else Nothing

{-
TODO:
1. IsList instances for MerklePatriciaForestry.
2. Move leaf, branch to it's own modules. Don't expose set fields of branch rather have safe setters that also update the hash.
3. notMember should be replaced with lookup, since we want element in case it exists.
4. Get rid of containers, vector (but maybe blake2 already depends on it!).
5. Write asymptotics for all functions.
6. Add haddock and useful comments to all functions.
7. Tests.
8. Corrected export list from main module with headings.
9. Make branch insert itself handle common prefix rather requiring caller to do it.
10. `merkleRoot` etc. should be exported from internal module.
11. review files, likely delete for monadstore...
12. Pretty printing, prolly get rid of stock show deriving.
13. clean tests.
-}