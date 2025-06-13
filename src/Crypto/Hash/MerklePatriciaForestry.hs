module Crypto.Hash.MerklePatriciaForestry (
  -- * Trie type
  MerklePatriciaForestry,

  -- * Key and value types
  Key (..),
  keyFromString,
  keyFromText,
  Value (..),
  valueFromString,
  valueFromText,

  -- * Construction
  empty,
  insert,

  -- * Query
  Crypto.Hash.MerklePatriciaForestry.Internal.null,
  size,
  rootHash,
  Crypto.Hash.MerklePatriciaForestry.Internal.lookup,
  member,

  -- * Deletion
  delete,

  -- * Proof
  Proof,
  generateProof,
  encodeProof,
) where

import Crypto.Hash.MerklePatriciaForestry.Internal