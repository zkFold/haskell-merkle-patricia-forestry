# Merkle Patricia Forestry

Haskell library to interact with [Merkle Patricia Forestry](https://github.com/aiken-lang/merkle-patricia-forestry?tab=readme-ov-file) data structure.

This library is meant to be imported qualified. Sample operation is below and see `Crypto.Hash.MerklePatriciaForestry` module for complete client side API.


```haskell
import Crypto.Hash.MerklePatriciaForestry qualified as MPF


-- Building Merkle Patricia Forestry.
mpf = MPF.empty 
     & MPF.insert (keyFromString "hail") (valueFromString "haskell")
     -- `Key`, `Value` have `IsString` instances.
     & MPF.insert "🙅‍♂️" "JS"
     -- Would update in case of duplicate key.
     & MPF.insert "hail" "🐼"
     -- Deletion for non-existent key, returns the original trie.
     & MPF.delete "does-not-exist"

-- Member lookup.
hailValue = MPF.lookup "hail" mpf

-- Generates proof for verifying "hail" to be a member of our trie.
hailProof = MPF.generateProof "hail" mpf

-- Proof's CBOR encoding, this can be serialized using @Codec.CBOR.Write.toLazyByteString@ from @cborg@ package.
hailProofCBOR = encodeProof proof

-- @ToJSON@ instance is provided for 'Proof' type.
hailProofJSON = toJSON proof
```

## Credits

* https://github.com/aiken-lang/merkle-patricia-forestry?tab=readme-ov-file.
* https://github.com/blinklabs-io/merkle-patricia-forestry.