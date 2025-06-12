# Merkle Patricia Forestry

Haskell library to interact with [Merkle Patricia Forestry](https://github.com/aiken-lang/merkle-patricia-forestry?tab=readme-ov-file) data structure.

This library is meant to be imported qualified. Sample operation is below and see `Crypto.Hash.MerklePatriciaForestry` module for complete client side API.


```haskell
import Crypto.Hash.MerklePatriciaForestry qualified as MPF


-- Inserting elements
mpf = MPF.empty 
     & MPF.insert (keyFromString "hail") (valueFromString "haskell")
     -- `Key`, `Value` have `IsString` instances.
     & MPF.insert "🚫" "JS!"
     -- Would update in case of duplicate key.
     & MPF.insert "hail" "🐼"
     -- Deletion for non-existent key, returns the original trie.
     & MPF.delete "does-not-exist"
```
<!-- TODO: Write more examples -->

## Credits

* https://github.com/aiken-lang/merkle-patricia-forestry?tab=readme-ov-file.
* https://github.com/blinklabs-io/merkle-patricia-forestry.