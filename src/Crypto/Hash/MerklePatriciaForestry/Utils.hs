module Crypto.Hash.MerklePatriciaForestry.Utils (
  commonPrefix,
) where

import Data.Foldable (foldl')

{- | Compute the common prefix of a list of strings.

>>> commonPrefix ["hello", "hell", "heaven"]
"he"

>>> commonPrefix ["hello"]
"hello"

>>> commonPrefix []
[]

>>> commonPrefix ["hello", "bye"]
""

>>> commonPrefix ["hello", "bolio"]
""
-}
commonPrefix :: (Eq b) => [[b]] -> [b]
commonPrefix [] = []
commonPrefix [s] = s
commonPrefix (s : ss) = foldl' commonPrefix2 s ss
 where
  commonPrefix2 acc str = map fst $ takeWhile (uncurry (==)) $ zip acc str
