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
""

>>> commonPrefix ["hello", "bye"]
""
-}
commonPrefix :: [String] -> String
commonPrefix [] = ""
commonPrefix (s : []) = s
commonPrefix (s : ss) = foldl' (\acc str -> [c | (c, c') <- zip acc str, c == c']) s ss
