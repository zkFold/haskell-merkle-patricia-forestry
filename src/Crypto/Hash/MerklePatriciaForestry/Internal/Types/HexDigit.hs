module Crypto.Hash.MerklePatriciaForestry.Internal.Types.HexDigit (
  HexDigit,
  mkHexDigit,
  unHexDigit,
  hexDigits,
  byteStringToHexDigits,
  hexDigitsToByteString,
) where

import Control.Arrow ((>>>))
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as BS16
import Data.ByteString.Char8 qualified as BS8
import Data.Char (digitToInt, intToDigit)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Maybe (fromJust)
import GHC.Natural (Natural)

-- | A hex digit is a natural number between 0 and 15.
newtype HexDigit = HexDigit {unHexDigit :: Natural}
  deriving stock (Eq, Ord, Show)

mkHexDigit :: Natural -> Maybe HexDigit
mkHexDigit n
  | n < 16 = Just (HexDigit n)
  | otherwise = Nothing

-- | All hex digits.
hexDigits :: [HexDigit]
hexDigits = coerce [(0 :: Natural) .. 15]

{- | Convert a byte string to a list of hex digits.

>>> byteStringToHexDigits "foo"
[HexDigit {unHexDigit = 6},HexDigit {unHexDigit = 6},HexDigit {unHexDigit = 6},HexDigit {unHexDigit = 15},HexDigit {unHexDigit = 6},HexDigit {unHexDigit = 15}]
-}
byteStringToHexDigits :: BS.ByteString -> [HexDigit]
byteStringToHexDigits bs =
  BS16.encode bs
    & BS8.unpack
    & map (digitToInt >>> fromIntegral >>> mkHexDigit >>> fromJust)

{- | Convert a list of hex digits to a corresponding byte string.

__NOTE:__ Input length must be even.

>>> hexDigitsToByteString (byteStringToHexDigits "foo") == "foo"
True
-}
hexDigitsToByteString :: [HexDigit] -> BS.ByteString
hexDigitsToByteString hds =
  map (unHexDigit >>> fromIntegral >>> intToDigit) hds
    & BS8.pack
    & BS16.decode
    & either (\s -> error $ "hexDigitsToByteString: " <> s) id