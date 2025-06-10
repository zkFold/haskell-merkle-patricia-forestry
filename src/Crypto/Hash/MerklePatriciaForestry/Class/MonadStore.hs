module Crypto.Hash.MerklePatriciaForestry.Class.MonadStore () where
import Data.ByteString (ByteString)
import GHC.Natural (Natural)
  
class (Monad m) => MonadStore m where
  put :: ByteString -> ByteString -> m ()
  del :: ByteString -> m ()
  size :: m Natural
  get :: ByteString -> m (Maybe ByteString)