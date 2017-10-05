-- | An implementation of Ziria that uses Feldspar to represent computations.
module Zeldspar
  ( module Feld
  , module Z
  , ZS
  , ZH
  , lift
  , runZ
  , precompute
  , store
  ) where

import Ziria as Z

import Control.Monad.Trans (lift)
import Prelude hiding (take)

-- co-feldspar.
import Feldspar as Feld hiding (loop)
import Feldspar.Storable
import Feldspar.Array.Vector as Feld hiding (take)
import Feldspar.Software (Software)
import Feldspar.Hardware (Hardware)

--------------------------------------------------------------------------------
-- * Representation and translation.
--------------------------------------------------------------------------------

-- | Software based 'Z' streams.
type ZS inp out = Z inp out Software

-- | Hardware based 'Z' streams.
type ZH inp out = Z inp out Hardware

-- | Translate a 'Z' stream into a computation program.
runZ
  :: forall inp out m a . MonadComp m
  => Z inp out m a  -- ^ Ziria stream.
  -> (m inp)        -- ^ Source.
  -> (out -> m ())  -- ^ Sink.
  -> m a
runZ (Z p) src snk = trans (p Return)
  where
    trans :: forall a. Action inp out m a -> m a
    trans (Lift m p)  = m >>= \a -> trans (p a)
    trans (Emit x p)  = snk x >> trans p
    trans (Take p)    = src >>= trans . p
    trans (Return x)  = return x
    trans (Loop s0 p) =
      do st <- initStore s0
         Feld.while (return true) $
           do s  <- readStore st
              s' <- trans (p s)
              writeStore st s'
         return (error "unreachable")

    true = undefined

--------------------------------------------------------------------------------
-- ** Utilities
--------------------------------------------------------------------------------

precompute :: (MonadComp m, Storable m a) => a -> m a
precompute x = do
  s <- initStore x
  unsafeFreezeStore s

store :: (MonadComp m, Storable m a) => Z a a m ()
store = do
  i <- take
  o <- lift $ precompute i
  emit o

--------------------------------------------------------------------------------
