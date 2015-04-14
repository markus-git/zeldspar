{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module defines a simple expression type for use in Zeldspar, and a function for running
-- Zeldspar programs in the 'IO' monad.
module RunIO where



import Control.Monad.State
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map

import Zeldspar
import Frontend



type Store = Map VarId Dynamic
type Run   = StateT Store IO

assign :: Typeable a => Ref a -> a -> Run ()
assign (Ref v) = modify . Map.insert v . toDyn

runIO :: forall exp inp out a . EvalExp exp Run =>
    Prog exp inp out a -> IO inp -> (out -> IO ()) -> IO a
runIO prog get put = do
    let (a,p) = runProg prog
    flip evalStateT Map.empty $ go p
    return a
  where
    go :: Program exp inp out -> Run ()
    go (Emit a    :> p) = (eval a >>= liftIO . put) >> go p
    go (Receive r :> p) = (liftIO get >>= assign r) >> go p
    go (r := a    :> p) = (eval a >>= assign r) >> go p
    go (Loop p)         = go p >> go (Loop p)
    go Return           = return ()
    -- EndL should not appear here

