module ZeldsparTest where

import Zeldspar

import Prelude hiding (head, take, drop, reverse, sum)

-- co-feldspar.
import Feldspar.Software as Soft hiding (loop)
import Feldspar.Hardware as Hard hiding (loop)

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

prog1 :: ZS (SExp Int32) (SExp Int32) ()
prog1 = loop $ do
    i <- take
    lift $ printf "prog1 received %d\n" i
    emit (i + 1)

prog2 :: ZS (SExp Int32) (SExp Int32) ()
prog2 = loop $ do
    i <- take
    lift $ printf "prog2 received %d\n" i
    emit (i * 2)

fused :: ZS (SExp Int32) (SExp Int32) ()
fused = prog1 >>> prog2

stored :: ZS (SExp Int32) (SExp Int32) ()
stored = prog1 >>> loop store >>> prog2

--------------------------------------------------------------------------------

prog3 :: ZS (SExp Int32) (SExp Int32) ()
prog3 = loop $ do
    i <- take
    emit (i + 1)
    emit (i + 2)

prog4 :: ZS (SExp Int32) (SExp Int32) ()
prog4 = loop $ do
    i <- take
    emit (i * 2)
    i <- take
    emit (i * 3)
    i <- take
    emit (i * 4)

infinite :: ZS (SExp Int32) (SExp Int32) ()
infinite = prog3 >>> prog4

infinite' :: ZS (SExp Int32) (SExp Int32) ()
infinite' = (emit 13 >> prog3) >>> prog4

--------------------------------------------------------------------------------

type SPull a = Pull Software (SExp a)

vecMake :: ZS (SExp Int32) (SPull Int32) ()
vecMake = loop $ do
    i <- take
    emit $ fmap (i2n) (0 ... i2n i)

vecInc :: ZS (SPull Int32) (SPull Int32) ()
vecInc = loop $ do
    v <- take
    emit (fmap (+1) v)

vecRev :: ZS (SPull Int32) (SPull Int32) ()
vecRev = loop $ do
    v <- take
    emit (reverse v)

vecTail :: ZS (SPull Int32) (SPull Int32) ()
vecTail = loop $ do
    v <- take
    lift $ printf "Dropping: %d\n" (head v)
    emit (drop 1 v)

vecSum :: ZS (SPull Int32) (SExp Int32) ()
vecSum = loop $ do
    v <- take
    emit (sum v)

--fusedVec  = vecMake >>> vecInc >>> vecRev >>> vecTail >>> vecSum
--storedVec = vecMake >>> vecInc >>> loop store >>> vecRev >>> vecTail >>> vecSum

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

prepare :: ZS (SExp Int32) (SExp Int32) () -> Software ()
prepare p = runZ p src snk
  where
    src = fget stdin
    snk = printf "%d\n"

scompile :: ZS (SExp Int32) (SExp Int32) () -> IO ()
scompile = Soft.icompile . prepare


--------------------------------------------------------------------------------
