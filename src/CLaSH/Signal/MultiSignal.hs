{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module CLaSH.Signal.MultiSignal (
    MultiSignal (..) ,
    -- * Prependable
    Prependable(..) ,
    -- * Utility functions
    SignalLike(..),
    mealyP ,
    mooreP ,
    windowP,
    -- * Simulation functions (not synthesisable)
    fromListP,
    fromListPI,
    simulateP
) where

import qualified Data.Foldable as F
import qualified Prelude as P
import CLaSH.Prelude as CP
import CLaSH.Signal.Explicit as SE
import Test.QuickCheck (Arbitrary(..),CoArbitrary(..))
import Control.DeepSeq


-- Stream of elements where n elements arrives at same clock
data MultiSignal n a = MultiSignal {unMultiSignal :: Signal (Vec n a) } 


instance Functor (MultiSignal n) where
    fmap f s = MultiSignal ( fmap ( fmap f ) ( unMultiSignal s ) )


instance KnownNat n => Applicative (MultiSignal n) where
   pure = MultiSignal . pure . pure 
   f <*> s = MultiSignal  (fmap (<*>) fu <*> su) where
      fu = unMultiSignal f
      su = unMultiSignal s


instance (KnownNat n) =>  Foldable (MultiSignal n) where
    foldr f x xs = (F.foldr (.) id xs') x where 
        xs' = (\ys y -> CP.foldr f y ys) <$> unMultiSignal xs      


instance (KnownNat m, m ~ (n+1)) => Traversable (MultiSignal m) where
    traverse f xs = MultiSignal <$> sequenceA (fmap (traverse f) (unMultiSignal xs))


instance (Num a,KnownNat n) => Num (MultiSignal n a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*) 
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger
    negate = fmap negate


instance Eq (MultiSignal n a) where
   (==) = error "(==)' undefined for 'MultiSignal', use '(.==.)' instead"
   (/=) = error "(/=)' undefined for 'MultiSignal', use '(./=.)' instead"


instance  (Bounded a,KnownNat n)  =>  Bounded (MultiSignal n a) where
    minBound = pure minBound 
    maxBound = pure maxBound


instance (Fractional a,KnownNat n) => Fractional (MultiSignal n a) where
  (/)          = liftA2 (/)
  recip        = fmap recip
  fromRational = pure . fromRational


instance (Show a,KnownNat n) => Show (MultiSignal n a) where
  show = show . unMultiSignal


instance (Arbitrary a,KnownNat n) => Arbitrary (MultiSignal n a) where
  arbitrary  = MultiSignal <$> arbitrary


instance (CoArbitrary a) => CoArbitrary (MultiSignal n a) where
  coarbitrary  = coarbitrary . unMultiSignal 


instance (FiniteBits a,KnownNat n) => FiniteBits (MultiSignal n a) where
  finiteBitSize _ = finiteBitSize (undefined :: a)


instance (Bits a,KnownNat n) => Bits (MultiSignal n a) where
  (.&.)            = liftA2 (.&.)
  (.|.)            = liftA2 (.|.)
  xor              = liftA2 xor
  complement       = fmap complement
  shift a i        = fmap (`shift` i) a
  rotate a i       = fmap (`rotate` i) a
  zeroBits         = pure zeroBits
  bit              = pure . bit
  setBit a i       = fmap (`setBit` i) a
  clearBit a i     = fmap (`clearBit` i) a
  testBit          = error "'testBit' undefined for 'Signal'', use 'testbit1'"
  bitSizeMaybe _   = bitSizeMaybe (undefined :: a)
  bitSize _        = maybe 0 id (bitSizeMaybe (undefined :: a))
  isSigned _       = isSigned (undefined :: a)
  shiftL a i       = fmap (`shiftL` i) a
  unsafeShiftL a i = fmap (`unsafeShiftL` i) a
  shiftR a i       = fmap (`shiftR` i) a
  unsafeShiftR a i = fmap (`unsafeShiftR` i) a
  rotateL a i      = fmap (`rotateL` i) a
  rotateR a i      = fmap (`rotateR` i) a
  popCount         = error "'popCount' undefined for 'Signal'', use 'popCount1'"


instance (Default a,KnownNat n, n ~ (m+1)) => Default (MultiSignal n a) where
  def = pure def


instance (SaturatingNum a,KnownNat n, n ~ (m+1)) => SaturatingNum (MultiSignal n a) where
  satPlus = liftA2 . satPlus
  satMin = liftA2 . satMin
  satMult = liftA2 . satMult


instance (ExtendingNum a b,KnownNat n, n ~ (m+1)) => ExtendingNum (MultiSignal n a) (MultiSignal n b) where
  type AResult (MultiSignal n a) (MultiSignal n b) = MultiSignal n (AResult a b)
  plus  = liftA2 plus
  minus = liftA2 minus
  type MResult (MultiSignal n  a) (MultiSignal n b) = MultiSignal n (MResult a b)
  times = liftA2 times

-- | Constraints synonym
type SignalLike f = (Prependable f, Applicative f) 

mealyP :: (SignalLike f) 
       => (s -> i -> (s,o)) -- ^ Transfer function in mealy machine form:
                           -- @state -> input -> (newstate,output)@
      -> s                 -- ^ Initial state
      -> (f i -> f o)
      -- ^ Synchronous sequential function with input and output matching that
      -- of the mealy machine
mealyP f d i = o where
   r = f <$> prepend d s <*> i
   s = fst <$> r
   o = snd <$> r


mooreP :: (SignalLike f) 
       => (s -> i -> s) -- ^ Transfer function in moore machine form:
                       -- @state -> input -> newstate@
      -> (s -> o)      -- ^ Output function in moore machine form:
                       -- @state -> output@
      -> s             -- ^ Initial state
      -> (f i -> f o)  -- ^ Synchronous sequential function with input and output matching that
                       -- of the moore machine
mooreP fs fo s i = fmap fo r where
     r = fs <$> prepend s r <*> i 


windowP :: (KnownNat n, Default a, Prependable f)
       => f a               -- ^ 'SignalLike' to create a window over
       -> Vec n (f a)       -- ^ Vector of signals
windowP  x = iterateI (prepend def) x


-- | convert list to  'MultiSignal'
fromListP :: (NFData a) => SNat (n + 1) -> [a] -> MultiSignal (n + 1) a
fromListP n xs = MultiSignal $ CP.fromList $ l2vl n xs where
   l2vl n xs = v :  l2vl n ys where
      v = P.head <$> r
      ys = P.tail (CP.last r) 
      r = CP.iterate n P.tail xs
      vList = v : l2vl n ys 


-- | convert list to  'MultiSignal'
fromListPI :: (NFData a,KnownNat n) 
           => [a] -> MultiSignal (n + 1) a
fromListPI = withSNat fromListP


-- | Simulate a (@'MultiSignal' (n + 1) a -> 'MultiSignal' (n + 1) b@) 
-- function given a list of samples of type a
simulateP :: (NFData a, NFData b, KnownNat n) 
          => (MultiSignal (n + 1) a -> MultiSignal (n + 1) b )
          -> [a] 
          -> [b]
simulateP f xs = P.concat 
                 $ fmap toList 
                 $ sample 
                 $ unMultiSignal 
                 $ f 
                 $ fromListPI xs

-- Prependable

-- |
-- class that can be prepended
--
-- [/rule/]
--
--      @ toList ('prepend' a ax) == a : toList ax @
--
class Prependable f where
   prepend :: a -> f a -> f a

instance Prependable ZipList where
   prepend x xs = ZipList (x : getZipList xs)

instance (KnownNat n, n ~ (m+1)) => Prependable (MultiSignal n) where
    prepend x (MultiSignal s) = 
         MultiSignal $ liftA2 (+>>) (register x (fmap last s)) s

instance Prependable (Signal' SE.SystemClock) where
   prepend = CP.register
