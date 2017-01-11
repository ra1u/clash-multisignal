{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CLaSH.Signal.MultiSignal (
    MultiSignal (..) ,
    -- * Prependable
    Prependable(..) ,
    -- * Utility functons
    mealyP ,
    mooreP ,
    windowP 
) where

import qualified Data.Foldable as F
import qualified Prelude 
import CLaSH.Prelude as P
import CLaSH.Signal.Explicit as SE
import Test.QuickCheck (Arbitrary(..),CoArbitrary(..))


data MultiSignal n a = MultiSignal {unMultiSignal :: Signal (Vec n a) } 


instance Functor (MultiSignal n) where
    fmap f s = MultiSignal ( fmap ( fmap f ) ( unMultiSignal s ) )


instance KnownNat n => Applicative (MultiSignal n) where
   pure = MultiSignal . pure . pure 
   f <*> s = MultiSignal  (fmap (<*>) fu <*> su) where
      fu = unMultiSignal f
      su = unMultiSignal s


instance Foldable (MultiSignal n) where
    foldr f x xs = (F.foldr (.) id xs') x where 
        xs' = (\ys y -> P.foldr f y ys) <$> unMultiSignal xs       


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


instance Show a => Show (MultiSignal n a) where
  show x = foldMap ( \a -> mappend (show a) " ") x


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



mealyP :: (Prependable f, Applicative f) => (a1 -> a -> (a1, b)) -> a1 -> f a -> f b
mealyP f d i = o where
   r = f <$> prepend d s <*> i
   s = fst <$> r
   o = snd <$> r

mooreP fs fo s i = fmap fo r where
     r = fs <$> prepend s r <*> i 

windowP  x = iterateI (prepend def) x

-- Prependable

-- |
-- class that can be prepended
--
-- [/rule/]
--
--      @ toList (prep a ax) == a : toList ax @
--
class Prependable f where
   prepend :: a -> f a -> f a

instance Prependable ZipList where
   prepend x xs = ZipList (x : getZipList xs)

instance (KnownNat n, n ~ (m+1)) => Prependable (MultiSignal n) where
    prepend x (MultiSignal s) = 
         MultiSignal $ liftA2 (+>>) (register x (fmap last s)) s

instance Prependable (Signal' SE.SystemClock) where
   prepend = P.register
