{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# language ImplicitParams #-}
{-# language RankNTypes #-}
{-# language DeriveFunctor #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveTraversable #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

module Clash.Signal.MultiSignal (
    MultiSignal (..) ,
    -- * SignalLike
    SignalLike(..) ,
    mealyP ,
    mooreP ,
    windowP,
    -- * Simulation functions (not synthesisable)
    fromListP,
    fromListPI,
    simulateP,
    sampleP,
    -- * SignalLikeList (not synthesisable)
    SignalLikeList(..) ,
    signalLikeList2list,
    list2signalLikeList
) where


import qualified Data.Foldable as F
import qualified Prelude as P
import Clash.Prelude as CP
import Test.QuickCheck (Arbitrary(..),CoArbitrary(..))
import Control.DeepSeq
import Clash.Explicit.Signal as E
import Unsafe.Coerce
import           Clash.Explicit.Signal (System, resetSynchronizer, systemClockGen, systemResetGen, tbSystemClockGen)
import qualified Clash.Explicit.Signal as S
import           Clash.Promoted.Nat    (SNat (..))
import           Clash.Promoted.Symbol (SSymbol (..))
import           Clash.Signal.Bundle   (Bundle (..))
import           Clash.Signal.Internal (Clock)
import Control.Monad (join)

-- Stream of elements where n elements arrives at same clock
newtype MultiSignal n d a = MultiSignal {unMultiSignal :: Signal d (Vec n a) } 
      deriving ()


instance Functor (MultiSignal n d) where
    fmap f s = MultiSignal ( fmap ( fmap f ) ( unMultiSignal s ) )


instance KnownNat n => Applicative (MultiSignal n d) where
   pure = MultiSignal . pure . pure 
   f <*> s = MultiSignal  (fmap (<*>) fu <*> su) where
      fu = unMultiSignal f
      su = unMultiSignal s



instance (KnownNat n) =>  Foldable (MultiSignal n d) where
    foldr f x xs = (F.foldr (.) id xs') x where 
        xs' = (\ys y -> CP.foldr f y ys) <$> unMultiSignal xs      


instance (KnownNat m,  (n + 1) ~ m) => Traversable (MultiSignal m d) where
    traverse f xs = MultiSignal <$> sequenceA (fmap (traverse f) (unMultiSignal xs))


instance (Num a,KnownNat n) => Num (MultiSignal n d a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*) 
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger
    negate = fmap negate



instance Eq (MultiSignal n d a) where
   (==) = error "(==)' undefined for 'MultiSignal', use '(.==.)' instead"
   (/=) = error "(/=)' undefined for 'MultiSignal', use '(./=.)' instead"


instance  (Bounded a,KnownNat n)  =>  Bounded (MultiSignal n d a) where
    minBound = pure minBound 
    maxBound = pure maxBound


instance (Fractional a,KnownNat n) => Fractional (MultiSignal n d a) where
  (/)          = liftA2 (/)
  recip        = fmap recip
  fromRational = pure . fromRational


instance (Show a,KnownNat n) => Show (MultiSignal n d a) where
  show = show . unMultiSignal


instance (Arbitrary a,KnownNat n) => Arbitrary (MultiSignal n d a) where
  arbitrary  = MultiSignal <$> arbitrary


instance (CoArbitrary a) => CoArbitrary (MultiSignal n d a) where
  coarbitrary  = coarbitrary . unMultiSignal 


instance (FiniteBits a,KnownNat n) => FiniteBits (MultiSignal n d a) where
  finiteBitSize _ = finiteBitSize (undefined :: a)

instance (Bits a,KnownNat n) => Bits (MultiSignal n d a) where
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
  testBit          = error "'testBit' undefined for 'MultiSignal'', use 'fmap testBit'"
  bitSizeMaybe _   = bitSizeMaybe (undefined :: a)
  bitSize _        = maybe 0 id (bitSizeMaybe (undefined :: a))
  isSigned _       = isSigned (undefined :: a)
  shiftL a i       = fmap (`shiftL` i) a
  unsafeShiftL a i = fmap (`unsafeShiftL` i) a
  shiftR a i       = fmap (`shiftR` i) a
  unsafeShiftR a i = fmap (`unsafeShiftR` i) a
  rotateL a i      = fmap (`rotateL` i) a
  rotateR a i      = fmap (`rotateR` i) a
  popCount         = error "'popCount' undefined for 'MultiSignal'', use 'fmap popCount'"


instance (Default a,KnownNat n, n ~ (m+1)) => Default (MultiSignal n d a) where
  def = pure def


instance (SaturatingNum a,KnownNat n, n ~ (m+1)) => SaturatingNum (MultiSignal n d a) where
  satPlus = liftA2 . satPlus
  satMin = liftA2 . satMin
  satMult = liftA2 . satMult


instance (ExtendingNum a b,KnownNat n, n ~ (m+1)) => ExtendingNum (MultiSignal n d a) (MultiSignal n d b) where
  type AResult (MultiSignal n d a) (MultiSignal n d b) = MultiSignal n d (AResult a b)
  plus  = liftA2 plus
  minus = liftA2 minus
  type MResult (MultiSignal n d a) (MultiSignal n d b) = MultiSignal n d (MResult a b)
  times = liftA2 times


mealyP :: (HasClockReset d dc ds, Applicative (p d), SignalLike p , f ~ p d )
       => (s -> i -> (s,o)) -- ^ Transfer function in mealy machine form:
                           -- @state -> input -> (newstate,output)@
      -> s                 -- ^ Initial state
      -> (f i -> f o)
      -- ^ Synchronous sequential function with input and output matching that
      -- of the mealy machine
mealyP f d i = o where
   r = f <$> register' d s <*> i
   s = fst <$> r
   o = snd <$> r


mooreP :: (HasClockReset d dc ds, Applicative (p d), SignalLike p , f ~ p d)
       => (s -> i -> s) -- ^ Transfer function in moore machine form:
                       -- @state -> input -> newstate@
      -> (s -> o)      -- ^ Output function in moore machine form:
                       -- @state -> output@
      -> s             -- ^ Initial state
      -> (f i -> f o)  -- ^ Synchronous sequential function with input and output matching that
                       -- of the moore machine
mooreP fs fo s i = fmap fo r where
     r = fs <$> register' s r <*> i 



windowP :: (KnownNat n, Default a, HasClockReset d dc ds, Applicative (p d), SignalLike p , f ~ p d)
       => f a               -- ^ SignalLike to create a window over
       -> Vec n (f a)       -- ^ Vector of signals
windowP  x = iterateI (register' def) x



-- | convert list to  'MultiSignal'
fromListP :: (NFData a) => SNat (n + 1) -> [a] -> MultiSignal (n + 1) d a
fromListP n xs = MultiSignal $ CP.fromList $ l2vl n xs where
   l2vl n xs = v :  l2vl n ys where
      v = P.head <$> r
      ys = P.tail (CP.last r) 
      r = CP.iterate n P.tail xs
      vList = v : l2vl n ys 




-- | convert list to  'MultiSignal'
fromListPI :: (NFData a,KnownNat n) 
           => [a] -> MultiSignal (n + 1) d a
fromListPI = withSNat fromListP


-- | Simulate a (@'MultiSignal' (n + 1) a -> 'MultiSignal' (n + 1) b@) 
-- function given a list of samples of type a
simulateP :: (NFData a, NFData b, KnownNat n) 
          => (HasClockReset d 'Source 'Asynchronous 
                    => MultiSignal (n + 1) d a 
                    -> MultiSignal (n + 1) d b )
          -> [a] 
          -> [b]
simulateP f xs = P.concat 
                 $ fmap toList 
                 $ CP.sample 
                 $ unMultiSignal 
                 $ f 
                 $ fromListPI xs

-- | Change a (@'MultiSignal' (n + 1) a @)  to list
--
sampleP :: (KnownNat n, NFData b) 
      => (HasClockReset d 'Source 'Asynchronous 
             => MultiSignal (n + 1) d b)
      -> [b]
sampleP x = join $ fmap toList $ CP.sample $ unMultiSignal x


-- |
-- class that can be delayed/registered
--
class SignalLike (f :: Domain -> * -> * ) where
   register' :: (HasClockReset d dc ds)  =>  a -> f d a -> f d a

instance (KnownNat n, n ~ (m+1)) => SignalLike (MultiSignal n) where
    register' x (MultiSignal s) = 
         MultiSignal $ liftA2 (+>>) (register' x (fmap last s)) s

instance SignalLike Signal where
   register' = CP.register

newtype SignalLikeList (domain :: Domain) a =  SignalLikeList {unsll :: (ZipList a)} 
   deriving (Functor,Applicative,Show,Foldable,Traversable)

instance SignalLike SignalLikeList where
   register' x xs = SignalLikeList $ ZipList $ (x :) $ getZipList $ unsll xs 


signalLikeList2list ::  (HasClockReset d 'Source 'Asynchronous => SignalLikeList d a) -> [a]
signalLikeList2list =  getZipList . unsll where
                            ?clk = unsafeCoerce systemClockGen
                            ?rst = unsafeCoerce systemResetGen

list2signalLikeList  :: [a] -> (HasClockReset d 'Source 'Asynchronous => SignalLikeList d a)
list2signalLikeList  =  SignalLikeList . ZipList

-- instances for SignalLikeList

instance Num a => Num (SignalLikeList d a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*) 
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger
    negate = fmap negate

instance Eq (SignalLikeList d a) where
   (==) = error "(==)' undefined for 'SignalLikeList', use '(.==.)' instead"
   (/=) = error "(/=)' undefined for 'SignalLikeList', use '(./=.)' instead"

instance  (Bounded a)  =>  Bounded (SignalLikeList d a) where
    minBound = pure minBound 
    maxBound = pure maxBound

instance (Fractional a) => Fractional (SignalLikeList d a) where
  (/)          = liftA2 (/)
  recip        = fmap recip
  fromRational = pure . fromRational

instance (FiniteBits a) => FiniteBits (SignalLikeList d a) where
  finiteBitSize _ = finiteBitSize (undefined :: a)

instance (Bits a) => Bits (SignalLikeList d a) where
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
  popCount         = error "'popCount' undefined for 'SignalLike'', use 'fmap popCount'"

instance (Default a) => Default (SignalLikeList d a) where
  def = pure def


instance (SaturatingNum a) => SaturatingNum (SignalLikeList d a) where
  satPlus = liftA2 . satPlus
  satMin = liftA2 . satMin
  satMult = liftA2 . satMult


instance (ExtendingNum a b) => ExtendingNum (SignalLikeList d a) (SignalLikeList d b) where
  type AResult (SignalLikeList d a) (SignalLikeList d b) = SignalLikeList d (AResult a b)
  plus  = liftA2 plus
  minus = liftA2 minus
  type MResult (SignalLikeList d a) (SignalLikeList d b) = SignalLikeList d (MResult a b)
  times = liftA2 times
