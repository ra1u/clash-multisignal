{-# LANGUAGE FlexibleInstances #-}


module Clash.Signal.MultiSignal

where

import Clash.Prelude
import Clash.Signal.PolySignal
import qualified Clash.Explicit.Signal as E
import qualified Clash.Sized.Vector as V

import qualified Prelude as P
import qualified Data.Foldable as F



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


instance (KnownNat n, n ~ (m+1)) =>  Foldable (MultiSignal n d) where
    foldr f x xs = r
      where
        r = F.foldr fv x (unMultiSignal xs) 
        fv va b = foldr f b va

instance (KnownNat m,  (n + 1) ~ m) => Traversable (MultiSignal m d) where
    traverse f xs = MultiSignal <$> sequenceA (fmap (traverse f) (unMultiSignal xs))


instance (KnownNat n, n ~ (m+1)) => SignalLike (PolySignal (MultiSignal n)) where
   reg x xs = PolySignal (\a -> registerME (_clock a) (_reset a) (_enable a) x (sig xs a))


{-# NOINLINE registerME #-}
registerME :: (KnownDomain d, KnownNat n, n ~ (m+1) , NFDataX a)
          => Clock d
          -> Reset d
          -> Enable d
          -> a
          -> MultiSignal n d a
          -> MultiSignal n d a
registerME c r e x (MultiSignal s) = 
         MultiSignal $ (+>>) <$> (E.register c r e x (fmap last s)) <*> s


registerM :: (HiddenClockResetEnable d, KnownNat n, n ~ (m+1) , NFDataX a)
          => a
          -> MultiSignal n d a
          -> MultiSignal n d a
registerM = hideClockResetEnable registerME



-- | convert list to  'MultiSignal'
fromListM :: (NFDataX a, KnownNat n, n ~ (m+1)) => SNat n -> [a] -> MultiSignal n d a
fromListM n xs = MultiSignal $ fromList $ l2vl xs where
   l2vl ys = V.unsafeFromList h : l2vl t where
      (h,t) = P.splitAt (fromInteger (natVal n)) ys

-- | convert list to  'MultiSignal'
fromListMI :: (NFDataX a, KnownNat n, n ~ (m+1)) 
           => [a] -> MultiSignal n d a
fromListMI = withSNat fromListM


toListM :: (KnownDomain d, KnownNat n, n ~ (m+1) , NFDataX a)
        => MultiSignal n d a
        -> [a]
toListM s = P.concat
          $ fmap toList
          $ sampleWithReset (SNat  @1)
          $ unMultiSignal
          $ s


-- | Simulate a (@'MultiSignal' (n + 1) a -> 'MultiSignal' (n + 1) b@) 
-- function given a list of samples of type a

simulateM :: (KnownDomain d, KnownNat n, n ~ (m+1) , NFDataX a, NFDataX b)
          => (MultiSignal n d a -> MultiSignal n d b)
          -> [a] 
          -> [b]
simulateM f xs = toListM
               $ f
               $ fromListMI xs


