{-# LANGUAGE FlexibleInstances #-}


module Clash.Signal.PolySignal where

import Clash.Prelude
-- import qualified Clash.Signal as S
import qualified Clash.Explicit.Signal as E
-- import qualified Clash.Hidden as H
import qualified Control.Lens.TH as L
  
-- import qualified Data.Foldable as F
-- import qualified Prelude as P


data PolySignalArg (dom :: Domain) = PolySignalArg { _clock :: Clock dom, _reset :: Reset dom, _enable :: Enable dom }
L.makeLenses ''PolySignalArg

sigarg :: Clock dom
       -> Reset dom
       -> Enable dom
       -> PolySignalArg dom
sigarg c r e = PolySignalArg { _clock = c, _reset = r, _enable = e}



data PolySignal s (dom :: Domain) a = PolySignal { sig :: PolySignalArg dom -> s dom a }

instance (Functor (sig (dom :: Domain))) => Functor (PolySignal sig (dom :: Domain)) where
  fmap f s = PolySignal $ \a -> fmap f (sig s a) 

instance (Applicative (sig (dom :: Domain))) => Applicative (PolySignal sig (dom :: Domain)) where
  pure = toPolySignal . pure
  f <*> k = PolySignal $ \s -> (sig f s <*> sig k s)


class SignalLike (s :: Domain -> Type -> Type)  where
  reg :: ( KnownDomain d
          , NFDataX a )
       => a -> s d a -> s d a

instance (  Applicative (sig (dom :: Domain)),
            Num a)
         => Num (PolySignal sig dom a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*) 
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger
    negate = fmap negate


fromPolySignal :: PolySignalArg d
        -> PolySignal s d a
        -> s d a
fromPolySignal a s = sig s a

toPolySignal :: s d a
             -> PolySignal s d a
toPolySignal = PolySignal . const


-- run Sig (forall s convert (Sig s -> Sig s) into (s -> s)  ) 
runPolySignal ::
          (KnownDomain d, NFDataX a, NFDataX b, Applicative (sig d))
       => (PolySignal sig d a -> PolySignal sig d b)
       -> Clock d
       -> Reset d
       -> Enable d
       -> sig d a
       -> sig d b
runPolySignal f c r e s = x where
  sa = sigarg c r e
  i = toPolySignal s
  x = fromPolySignal sa (f i)


mealyS :: (KnownDomain d, NFDataX s, SignalLike sig, Applicative (sig d))
       => (s -> i -> (s,o)) -- ^ Transfer function in mealy machine form:
                            -- @state -> input -> (newstate,output)@
      -> s                  -- ^ Initial state
      -> (sig d i-> sig d o)
      -- ^ Synchronous sequential function with input and output matching that
      -- of the mealy machine
mealyS f d i = o where
   r = f <$> reg d s <*> i
   s = fst <$> r
   o = snd <$> r



mooreS  :: ( KnownDomain d , NFDataX s, SignalLike sig, Applicative (sig d))
  => (s -> i -> s)
  -- ^ Transfer function in moore machine form: @state -> input -> newstate@
  -> (s -> o)
  -- ^ Output function in moore machine form: @state -> output@
  -> s
  -- ^ Initial state
  -> (sig d i -> sig d o)
  -- ^ Synchronous sequential function with input and output matching that
  -- of the moore machine
mooreS ft fo iS i = o where 
  s' = ft <$> s <*> i
  s  = reg iS s'
  o  = fo <$> s

-- | Create a synchronous function from a combinational function describing
-- a moore machine without any output logic
medvedevS
  :: ( KnownDomain d , NFDataX s, SignalLike sig, Applicative (sig d))
  => (s -> i -> s)
  -> s
  -> (sig d i -> sig d s)
medvedevS tr st = mooreS tr id st

windowS :: (SignalLike s, KnownDomain d,KnownNat n ,NFDataX a, Default a)
       => s d a               -- ^ 'SignalLike' to create a window over
       -> Vec n (s d a)       -- ^ Vector of signals
windowS  x = iterateI (reg def) x


instance SignalLike (PolySignal Signal) where
   reg x xs = PolySignal (\a -> E.register (_clock a) (_reset a) (_enable a) x (sig xs a))


