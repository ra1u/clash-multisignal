
import Prelude ((.))

import Clash.Prelude
import Clash.Signal.MultiSignal
import Clash.Signal.PolySignal

import qualified Prelude as P
import qualified Data.Foldable as F
import qualified Clash.Explicit.Signal as E

-- integ is integrator over geberic signal
integX :: (KnownDomain d, NFDataX a, Num a, SignalLike sig,
          Applicative (sig d))
       => sig d a
       -> sig d a
integX s = r where
  r = liftA2 (+) s (reg 0 r)



-- integS iover mealyS (Generic version of mealy)
integS :: (KnownDomain d, NFDataX a, Num a, SignalLike sig,
          Applicative (sig d))
       => sig d a
       -> sig d a
integS = mealyS integT 0


integT :: Num a
       => a -> a -> (a, a) 
integT s i = (o,o)
  where o = s + i

-- instance of integrator as Signal System (Signed 9)  
topEntity1
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Signed 9)
  -> Signal System (Signed 9)
topEntity1 = runPolySignal integS

-- instance of integrator as MultiSignal 4  System (Signed 9)
-- check next section ro see howto reason abot this
topEntity2
  :: Clock System
  -> Reset System
  -> Enable System
  -> MultiSignal 4 System (Signed 9)
  -> MultiSignal 4 System (Signed 9)
topEntity2 = runPolySignal integX




-- testEntity2sim = P.take 10 
-- instead of running 
testEntity2 = P.take 10 
              $ simulate
              ( unMultiSignal
              . topEntity2 clockGen resetGen enableGen
              . MultiSignal )
              $ P.repeat
              $  pure 1
-- given constant signal 1 as input, we get
-- [<1,2,3,4>,<5,6,7,8>,<9,10,11,12>,<13,14,15,16>,
-- <17,18,19,20>,<21,22,23,24>,<25,26,27,28>,
-- <29,30,31,32>,<33,34,35,36>,<37,38,39,40>]

testEntity2sim = P.take 10
               $ simulateM (topEntity2 clockGen resetGen enableGen)
               $ [1,1 .. ]
-- output [1,2,3,4,5,6,7,8,9,10]


-- example circuitry
-- circuitry takes 4 values per clock and outputs 4 values per clock and integarte output
-- if you give input as constat, than ouptut is as per testEntity2
topEntity = topEntity2

