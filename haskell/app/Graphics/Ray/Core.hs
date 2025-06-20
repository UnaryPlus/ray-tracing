module Graphics.Ray.Core where

import Linear (V2(V2), V3(V3), dot, quadrance, (*^), (^/))
import System.Random (RandomGen, randomR)
import Control.Monad.State (MonadState, state)

infinity :: Double
infinity = 1/0

degrees :: Double -> Double
degrees x = x * pi / 180

type Vec3 = V3 Double
type Point3 = V3 Double
type Color = V3 Double

-- v need not be a unit vector
reflect :: Vec3 -> Vec3 -> Vec3
reflect normal v = 
  v - 2 * dot normal v *^ normal

randomUnitVector :: (RandomGen g, MonadState g m) => m Vec3
randomUnitVector = do
  vec <- V3 <$> state (randomR (-1, 1)) <*> state (randomR (-1, 1)) <*> state (randomR (-1, 1))
  let q = quadrance vec
  if 1e-8 <= q && q <= 1
    then pure (vec ^/ sqrt q)
    else randomUnitVector

randomInUnitDisk :: (RandomGen g, MonadState g m) => m (V2 Double)
randomInUnitDisk = do
  vec <- V2 <$> state (randomR (-1, 1)) <*> state (randomR (-1, 1))
  if quadrance vec <= 1
    then pure vec
    else randomInUnitDisk

data Ray = Ray Point3 Vec3
  deriving (Show)

type Interval = (Double, Double)

inInterval :: Interval -> Double -> Bool
inInterval (tmin, tmax) t = tmin < t && t < tmax

