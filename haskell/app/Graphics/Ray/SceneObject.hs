{-# LANGUAGE TupleSections #-}
module Graphics.Ray.SceneObject where

import Graphics.Ray.Core
import Graphics.Ray.Geometry
import Graphics.Ray.Material

newtype SceneObject = SceneObject (Ray -> Interval -> Maybe (HitRecord, Material))

geometryObject :: Geometry -> Material -> SceneObject
geometryObject (Geometry f) mat = SceneObject $ 
  \r i -> fmap (, mat) (f r i)

group :: [SceneObject] -> SceneObject
group obs = SceneObject $ \ray (tmin, tmax) ->
  let try (tmax', knownHit) (SceneObject hitObj) =
        case hitObj ray (tmin, tmax') of
          Nothing -> (tmax', knownHit)
          Just (hit, mat) -> (hr_t hit, Just (hit, mat))
  in snd (foldl try (tmax, Nothing) obs)