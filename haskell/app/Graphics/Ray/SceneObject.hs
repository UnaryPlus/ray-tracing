{-# LANGUAGE TupleSections #-}
module Graphics.Ray.SceneObject where

import Graphics.Ray.Core
import Graphics.Ray.Geometry
import Graphics.Ray.Material

data SceneObject = SceneObject Box (Ray -> Interval -> Maybe (HitRecord, Material))

geometryObject :: Geometry -> Material -> SceneObject
geometryObject (Geometry bbox f) mat = 
  SceneObject bbox (\r i -> fmap (, mat) (f r i))

group :: [SceneObject] -> SceneObject
group obs = let
  bbox = boxHull [ b | SceneObject b _ <- obs ]
  
  hitGroup ray (tmin, tmax) =
    let try (tmax', knownHit) (SceneObject _ hitObj) =
          case hitObj ray (tmin, tmax') of
            Nothing -> (tmax', knownHit)
            Just (hit, mat) -> (hr_t hit, Just (hit, mat))
    in snd (foldl try (tmax, Nothing) obs)
  
  in SceneObject bbox hitGroup