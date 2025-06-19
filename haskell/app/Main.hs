{-# LANGUAGE TupleSections #-}
module Main where

import Linear (V2, V3(V3), dot, quadrance, (^*), (*^), (^/))
import Control.Monad (forM_, guard)
import Debug.Trace

type Vec3 = V3 Double
type Point3 = V3 Double
type Color = V3 Double
type Interval = (Double, Double)

inInterval :: Interval -> Double -> Bool
inInterval (tmin, tmax) t = tmin < t && t < tmax

data Ray = Ray Point3 Vec3
  deriving (Show)

data HitRecord = HitRecord
  { hr_t :: Double
  , hr_point :: Point3
  , hr_normal :: Vec3
  , hr_frontFace :: Bool
  , hr_uv :: V2 Double
  }

newtype Geometry = Geometry (Ray -> Interval -> Maybe HitRecord)
newtype Material = Material (Ray -> HitRecord -> (Color, Maybe (Color, Ray)))
newtype SceneObject = SceneObject (Ray -> Interval -> Maybe (HitRecord, Material))

sphere :: Point3 -> Double -> Geometry
sphere center radius = Geometry $
  \(Ray orig dir) bounds -> do
    let oc = center - orig
    let a = quadrance dir
    let h = dot dir oc 
    let c = quadrance oc - radius*radius

    let discriminant = h*h - a*c
    guard (discriminant >= 0)
    
    let sqrtd = sqrt discriminant
    let root1 = (h - sqrtd) / a
    let root2 = (h + sqrtd) / a

    t <- 
      if inInterval bounds root1 
        then Just root1 
      else if inInterval bounds root2
        then Just root2
      else Nothing
    
    let point = orig + t *^ dir
    let outwardNormal = (point - center) ^/ radius
    let frontFace = dot dir outwardNormal <= 0
    Just HitRecord
      { hr_t = t
      , hr_point = point
      , hr_normal = if frontFace then outwardNormal else -outwardNormal
      , hr_frontFace = frontFace
      , hr_uv = undefined -- TODO
      }

lightSource :: Color -> Material
lightSource color = Material $
  \_ _ -> (color, Nothing)

geometryObject :: Geometry -> Material -> SceneObject
geometryObject (Geometry f) mat = SceneObject $ 
  \r i -> fmap (, mat) (f r i)

raytracingTest :: Geometry -> (Int, Int) -> Double -> IO ()
raytracingTest (Geometry hitObj) (w, h) vfov = let
  cameraCenter = V3 0 0 1
  viewportHeight = tan (vfov / 2) * 2
  viewportWidth = viewportHeight * fromIntegral w / fromIntegral h
  topLeft = V3 (-(viewportWidth / 2)) (viewportHeight / 2) 0
  pixel_dx = V3 (viewportWidth / fromIntegral w) 0 0
  pixel_dy = V3 0 (-(viewportHeight / fromIntegral h)) 0
  pixel00 = topLeft + pixel_dx ^/ 2 + pixel_dy ^/ 2
  getRay i j = Ray cameraCenter (pixel00 + pixel_dx ^* fromIntegral i + pixel_dy ^* fromIntegral j - cameraCenter) 
  in do
  putStrLn "P3"
  putStrLn (show w ++ " " ++ show h)
  putStrLn "255"
  forM_ [0..h-1] $ \j ->
    forM_ [0..w-1] $ \i ->
      case hitObj (getRay i j) (0, 1000000000) of
        Nothing -> putStrLn "0 0 0"
        Just _ -> putStrLn "255 255 255"

main :: IO ()
main = raytracingTest (sphere (V3 0 0 0) 0.2) (120, 100) (pi / 2)
