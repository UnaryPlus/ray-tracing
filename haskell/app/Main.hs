{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Linear (V2(V2), V3(V3), dot, quadrance, (^*), (*^), (^/), normalize, cross, zero, norm)
import Data.Massiv.Array (Matrix, D, B, S, Ix2((:.)), Sz(Sz), (!))
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.IO as I
import Graphics.Color.Space.RGB (SRGB, Linearity(Linear, NonLinear))
import qualified Graphics.Pixel.ColorSpace as C
import Control.Monad.State (State, MonadState, state, evalState)
import System.Random

import Control.Monad (guard, replicateM)
import Debug.Trace

infinity :: Double
infinity = 1/0

degrees :: Double -> Double
degrees x = x * pi / 180

type Vec3 = V3 Double
type Point3 = V3 Double
type Color = V3 Double
type Interval = (Double, Double)

inInterval :: Interval -> Double -> Bool
inInterval (tmin, tmax) t = tmin < t && t < tmax

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

-- v need not be a unit vector
reflect :: Vec3 -> Vec3 -> Vec3
reflect normal v = 
  v - 2 * dot normal v *^ normal

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
newtype Material = Material (Ray -> HitRecord -> State StdGen (Color, Maybe (Color, Ray)))
newtype SceneObject = SceneObject (Ray -> Interval -> Maybe (HitRecord, Material))

newtype Texture = Texture (Point3 -> V2 Double -> Color)

constantTexture :: Color -> Texture
constantTexture color = Texture (\_ _ -> color)

solidTexture :: (Point3 -> Color) -> Texture
solidTexture f = Texture (\p _ -> f p)

uvTexture :: (V2 Double -> Color) -> Texture
uvTexture f = Texture (const f)

imageTexture :: (A.Manifest r Color) => A.Matrix r Color -> Texture
imageTexture image = let 
  Sz (h :. w) = A.size image 
  w' = fromIntegral w
  h' = fromIntegral h
  in
  uvTexture $ \(V2 u v) -> let
    i = floor (u * w') `mod` w
    j = floor ((1 - v) * h') `mod` h
    in
    image ! (j :. i)

-- With default camera settings (-z direction is forward, +y direction is up),
-- texture images will be wrapped around the sphere starting and ending on the
-- far side of the sphere.
sphereUV :: Vec3 -> V2 Double
sphereUV (V3 x y z) = V2 u v
  where
    u = atan2 x z / (2 * pi) + 0.5
    v = acos (-y) / pi 

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
      , hr_uv = sphereUV outwardNormal
      }

lightSource :: Texture -> Material
lightSource (Texture tex) = Material $
  \_ (HitRecord {..}) -> pure (tex hr_point hr_uv, Nothing) 

lambertian :: Texture -> Material
lambertian (Texture tex) = Material $
  \_ (HitRecord {..}) -> do
    u <- randomUnitVector
    -- TODO: make sure hr_normal + u is not too close to 0?
    pure (zero, Just (tex hr_point hr_uv, Ray hr_point (hr_normal + u)))

mirror :: Texture -> Material
mirror (Texture tex) = Material $
  \(Ray _ dir) (HitRecord {..}) -> 
    pure (zero, Just (tex hr_point hr_uv, Ray hr_point (reflect hr_normal dir)))

metal :: Double -> Texture -> Material
metal fuzz (Texture tex) = Material $
  \(Ray _ dir) (HitRecord {..}) -> do
    u <- randomUnitVector
    let dir' = normalize (reflect hr_normal dir) + (fuzz *^ u)
    let scatter = dot dir' hr_normal > 0
    pure (zero, if scatter then Just (tex hr_point hr_uv, Ray hr_point dir') else Nothing)

dielectric :: Double -> Material
dielectric ior = Material $
  \(Ray _ dir) (HitRecord {..}) -> do
    let iorRatio = if hr_frontFace then 1/ior else ior
    let u = normalize dir
    let cosTheta = min 1 (dot hr_normal (-u))
    let sinTheta = sqrt (1 - cosTheta * cosTheta)
    let cannotRefract = iorRatio * sinTheta > 1

    let r0 = (1 - iorRatio) / (1 + iorRatio)
    let r0' = r0 * r0
    let reflectance = r0' + (1 - r0') * (1 - cosTheta)**5 -- Schlick approximation
    x <- state random

    let dir' = if cannotRefract || x < reflectance
          then reflect hr_normal u
          else refract iorRatio cosTheta hr_normal u
    
    pure (zero, Just (V3 1 1 1, Ray hr_point dir'))
  
  where
    refract :: Double -> Double -> Vec3 -> Vec3 -> Vec3 
    refract iorRatio cosTheta normal u = let
      perp = iorRatio *^ (u + cosTheta *^ normal) 
      para = -(sqrt (abs (1 - quadrance perp)) *^ normal)
      in perp + para

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

data CameraSettings = CameraSettings
  { cs_center :: Point3
  , cs_lookAt :: Point3
  , cs_up :: Vec3
  , cs_vfov :: Double
  , cs_aspectRatio :: Double
  , cs_imageWidth :: Int
  , cs_samplesPerPixel :: Int
  , cs_maxRecursionDepth :: Int
  , cs_background :: Ray -> Color
  , cs_defocusAngle :: Double
  , cs_focusDist :: Double
  }

defaultCameraSettings :: CameraSettings
defaultCameraSettings = CameraSettings
  { cs_center = V3 0 0 0
  , cs_lookAt = V3 0 0 (-1)
  , cs_up = V3 0 1 0
  , cs_vfov = pi / 2
  , cs_aspectRatio = 1.0
  , cs_imageWidth = 100
  , cs_samplesPerPixel = 10
  , cs_maxRecursionDepth = 10
  , cs_background = const (V3 0 0 0)
  , cs_defocusAngle = 0.0
  , cs_focusDist = 10.0
  }

raytrace :: CameraSettings -> SceneObject -> StdGen -> Matrix D Color
raytrace (CameraSettings {..}) (SceneObject hitWorld) seed = let
  imageHeight = ceiling (fromIntegral cs_imageWidth / cs_aspectRatio)
  viewportHeight = cs_focusDist * tan (cs_vfov / 2) * 2
  viewportWidth = viewportHeight * fromIntegral cs_imageWidth / fromIntegral imageHeight

  w = normalize (cs_center - cs_lookAt)
  u = normalize (cross cs_up w) 
  v = cross w u

  across = viewportWidth *^ u
  down = -(viewportHeight *^ v)

  topLeft = cs_center - w ^* cs_focusDist - across ^/ 2 - down ^/ 2
  pixelU = across ^/ fromIntegral cs_imageWidth
  pixelV = down ^/ fromIntegral imageHeight

  defocusRadius = cs_focusDist * tan (cs_defocusAngle / 2)
  defocusDiskU = u ^* defocusRadius
  defocusDiskV = v ^* defocusRadius
  
  sampleDefocusDisk :: State StdGen Point3
  sampleDefocusDisk = do
    V2 x y <- randomInUnitDisk
    pure (cs_center + x *^ defocusDiskU + y *^ defocusDiskV)

  samplePixel :: Int -> Int -> State StdGen Point3
  samplePixel i j = do
    x <- state random
    y <- state random
    pure (topLeft + (fromIntegral i + x) *^ pixelU + (fromIntegral j + y) *^ pixelV)

  getRay :: Int -> Int -> State StdGen Ray
  getRay i j = do
    origin <- sampleDefocusDisk
    target <- samplePixel i j
    pure (Ray origin (target - origin))

  rayColor :: Int -> Ray -> State StdGen Color
  rayColor depth ray
    | depth <= 0 = pure zero
    | otherwise =
    case hitWorld ray (0.0001, infinity) of
      Nothing -> pure (cs_background ray)
      Just (hit, Material mat) -> 
        mat ray hit >>= \case
          (emitted, Nothing) -> pure emitted
          (emitted, Just (attenuation, ray')) -> do
            c <- rayColor (depth - 1) ray'
            pure (emitted + attenuation * c)
  
  pixelColor :: Int -> Int -> State StdGen Color
  pixelColor i j = do
    colors <- replicateM cs_samplesPerPixel (getRay i j >>= rayColor cs_maxRecursionDepth)
    pure (sum colors ^/ fromIntegral cs_samplesPerPixel)

  -- array of random seeds for each pixel (constructed using splitGen)
  seeds = snd (A.randomArrayS seed (Sz (imageHeight :. cs_imageWidth)) splitGen) :: Matrix B StdGen

  in A.makeArray A.Par (Sz (imageHeight :. cs_imageWidth)) (\ix@(j :. i) -> evalState (pixelColor i j) (seeds ! ix))

readImage :: (A.Manifest r Color) => FilePath -> IO (Matrix r Color)
readImage path = A.compute . A.map fromPixel <$> (I.readImageAuto path :: IO (Matrix S (C.Pixel (SRGB 'Linear) Double)))
  where
    fromPixel :: C.Pixel (SRGB 'Linear) Double -> Color
    fromPixel (C.Pixel (C.ColorSRGB r g b)) = V3 r g b

writeImage :: (A.Source r Color) => FilePath -> Matrix r Color -> IO ()
writeImage path m = I.writeImageAuto path (A.map toPixel m)
  where
    toPixel :: Color -> C.Pixel (SRGB 'Linear) Double
    toPixel (V3 r g b) = C.Pixel (C.ColorSRGB r g b)

-- Write image using incorrect color space conversion from "Ray Tracing in One Weekend"
writeImageRTW :: (A.Source r Color) => FilePath -> Matrix r Color -> IO ()
writeImageRTW path m = I.writeImageAuto path (A.map toPixel m)
  where
    toPixel :: Color -> C.Pixel (SRGB 'NonLinear) Double
    toPixel (V3 r g b) = C.Pixel (C.ColorSRGB (sqrt r) (sqrt g) (sqrt b))

sky :: Ray -> Color
sky (Ray _ (normalize -> V3 _ y _)) = 
  let a = 0.5 * (y + 1) in
  (1 - a) *^ V3 1 1 1 + a *^ V3 0.5 0.7 1

metalTest :: IO ()
metalTest = do
  let materialGround = lambertian (constantTexture (V3 0.8 0.8 0.0))
  let materialCenter = lambertian (constantTexture (V3 0.1 0.2 0.5))
  let materialLeft = dielectric 1.5
  let materialBubble = dielectric (1 / 1.5)
  let materialRight = metal 1.0 (constantTexture (V3 0.8 0.6 0.2))

  let world = group 
        [ geometryObject (sphere (V3 0 (-100.5) (-1)) 100) materialGround
        , geometryObject (sphere (V3 0 0 (-1.2)) 0.5) materialCenter
        , geometryObject (sphere (V3 (-1) 0 (-1)) 0.5) materialLeft
        , geometryObject (sphere (V3 (-1) 0 (-1)) 0.4) materialBubble
        , geometryObject (sphere (V3 1 0 (-1)) 0.5) materialRight
        ]

  let settings = defaultCameraSettings 
        { cs_aspectRatio = 16/9
        , cs_imageWidth = 400
        , cs_samplesPerPixel = 100
        , cs_maxRecursionDepth = 50
        , cs_background = sky
        , cs_center = V3 (-2) 2 1
        , cs_lookAt = V3 0 0 (-1)
        , cs_vfov = degrees 20
        , cs_defocusAngle = degrees 10
        , cs_focusDist = 3.4
        }

  writeImageRTW "test_image.png" $ raytrace settings world (mkStdGen 50)

main :: IO ()
main = metalTest