{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Graphics.Ray.Core
import Graphics.Ray.Material
import Graphics.Ray.SceneObject
import Graphics.Ray.Texture
import Graphics.Ray.Geometry
import Graphics.Ray

import Linear (V3(V3), (*^), normalize)
import System.Random

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