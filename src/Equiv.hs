{-# LANGUAGE TupleSections #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Equiv where

import Dist
import Plot (renderToFile, binFloat, plotHistogram, plotTrajectory, removeOutliers)
import Graphics.Rendering.Chart (layout_plots, layout_x_axis, layout_y_axis, laxis_generate, scaledAxis)
import Data.Default (Default (def))
import Control.Lens ((.~))

main :: IO ()
main = do
  let mh = fmap (map (, 1::Prob) . drop 100000) . tabMH 300000
      oneD (name, tab) = do
        samples <- tab
        let plots = plotHistogram (binFloat 30) (removeOutliers samples)
        renderToFile def ("/tmp/" ++ name)
          $ layout_plots .~ plots
          $ (layout_x_axis . laxis_generate) .~ scaledAxis def (-7,7)
          $ def
  mapM_ oneD [("m1a", tabSample 10000 m1a), ("m1b", tabSample 10000 m1b),
              ("m2a", tabSample 10000 m2a), ("m2b", tabSample 10000 m2b),
              ("m5a", tabSample 80000 m5a), ("m5b", tabSample 10000 m5b),
              ("m6a", mh              m6a), ("m6b", tabSample 10000 m6b),
              ("m7a", tabSample 10000 m7a), ("m7b", tabSample 10000 m7b),
              ("m8a", tabSample 10000 m8a), ("m8b", tabSample 10000 m8b)]
  let twoD (name, dist, dy) = do
        samples <- tabSample 10000 dist
        let plots = plotTrajectory def (map (\(xy,1) -> xy) samples)
        renderToFile def ("/tmp/" ++ name)
          $ layout_plots .~ plots
          $ (layout_x_axis . laxis_generate) .~ scaledAxis def (  -7,   7)
          $ (layout_y_axis . laxis_generate) .~ scaledAxis def (dy-7,dy+7)
          $ def
  mapM_ twoD [("m3a", m3a, 2), ("m3b", m3b, 2),
              ("m4a", m4a, 0), ("m4b", m4b, 0)]

-- Variable elimination (easy case)
-- aka dead code elimination

m1a, m1b :: MonadDist m => m Double
m1a = do
  x <- normal 0 1
  y <- normal x 1
  return x
m1b = do
  x <- normal 0 1
  return x

-- Variable elimination (harder case) (Kalman filter)
-- aka integrating out, collapse, marginalization, Rao-Blackwellization

m2a, m2b :: MonadDist m => m Double
m2a = do
  x <- normal 0 1
  y <- normal x 1
  return y
m2b = do
  y <- normal 0 (sqrt 2)
  return y

-- Commutation (easy case)

m3a, m3b :: MonadDist m => m (Double, Double)
m3a = do
  x <- normal 0 1
  y <- normal 2 3
  return (x,y)
m3b = do
  y <- normal 2 3
  x <- normal 0 1
  return (x,y)

-- Commutation (harder case)
-- aka conditioning, disintegration

m4a, m4b :: MonadDist m => m (Double, Double)
m4a = do
  x <- normal 0 1
  y <- normal x 1
  return (x,y)
m4b = do
  y <- normal 0 (sqrt 2)
  x <- normal (y / 2) (1 / sqrt 2)
  return (x,y)

-- Density (easy case)

m5a, m5b :: MonadDist m => m Double
m5a = do
  x <- lebesgue
  factor (dnorm 1 2 x)
  return x
m5b = do
  x <- normal 1 2
  return x

-- Density (harder case) (Kalman filter)
-- aka conjugacy

m6a, m6b :: MonadDist m => m Double
m6a = do
  x <- normal 0 1
  factor (dnorm 7 0.75 x)
  return x
m6b = do
  factor (exp_ (-15.68) * prob (0.8 / sqrt (2 * pi)))
  x <- normal 4.48 0.6
  return x

-- Reparametrization (easy case)

m7a, m7b :: MonadDist m => m Double
m7a = do
  x <- normal 0 1
  return (1 + 2 * x)
m7b = do
  x <- normal 1 2
  return x

-- Reparametrization (harder case)

m8a, m8b :: MonadDist m => m Double
m8a = do
  x <- stdUniform
  return (- log x)
m8b = do
  x <- stdExponential
  return x

