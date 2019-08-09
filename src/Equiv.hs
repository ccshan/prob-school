{-# LANGUAGE TupleSections #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Equiv where

import Dist
import Plot (renderToFile, binFloat, plotHistogram, plotTrajectory, plotHeatMap, removeOutliers)
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
  mapM_ oneD [("m1", tabSample 10000 m1), ("m1Prime", tabSample 10000 m1'),
              ("m2", tabSample 10000 m2), ("m2Prime", tabSample 10000 m2'),
              ("m5", tabSample 80000 m5), ("m5Prime", tabSample 10000 m5'),
              ("m6", mh              m6), ("m6Prime", tabSample 10000 m6'),
              ("m7", tabSample 10000 m7), ("m7Prime", tabSample 10000 m7'),
              ("m8", tabSample 10000 m8), ("m8Prime", tabSample 10000 m8')]
  let twoD (name, dist, dy) = do
        samples <- tabSample 10000 dist
        let plots = plotTrajectory def (map (\(xy,1) -> xy) samples)
        renderToFile def ("/tmp/" ++ name)
          $ layout_plots .~ plots
          $ (layout_x_axis . laxis_generate) .~ scaledAxis def (  -7,   7)
          $ (layout_y_axis . laxis_generate) .~ scaledAxis def (dy-7,dy+7)
          $ def
  mapM_ twoD [("m3", m3, 2), ("m3'", m3', 2),
              ("m4", m4, 0), ("m4'", m4', 0)]

homework :: IO ()
homework = do
  let twoD name dist = do
        samples <- tabSample 100000 (fmap snd dist)
        renderToFile def ("/tmp/" ++ name)
          $ plotHeatMap def (binFloat 20, binFloat 20) samples
  twoD "borelSub"      borelSub
  twoD "borelSubPrime" borelSub'
  twoD "borelDiv"      borelDiv
  twoD "borelDivPrime" borelDiv'

-- Variable elimination (easy case)
-- aka dead code elimination

m1, m1' :: MonadDist m => m Double
m1 = do
  x <- normal 0 1
  y <- normal x 1
  return x
m1' = do
  x <- normal 0 1
  return x

-- Variable elimination (harder case) (Kalman filter)
-- aka integrating out, collapse, marginalization, Rao-Blackwellization

m2, m2' :: MonadDist m => m Double
m2 = do
  x <- normal 0 1
  y <- normal x 1
  return y
m2' = do
  y <- normal 0 (sqrt 2)
  return y

-- Commutation (easy case)

m3, m3' :: MonadDist m => m (Double, Double)
m3 = do
  x <- normal 0 1
  y <- normal 2 3
  return (x,y)
m3' = do
  y <- normal 2 3
  x <- normal 0 1
  return (x,y)

-- Commutation (harder case)
-- aka conditioning, disintegration

m4, m4' :: MonadDist m => m (Double, Double)
m4 = do
  x <- normal 0 1
  y <- normal x 1
  return (x,y)
m4' = do
  y <- normal 0 (sqrt 2)
  x <- normal (y / 2) (1 / sqrt 2)
  return (x,y)

-- Density (easy case)

m5, m5' :: MonadDist m => m Double
m5 = do
  x <- lebesgue
  factor (dnorm 1 2 x)
  return x
m5' = do
  x <- normal 1 2
  return x

-- Density (harder case) (Kalman filter)
-- aka conjugacy

m6, m6' :: MonadDist m => m Double
m6 = do
  x <- normal 0 1
  factor (dnorm 7 0.75 x)
  return x
m6' = do
  factor (exp_ (-15.68) * prob (0.8 / sqrt (2 * pi)))
  x <- normal 4.48 0.6
  return x

-- Reparametrization (easy case)

m7, m7' :: MonadDist m => m Double
m7 = do
  x <- normal 0 1
  return (1 + 2 * x)
m7' = do
  x <- normal 1 2
  return x

-- Reparametrization (harder case)

m8, m8' :: MonadDist m => m Double
m8 = do
  x <- stdUniform
  return (- log x)
m8' = do
  x <- stdExponential
  return x

-- Borel's paradox: how to equate borelSub to something of the form borelSub'
--              and how to equate borelDiv to something of the form borelDiv' ?

borelSub, borelSub' :: MonadDist m => m (Double, (Double, Double))
borelSub = do
  x <- stdUniform
  y <- stdUniform
  return (y-x, (x,y))
borelSub' = do
  t <- fmap fst borelSub
  x <- uniform (max 0 (0-t)) (min 1 (1-t))
  return (t, (x,t+x))

borelDiv, borelDiv' :: MonadDist m => m (Double, (Double, Double))
borelDiv = do
  x <- stdUniform
  y <- stdUniform
  return (y/x, (x,y))
borelDiv' = do
  s <- fmap fst borelDiv
  x <- uniform 0 (min 1 (1/s))
  return (s, (x,s*x))
