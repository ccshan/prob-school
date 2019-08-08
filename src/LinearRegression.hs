{-# OPTIONS -Wall #-}
module LinearRegression where

import Dist
import Plot (renderToFile, plotTrajectory, plotHeatMap, binFloat, removeOutlierPairs)
import Data.Default (Default (def))
import Control.Monad (replicateM, forM_, liftM)

--------------------------------------------------------------------------------
-- Generative model

linear1DModel :: MonadDist m => Int
              -> m (Double, Double, [(Double, Double)])
linear1DModel n = do
  coeff0 <- normal 0 10
  coeff1 <- normal 0 10
  xys <- replicateM n (do
    x <- normal 0 20
    y <- normal (coeff0 + coeff1 * x) 3
    return (x, y))
  return (coeff0, coeff1, xys)

-- Conditional distribution of observed data given latent parameters

linear1DSynthesis :: MonadDist m => Int -> m [(Double, Double)]
linear1DSynthesis n = do
  let coeff0 = -6.9383
      coeff1 =  0.7124
  replicateM n (do
    x <- normal 0 20
    y <- normal (coeff0 + coeff1 * x) 3
    return (x, y))

fakePoints :: [(Double, Double)] -- linear1DSynthesis produced this fake data
fakePoints = [ (-20.03698206362577 ,-20.49021675929117)
             , ( -5.091072701578589,-11.074891690456273)
             , (  8.735097229558901, -1.3818968687184983)
             , (-21.006879622013063,-21.546262520768256) ]

-- Conditional distribution of latent parameters given observed data

linear1DRegression :: MonadDist m => [(Double, Double)] -> m (Double, Double)
linear1DRegression xys = do
  coeff0 <- normal 0 10
  coeff1 <- normal 0 10
  forM_ xys (\(x, y) -> do
    factor (dnorm (coeff0 + coeff1 * x) 3 y))
  return (coeff0, coeff1)

-- Infer latent parameters

main :: IO ()
main = do
  let m :: MonadDist m => m (Double, Double)
      m = linear1DRegression fakePoints
  samplesIS <- tabSample 50000 m
  renderToFile def "/tmp/plotIS"
    $ plotHeatMap def (binFloat 100, binFloat 100)
    $ removeOutlierPairs samplesIS
  samplesMH <- liftM (drop 1000) (tabMH 50000 m)
  renderToFile def "/tmp/plotMH"
    $ plotTrajectory def
    $ samplesMH
