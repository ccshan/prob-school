{-# LANGUAGE TupleSections #-}
{-# OPTIONS -Wall #-}
module MacKay where

import Dist
import Plot (renderToFile, plotHistogram, binFloat, plotTrajectory, _trajectory_lines)
import Data.Default (Default (def))
import Control.Monad (liftM)

lebesgue :: MonadDist m => m Double
lebesgue = density (recip . dnorm 0 10)
                   (normal 0 10)

mackay :: MonadDist m => m Double
mackay = density (\x -> exp_ (0.4 * (x - 0.4) ^ (2::Int) - 0.08 * x ^ (4::Int)))
                 lebesgue

mackaySample :: IO ()
mackaySample = do
  samples <- tabSample 500000 mackay
  renderToFile def "/tmp/plot.png" (plotHistogram (binFloat 50) samples)

mackayMH :: IO ()
mackayMH = do
  samples <- liftM (drop 10000)
           $ tabMH 100000 mackay
  renderToFile def "/tmp/plot.png"
           $ plotHistogram (binFloat 50)
           $ map (, 1::Int)
           $ samples
  renderToFile def "/tmp/trajectory.png"
           $ plotTrajectory (def { _trajectory_lines = True })
           $ zip (take 50 samples) [0::Int ..]
