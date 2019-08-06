{-# LANGUAGE TupleSections #-}
{-# OPTIONS -Wall #-}
module MacKay where

import Dist
import Plot (renderToFile, plotHistogram, binFloat, plotTrajectories, _trajectory_lines)
import Graphics.Rendering.Chart (Plot, toPlot, plot_lines_values)
import Control.Lens ((.~))
import Data.Default (Default (def))
import Control.Monad (liftM)

lebesgue :: MonadDist m => m Double
lebesgue = density (recip . dnorm 0 10) (normal 0 10)

mackayDensity :: Double -> Prob
mackayDensity x = exp_ (0.4 * (x - 0.4) ^ (2::Int) - 0.08 * x ^ (4::Int))

mackay :: MonadDist m => m Double
mackay = density mackayDensity lebesgue

mackaySample :: IO ()
mackaySample = do
  samples <- tabSample 500000 mackay
  renderToFile def "/tmp/plot.png" (plotHistogram (binFloat 50) samples)

step :: MonadDist m => Double -> m Double
step old = do
  r <- stdUniform
  let new = old + (r - 0.5)
      ratio = mackayDensity new / mackayDensity old
  if ratio < 1 then fromList [(new, ratio), (old, 1-ratio)]
               else return new

steps :: MonadDist m => Int -> Double -> m [Double]
steps 0 old = return [old]
steps n old = do new <- step old
                 xs  <- steps (n-1) new
                 return (new:xs)

plotTrajectories' :: [[Double]] -> [Plot Double Int]
plotTrajectories' xss = plotTrajectories (def { _trajectory_lines = True })
                                         [ zip xs [0::Int ..] | xs <- xss ]

plotSteps :: IO ()
plotSteps = do
  renderToFile def "/tmp/density.png" [toPlot (plot_lines_values .~ [[ (x, mackayDensity x) | x <- [-6,-5.9..4] ]] $ def)]
  trajectories <- fmap (map (drop 1000 . fst))
                $ tabSample 6
                $ fmap (\x -> 10 * (x - 0.5)) stdUniform >>= steps 10000
  renderToFile def "/tmp/plot.png" (plotHistogram (binFloat 50) (map (, 1::Int) (concat trajectories)))
  renderToFile def "/tmp/trajectory.png" (plotTrajectories' (map (take 50) trajectories))

mackayMH :: IO ()
mackayMH = do
  samples <- liftM (drop 10000)
           $ tabMH 100000 mackay
  renderToFile def "/tmp/plot.png"
           $ plotHistogram (binFloat 50)
           $ map (, 1::Int)
           $ samples
  renderToFile def "/tmp/trajectory.png"
           $ plotTrajectories' [take 50 samples]
