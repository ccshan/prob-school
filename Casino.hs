{-# OPTIONS -Wall #-}
module Casino where

import Dist
import Plot (renderToFile, plotHeatMap, plotHistogram, tally, binPair, binFloat, binIx)
import Data.Default (Default (def))
import Data.Number.LogFloat (pow)
import Control.Monad (replicateM)
import Text.Show.Pretty (dumpDoc)

coinFlips :: MonadDist m => Double -> Int -> m Int
coinFlips p n = fmap (length . filter id) (replicateM n (coinFlip p))

casino :: MonadDist m => m (Double, Int)
casino = do
  p <- stdUniform
  before <- coinFlips p 8
  observe (before == 5)
  after <- coinFlips p 3
  return (p, after)

casino' :: MonadDist m => m (Double, Int)
casino' = do
  p <- stdUniform
  factor (pow (prob p) 5 * pow (prob (1-p)) 3 * 56)
  after <- coinFlips p 3
  return (p, after)

main :: IO ()
main = do
  samples <- tabSample 10000 casino'
  -- let values = tally (binPair (binFloat 20) binIx) samples
  -- print (dumpDoc values)
  renderToFile def "/tmp/plot.png" (plotHeatMap def (binFloat 20, binIx) samples)
