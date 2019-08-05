{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Casino where

import Dist
import Plot (renderToFile, plotHeatMap, plotHistogram, tally, binPair, binFloat, binIx)
import Graphics.Rendering.Chart (layout_plots, layout_x_axis, laxis_generate, scaledAxis)
import Control.Lens ((.~))
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
  renderToFile def "/tmp/plot.png" (plotHeatMap def (binFloat 20, binIx) samples)

validate :: IO ()
validate = do
  estimates <- replicateM 200 $ do
    samples <- tabSample 1000 (fmap ((> 0) . snd) casino)
    let [((_,False,_),wf), ((_,True ,_),wt)] = tally binIx samples
    print (dumpDoc (wf, wt))
    return (fromProb wt / (fromProb wt + fromProb wf), 1::Int)
  let lay plots = layout_plots .~ plots
                $ (layout_x_axis . laxis_generate) .~ scaledAxis def (0.8,1)
                $ def
  renderToFile def "/tmp/plot.png" $ lay $ plotHistogram (binFloat 20) estimates
