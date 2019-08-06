{-# LANGUAGE TupleSections #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Misc where

import Dist (tabSample, tabMH, density, exp_, stdNormal, flatDirichlet)
import Plot (renderToFile, binFloat, plotHistogram, plotTrajectory)
import Data.Default (Default (def))
import Control.Monad (liftM, liftM2)

normalSample :: IO ()
normalSample = do
  samples <- tabSample 100000
           $ density (\x -> exp_ (-(x-8)^(2::Int)/2)) 
           $ stdNormal
  renderToFile def "/tmp/plot.png" (plotHistogram (binFloat 20) samples)

normalMH :: IO ()
normalMH = do
  samples <- liftM (map (, 1::Int) . drop 10000) . tabMH 100000
           $ density (\x -> exp_ (-(x-8)^(2::Int)/2)) 
           $ stdNormal
  renderToFile def "/tmp/plot.png" (plotHistogram (binFloat 20) samples)

dirichletSample :: IO ()
dirichletSample = do
  samples <- tabSample 10000 (fmap (\[_,x,y] -> (x,y)) (flatDirichlet 3))
  renderToFile def "/tmp/plot.png" (plotTrajectory def (map fst samples))

ellipse :: IO ()
ellipse = do
  samples <- liftM (drop 10000) . tabMH 100000
           $ density (\(x,y) -> exp_ (-(x-y)^(2::Int)))
           $ liftM2 (,) stdNormal stdNormal
  renderToFile def "/tmp/plot.png" (plotTrajectory def samples)
