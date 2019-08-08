{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}
module Scatter where

import Data.Csv
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Data.Word (Word8)
import Plot (renderToFile, plotTrajectory)
import Data.Default (Default (def))

main :: IO ()
main = do
  dat <- readCSV
  let samples = [ (sample Map.! "coeff0", sample Map.! "coeff1")
                | sample <- dat ]
  renderToFile def "/tmp/plotHMC" (plotTrajectory def samples)

readCSV :: IO [Map.Map B.ByteString Double]
readCSV = do
  csv <- B.readFile "../stan/output.csv"
  let comment line = not (B.null line) && B.head line == hash
      csv' = B.intercalate (B.singleton lf) $ filter (not . comment) $ B.split lf csv
      lf   = 10 :: Word8
      hash = 35 :: Word8
  case decodeByName csv' of
    Left  err     -> error err
    Right (_,dat) -> return (V.toList dat)
