{-# LANGUAGE RecursiveDo #-}
module Infinite where

import Dist

flips :: MonadDist m => m [Bool]
flips = mdo x <- coinFlip 0.5
            xs <- flips
            return (x:xs)

useFlips :: MonadDist m => m [Bool]
useFlips = mdo xs <- flips
               let three = take 3 xs
               observe (head three)
               return (tail three)
