{-# OPTIONS -Wall #-}
module TopicModel where

import Dist
import Text.Show.Pretty (dumpDoc)
import Control.Monad (replicateM_, replicateM, forM)

topicModel :: MonadDist m
           => Int -> Int -> [Int] -> m ([Prob], [[Prob]], [(Int, [Int])])
topicModel nTopics nWords docLengths = do
  topicDist <- flatDirichlet' nTopics
  wordDists <- replicateM nTopics (flatDirichlet' nWords)
  docs <- forM docLengths (\len -> do
    (topic, wordDist) <- fromList (zip (zip [0..] wordDists) topicDist)
    doc <- replicateM len (fromList (zip [0..] wordDist))
    return (topic, doc))
  return (topicDist, wordDists, docs)
  where flatDirichlet' = fmap (map prob) . flatDirichlet

main :: IO ()
main = replicateM_ 3 (do Just (outcome,_) <- runSample (topicModel 2 3 [4,5,4])
                         print (dumpDoc outcome))
