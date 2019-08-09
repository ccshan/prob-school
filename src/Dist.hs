{-# LANGUAGE GeneralizedNewtypeDeriving, DefaultSignatures #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Dist where

import Control.Monad.Fix
import System.Random (StdGen, getStdRandom, Random (random))
import qualified Data.Number.LogFloat as LF
import Data.Number.Erf (invnormcdf)
import Data.List (scanl', findIndex)
import Data.Maybe (catMaybes)
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Foldable (asum)
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (ReaderT), runReaderT)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Control.Monad.Trans.State.Strict (State, runState, state, StateT (StateT), runStateT, modify')
import Text.Show.Pretty (Value (Con), PrettyVal (prettyVal)) -- dumpDoc is handy

-- Represent weights by their log, so as to avoid floating-point underflow

type Prob = LF.LogFloat

fromProb :: Prob -> Double
fromProb = LF.fromLogFloat

prob :: Double -> Prob
prob = LF.logFloat

exp_ :: Double -> Prob
exp_ = LF.logToLogFloat

log_ :: Prob -> Double
log_ = LF.logFromLogFloat

instance PrettyVal LF.LogFloat where
  prettyVal lf
    | not (isInfinite precise) && isInfinite friendly
    = Con "exp_" [prettyVal precise]
    | otherwise
    = prettyVal friendly
    where precise  = LF.logFromLogFloat lf
          friendly = LF.fromLogFloat    lf

--------------------------------------------------------------------------------
-- A weighted set is a linear combination of outcomes

class Monad m => MonadWeightedSet m where
  {-# MINIMAL superpose | (reject, weight) #-}
  superpose :: [(Prob, m a)] -> m a

  default superpose :: MonadDist m => [(Prob, m a)] -> m a
  superpose []       = reject
  superpose [(p, m)] = weight p m
  superpose pms      = do -- inverse transform method
    r <- stdUniform
    let ps    :: [Prob]
        ps    = map fst pms
        mx    :: Prob
        mx    = maximum ps
        qs    :: [Double]
        qs    = scanl' (\acc p -> acc + fromProb (p / mx))
                       (fromProb (head ps / mx))
                       (tail ps)
        total :: Double
        total = last qs
    case findIndex (>= r * total) qs of
      Nothing -> reject
      Just i  -> weight (mx * prob total) (snd (pms !! i))

  reject :: m a
  reject = superpose []

  weight :: Prob -> m a -> m a
  weight p m = superpose [(p, m)]

fromList :: MonadWeightedSet m => [(a, Prob)] -> m a
fromList aps = superpose [ (p, return a) | (a, p) <- aps ]

observe :: MonadWeightedSet m => Bool -> m ()
observe True  = return ()
observe False = reject

-- Scoring is scalar multiplication
-- and thus subsumed by linear combination

factor :: MonadWeightedSet m => Prob -> m ()
factor p = weight p (return ())

density :: MonadWeightedSet m => (a -> Prob) -> m a -> m a
density p m = do a <- m
                 weight (p a) (return a)

-- Choosing among a finite number of alternatives is addition
-- and thus also subsumed by linear combination

coinFlip :: MonadWeightedSet m => Double -> m Bool
coinFlip p = fromList [(True, prob p), (False, prob (1-p))]

discreteUniform :: MonadWeightedSet m => Int -> m Int
discreteUniform n = fromList [ (i, recip (fromIntegral n)) | i <- [0..n-1] ]

die :: MonadWeightedSet m => m Int
die = fmap (1+) (discreteUniform 6)

dice :: MonadWeightedSet m => Int -> m Int
dice 0 = return 0
dice n = do r <- dice (n-1)
            d <- die
            return (r+d)

-- Inference on a weighted set by exhaustive enumeration

newtype Enumerate a = Enumerate (StateT Prob [] a)
  deriving (Functor, Applicative, Monad)

instance MonadWeightedSet Enumerate where
  superpose pms = Enumerate (asum [ modify' (p *) >> m | (p, Enumerate m) <- pms ])

runEnumerate :: Enumerate a -> [(a, Prob)]
runEnumerate (Enumerate m) = runStateT m 1

tabulate :: Ord a => [(a, Prob)] -> [(a, Prob)]
tabulate t = Map.toList (fmap sum' table)
  where table = Map.fromListWith (++) [ (a, [p]) | (a, p) <- t ]
        sum' []  = 0
        sum' [x] = x
        sum' xs  = LF.sum xs

tabEnumerate :: Ord a => Enumerate a -> [(a, Prob)]
tabEnumerate = tabulate . runEnumerate

-- Dynamic programming (bucket elimination) for exponential speedup

dice' :: Int -> Enumerate Int
dice' 0 = return 0
dice' n = do r <- fromList (tabEnumerate (dice' (n-1)))
             d <- die
             return (r+d)

--------------------------------------------------------------------------------
-- A distribution is a weighted set that allows continuous choice

class MonadWeightedSet m => MonadDist m where
  stdUniform :: m Double

-- Lots of distributions can be expressed by transforming stdUniform

stdNormal :: MonadDist m => m Double
stdNormal = fmap invnormcdf stdUniform -- inverse transform method

normal :: MonadDist m => Double -> Double -> m Double
normal loc scale = fmap (\x -> loc + scale * x) stdNormal

dnorm :: Double -> Double -> Double -> Prob
dnorm loc scale outcome =
  exp_ (- ((outcome - loc) / scale) ^ (2::Int) / 2)
  / prob scale
  / prob (sqrt (2 * pi))

lebesgue :: MonadDist m => m Double
lebesgue = density (recip . dnorm 0 10) (normal 0 10)

stdExponential :: MonadDist m => m Double
stdExponential = fmap (negate . log) stdUniform

flatDirichlet :: MonadDist m => Int -> m [Double]
flatDirichlet n = fmap normalize (replicateM n stdExponential)
  where normalize xs = map (/ sum xs) xs

-- Inference on a distribution by importance sampling

newtype Sample a = Sample (StateT Prob (MaybeT (State StdGen)) a)
  deriving (Functor, Applicative, Monad, MonadFix)

instance MonadWeightedSet Sample where
  reject              = Sample mzero
  weight p (Sample m) = Sample (modify' (p *) >> m)

instance MonadDist Sample where
  stdUniform = Sample (lift (lift (state random)))

runSample :: Sample a -> IO (Maybe (a, Prob))
runSample (Sample m) = getStdRandom (runState (runMaybeT (runStateT m 1)))

tabSample :: Ord a => Int -> Sample a -> IO [(a, Prob)]
tabSample n m = fmap (tabulate . catMaybes) (replicateM n (runSample m))

--------------------------------------------------------------------------------
-- Metropolis-Hastings sampling

newtype MH m a = MH (Address -> ReaderT Database
                                  (StateT (Prob, Database) (MaybeT m)) a)
type Address  = Integer
type Database = Map.Map Address Double

instance Functor m => Functor (MH m) where
  fmap f (MH m) = MH (fmap (fmap f) m)

instance Monad m => Applicative (MH m) where
  pure a = MH (\_ -> pure a)
  (<*>)  = ap

instance Monad m => Monad (MH m) where
  return     = pure
  MH m >>= k = MH (\addr -> do a <- m (2*addr)
                               let MH f = k a
                               f (2*addr+1))

instance MonadDist m => MonadWeightedSet (MH m) where
  reject          = MH (\_ -> mzero)
  weight p (MH m) = MH (\addr -> do lift (modify' (\(p0, db') -> (p * p0, db')))
                                    m addr)

instance MonadDist m => MonadDist (MH m) where
  stdUniform =
    MH (\addr -> ReaderT (\db -> StateT (\(p, db') -> MaybeT (do
      r' <- case Map.lookup addr db of Just r  -> return r
                                       Nothing -> stdUniform
      return (Just (r', (p, Map.insertWith undefined addr r' db')))))))

runMH :: MH m a -> Database -> m (Maybe (a, (Prob, Database)))
runMH (MH m) db = runMaybeT (runStateT (runReaderT (m 1) db) (1, Map.empty))

initMH :: MonadWeightedSet m => MH m a -> m (a, (Prob, Database))
initMH m = do result <- runMH m Map.empty
              case result of Nothing      -> initMH m
                             Just success -> return success

stepMH :: MonadDist m => MH m a -> (a, (Prob, Database))
                              -> m (a, (Prob, Database))
stepMH m old@(_,(p,db)) = do
  let dbSizeRecip   = recip (fromIntegral (Map.size db))
  addr <- fromList [ (addr, dbSizeRecip) | addr <- Map.keys db ]
  let f Nothing     = error ("picked impossible address " ++ show addr)
      f (Just from) = Compose $ do to <- proposalDist from
                                   return ((from, to), Just to)
  ((from, to), db') <- getCompose (Map.alterF f addr db)
  result <- runMH m db'
  case result of
    Nothing -> return old
    Just new@(_,(p',db'')) -> do
      let db''SizeRecip = recip (fromIntegral (Map.size db''))
          -- double-check: agrees with Wingate et al. and disagrees with Staton
          ratio = p' * proposalDensity to from * db''SizeRecip
                / (p * proposalDensity from to * dbSizeRecip)
      if ratio < 1 then fromList [(new, ratio), (old, 1-ratio)]
                   else return new

{-
proposalDist :: MonadDist m => Double -> m Double
proposalDist _ = stdUniform

proposalDensity :: Double -> Double -> Prob
proposalDensity from to | 0 <= to && to <= 1 = 1
                        | otherwise = 0
-}

proposalDist :: MonadDist m => Double -> m Double
proposalDist from = liftM (\u -> 2 * proposalDelta * (u - 0.5) + center)
                          stdUniform
  where center = max proposalDelta (min (1-proposalDelta) from)

proposalDensity :: Double -> Double -> Prob
proposalDensity from to
  | center-proposalDelta <= to &&
    to <= center+proposalDelta
  = 0.5 / prob proposalDelta
  | otherwise
  = 0
  where center = max proposalDelta (min (1-proposalDelta) from)

proposalDelta :: Double
proposalDelta = 0.05 -- jumpiness of proposals; must be a number in (0,0.5]

tabMH :: Int -> MH Sample a -> IO [a]
tabMH steps m = do Just (as,p) <- runSample (initMH m >>= loop steps)
                   if p < 0.9 || p > 1.1 then error (show p) else return as
  where loop 0     (a,_) = return [a]
        loop n old@(a,_) = do new <- stepMH m old
                              as  <- loop (n-1) new
                              return (a:as)
