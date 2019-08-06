{-# LANGUAGE FlexibleInstances, TupleSections #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Plot where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Utils (isValidNumber)
import Graphics.Rendering.Chart.Backend.Cairo (renderableToFile, FileOptions)
import Control.Lens ((.~), (%~), _1)
import Control.Applicative (liftA2)
import Control.Monad (forM_)
import Control.Arrow (first)
import Data.Default (Default (def))
import Data.Array (Ix, accumArray, assocs)
import Data.Maybe (catMaybes)
import Data.List (mapAccumL)
import Data.Colour (dissolve, opaque, black, withOpacity)
import Data.Function (on)
import qualified Data.Map.Strict as Map
import qualified Data.Number.LogFloat as LF
import Numeric (showEFloat, showFFloat)

instance Default (PlotHidden x y) where def = PlotHidden [] []

instance (PlotValue x, PlotValue y) => ToRenderable [Plot x y] where
  -- quick-and-dirty plotting using the default layout
  toRenderable plots = toRenderable (layout_plots .~ plots $ def)

renderToFile :: ToRenderable a => FileOptions -> FilePath -> a -> IO ()
renderToFile fo path r = do
  _ <- renderableToFile fo path (toRenderable r)
  return ()

--------------------------------------------------------------------------------
-- Integrate LogFloat with Chart

showDs :: (RealFloat d)
       => Int --add argument to code from Graphics.Rendering.Chart.Axis.Floating
       -> [d] -> [String]
showDs expon xs = case showWithoutOffset expon xs of
  (s0:others)
    | anyEqualNeighbor s0 others -> map addShownOffset $ showWithoutOffset expon (map (\x -> x - offset) xs)
  s -> s
  where
    anyEqualNeighbor z0 (z1:others)
      | z0 == z1 = True
      | otherwise = anyEqualNeighbor z1 others
    anyEqualNeighbor _ [] = False

    -- Use the min for offset. Another good choice could be the mean.
    offset = minimum xs
    shownOffset = case showWithoutOffset expon [offset] of
      [r] -> r
      rs -> error $ "showDs: shownOffset expected 1 element, got " ++ show (length rs)

    addShownOffset :: String -> String
    addShownOffset ('-':x) = shownOffset ++ " - " ++ x
    addShownOffset x = shownOffset ++ " + " ++ x

showWithoutOffset :: RealFloat d => Int -> [d] -> [String]
showWithoutOffset expon xs
  | useScientificNotation = map (\x -> showEFloat' expon (Just 1) x) xs
  | otherwise = map showD xs
  where
    -- use scientific notation if max value is too big or too small
    useScientificNotation = abs (logBase 10 maxAbs + fromIntegral expon) >= 6
    maxAbs = maximum (map abs xs)

-- | Changes the behavior of showEFloat to drop more than one trailings 0.
-- Instead of 1.000e4 you get 1.0e4
showEFloat' :: RealFloat d => Int -> Maybe Int -> d -> String
showEFloat' _ _ 0 = "0"
showEFloat' expon mdigits x =
  reverse (cleanup1 (reverse mant)) ++ add expo
  where
    shown0 = showEFloat mdigits x ""
    (mant, expo) = break ('e'==) shown0

    -- get rid of redundant 0s before the '.'
    cleanup1 :: String -> String
    cleanup1 ('0':ys@('0':_)) = cleanup1 ys
    cleanup1 y = y

    -- add expon to expo
    add ('e':n) = 'e' : show (read n + expon)
    add y = y

showD :: (RealFloat d) => d -> String
showD x = case reverse $ showFFloat Nothing x "" of
            '0':'.':r -> reverse r
            r         -> reverse r

data LogFloatAxisParams = LogFloatAxisParams {
  _lfa_labelf  :: Int -> [Double] -> [String],
  _lfa_nLabels :: Int,
  _lfa_nTicks  :: Int
}

instance Default LogFloatAxisParams where
  def = LogFloatAxisParams
    { _lfa_labelf  = showDs
    , _lfa_nLabels = 5
    , _lfa_nTicks  = 50
    }

-- | Generate a linear axis with the specified bounds
scaledLogFloatAxis :: LogFloatAxisParams
                   -> (LF.LogFloat, LF.LogFloat) -> AxisFn LF.LogFloat
scaledLogFloatAxis lfap (minV,maxV) ps0 = AxisData
  { _axis_visibility= _axis_visibility axisData
  , _axis_viewport  = \range -> _axis_viewport axisData range . divide
  , _axis_tropweiv  = \range -> multiply . _axis_tropweiv axisData range
  , _axis_ticks     = map      (_1 %~ multiply)  (_axis_ticks  axisData)
  , _axis_labels    = map (map (_1 %~ multiply)) (_axis_labels axisData)
  , _axis_grid      = map             multiply   (_axis_grid   axisData) }
  where
    expon :: Int
    expon
      | not (null ps0) && maxV > 0 = round (LF.logFromLogFloat maxV / log 10)
      | otherwise                  = 0
    factor :: LF.LogFloat
    factor = LF.pow 10 (fromIntegral expon)
    divide :: LF.LogFloat -> Double
    divide lf = LF.fromLogFloat (lf/factor)
    multiply :: Double -> LF.LogFloat
    multiply db = LF.logFloat db * factor
    lap :: LinearAxisParams Double
    lap = LinearAxisParams
      { _la_labelf  = _lfa_labelf  lfap expon
      , _la_nLabels = _lfa_nLabels lfap
      , _la_nTicks  = _lfa_nTicks  lfap
      }
    axisData :: AxisData Double
    axisData = scaledAxis lap (divide minV, divide maxV) (map divide ps0)

-- | Generate a linear axis automatically, scaled appropriately for the
-- input data.
autoScaledLogFloatAxis :: LogFloatAxisParams -> AxisFn LF.LogFloat
autoScaledLogFloatAxis lfap ps0 = scaledLogFloatAxis lfap rs ps
  where
    ps = filter (isValidNumber . LF.logFromLogFloat) ps0
    rs = (minimum ps, maximum ps)

instance PlotValue LF.LogFloat where
  toValue = LF.fromLogFloat
  fromValue = LF.logFloat
  autoAxis = autoScaledLogFloatAxis def

instance BarsPlotValue LF.LogFloat where
  barsReference = 0
  barsAdd = (+)

--------------------------------------------------------------------------------
-- Tools for making histograms and heat maps

newtype Bin a ix = Bin ([a] -> ((ix, ix), a -> Maybe ix, ix -> (a, a, a)))

binIx :: (Ord a, Enum a) => Bin a a
binIx = Bin (\xs ->
  let b = (minimum xs, maximum xs)
  in (b, Just, \x -> (x, x, succ x)))

binFloat :: (RealFloat a) => Int -> Bin a Int
binFloat bins = Bin (\xs ->
  let mn = minimum xs
      mx = maximum xs
      binWidth = (mx - mn) / fromIntegral bins
      bin a | a <= mn         = Just 0
            | a >= mx         = Just (bins-1)
            | isValidNumber a = Just (max 0 $ min (bins-1) $
                                      floor $ (a - mn) / binWidth)
            | otherwise       = Nothing
      unbin ix = ( mn + binWidth * (fromIntegral ix      )
                 , mn + binWidth * (fromIntegral ix + 0.5)
                 , mn + binWidth * (fromIntegral ix + 1  ) )
  in ((0, bins-1), bin, unbin))

binPair :: Bin a ix -> Bin b jx -> Bin (a,b) (ix,jx)
binPair (Bin f) (Bin g) = Bin (\xys ->
  let ((ix1, ix2), binx, unbinx) = f (map fst xys)
      ((jx1, jx2), biny, unbiny) = g (map snd xys)
      bin   (x,y) = liftA2 (,) (binx x) (biny y)
      unbin (i,j) = case (unbinx i, unbiny j) of
                      ((x1,x,x2), (y1,y,y2)) -> ((x1,y1), (x,y), (x2,y2))
  in (((ix1, jx1), (ix2, jx2)), bin, unbin))

tally :: (BarsPlotValue w, Ix ix)
      => Bin a ix -> [(a, w)] -> [((a, a, a), w)]
tally (Bin f) aws =
  let (bounds, bin, unbin) = f (map fst aws)
      binned = [ fmap (, w) (bin a) | (a, w) <- aws ]
      arr = accumArray barsAdd barsReference bounds (catMaybes binned)
  in [ (unbin ix, w) | (ix, w) <- assocs arr ]

plotHistogram :: (PlotValue x, Show x, Ix ix, BarsPlotValue w)
              => Bin x ix -> [(x, w)] -> [Plot x w]
plotHistogram binx xws =
  [plotBars bars, toPlot hidden]
  where bars = plot_bars_values .~ [ (x,[w]) | ((_,x,_),w) <- values ]
             $ plot_bars_spacing .~ BarsFixGap 0 0
             $ def
        hidden = plot_hidden_x_values .~ (values >>= \((x1,_,x2),_) -> [x1,x2])
               $ plot_hidden_y_values .~ [barsReference]
               $ def
        values = tally binx xws

data HeatMap = HeatMap
    { _heatmap_style :: FillStyle }

instance Default HeatMap where
  def = HeatMap
    { _heatmap_style = FillStyleSolid (opaque black) }

plotHeatMap :: (PlotValue x, Show x, Ix ix,
                PlotValue y, Show y, Ix iy,
                BarsPlotValue w)
            => HeatMap -> (Bin x ix, Bin y iy) -> [((x,y), w)] -> [Plot x y]
plotHeatMap HeatMap{_heatmap_style=fill} (binx,biny) xyws =
  [Plot { _plot_render     = renderHeatMap
        , _plot_legend     = []
        , _plot_all_points = (xs, ys) }]
  where renderHeatMap pmap = forM_ values (\((xy1,_,xy2),w) ->
          withFillStyle (fill_color %~ dissolve (heatAxis w) $ fill) $
            alignFillPath (rectPath (Rect (mapXY pmap xy1) (mapXY pmap xy2)))
            >>= fillPath)
        values = tally (binPair binx biny) xyws
        heatAxis = _axis_viewport (autoAxis (map snd values)) (0,1)
        xs = values >>= \(((x1,_),_,(x2,_)),_) -> [x1,x2]
        ys = values >>= \(((_,y1),_,(_,y2)),_) -> [y1,y2]

newtype OnFst ab = OnFst {unOnFst :: ab}
newtype OnSnd ab = OnSnd {unOnSnd :: ab}
instance Eq  a => Eq  (OnFst (a,b)) where (==)    = (==)    `on` (fst . unOnFst)
instance Ord a => Ord (OnFst (a,b)) where compare = compare `on` (fst . unOnFst)
instance Eq  b => Eq  (OnSnd (a,b)) where (==)    = (==)    `on` (snd . unOnSnd)
instance Ord b => Ord (OnSnd (a,b)) where compare = compare `on` (snd . unOnSnd)

removeOutliers :: (Ord a, Ord w, Fractional w) => [(a,w)] -> [(a,w)]
removeOutliers aws = Map.assocs m2
  where m = Map.fromListWith (+) aws
        margin = sum (Map.elems m) / 20000
        bound = map fst . dropWhile ((< margin) . snd) . snd . mapAccumL f 0
          where f acc (a,w) = (acc', (a,acc')) where acc' = acc + w
        m1 = case bound (Map.toAscList m) of
          []  -> Map.empty
          a:_ -> Map.dropWhileAntitone (< a) m
        m2 = case bound (Map.toDescList m1) of
          []  -> Map.empty
          a:_ -> Map.takeWhileAntitone (<= a) m1

removeOutlierPairs :: (Ord a, Ord b, Ord w, Fractional w) => [((a,b),w)] -> [((a,b),w)]
removeOutlierPairs = map (first unOnFst) . removeOutliers . map (first OnFst)
                   . map (first unOnSnd) . removeOutliers . map (first OnSnd)

--------------------------------------------------------------------------------
-- Tools for plotting points

data Trajectory = Trajectory
    { _trajectory_lines  :: Bool
    , _trajectory_points :: Bool }

instance Default Trajectory where
  def = Trajectory
    { _trajectory_lines  = False
    , _trajectory_points = True }

plotTrajectories :: (PlotValue x, PlotValue y)
                 => Trajectory -> [[(x, y)]] -> [Plot x y]
plotTrajectories traj xyss =
  [ toPlot $ plot_lines_values .~ [xys]
           $ plot_lines_style .~ solidLine width (dissolve alpha color)
           $ def
  | _trajectory_lines traj
  , (xys, color) <- zip xyss defaultColorSeq ] ++
  [ toPlot $ plot_points_values .~ concat xyss
           $ plot_points_style .~ filledCircles width (black `withOpacity` alpha)
           $ def
  | _trajectory_points traj ]
  where alpha = fromIntegral (n + 1) ** (-0.2)
        width = fromIntegral (n + 1) ** (-0.1) * 3
        n = sum (map length xyss)

plotTrajectory :: (PlotValue x, PlotValue y)
               => Trajectory -> [(x, y)] -> [Plot x y]
plotTrajectory traj xys = plotTrajectories traj [xys]
