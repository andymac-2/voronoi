module Main where

import Voronoi
import qualified Data.Sequence as S
import System.Random
import Data.Ratio

main :: IO ()
main = do
  let 
    rng = mkStdGen 1242332 
    points = take 40 . makePointList $ rng
    -- points :: [Point2D (Ratio Integer)]
    -- points = [(0, 0), (300, 0),
      -- (10, 400), (300, 378), (320, 158),
      -- (0, 100), (200, 100), (400, 100), (600, 100) 
      --(234, 320), (213, 150), (56, 389), (20, 360),
      --(278, 30),
      --(478, 246), (587, 400)
      -- ]
  let 
    vstate = voronoi2D points
    roundPoint (x, y) = (round x, round y)  
  putStrLn . outputSVG (map roundPoint points) $ vstate
  --putStrLn . show . voronoi2D $ points
  

makePointList :: StdGen -> [Point2D (Ratio Integer)]
makePointList rng = 
  let
    (x, rng') = next rng
    (y, rng'') = next rng'
    
    x' = toRational (x `rem` 500)
    y' = toRational (y `rem` 500)
  in
    (x', y') : makePointList rng''
    