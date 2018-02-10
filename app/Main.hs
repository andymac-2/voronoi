module Main where

import Voronoi
import qualified Data.Sequence as S
import System.Random
import Data.Ratio

main :: IO ()
main = do
  let 
    rng = mkStdGen 1242332 
    points = take 301 . makePointList $ rng
    -- points :: [Point2D Integer]
    -- points = [(0, 0), (300, 0),
      -- (10, 400), (300, 378), (320, 158),
      -- (0, 100), (200, 100), (400, 100), (600, 100) 
      --(234, 320), (213, 150), (56, 389), (20, 360),
      --(278, 30),
      --(478, 246), (587, 400)
      -- ]
  let 
    vstate = voronoi2D points
  putStrLn . outputSVG points $ vstate
  --putStrLn . show . voronoi2D $ points
  

makePointList :: StdGen -> [Point2D Integer]
makePointList rng = 
  let
    (x, rng') = next rng
    (y, rng'') = next rng'
    
    x' = fromIntegral x `rem` 500
    y' = fromIntegral y `rem` 500
  in
    (x', y') : makePointList rng''
    
    
outputSVG :: (Show a) => [Point2D a] -> ([Edge a], [Vert a]) -> String
outputSVG p (e, v) = 
  let
    svgLine (_, ((x1, y1): (x2, y2): _)) text = 
      "<line x1='" ++ (show x1) ++ "'  y1='" ++ (show y1) ++ "' x2='" ++
      (show x2) ++ "'   y2='" ++ (show y2) ++ "' style='stroke:#000000;'/>" ++ text
    svgLine _ text = text
    
    svgPoints (x, y) text = 
      "<circle cx='" ++ (show x) ++ "' cy='" ++ (show y) ++ "' r='3'/>" ++ text
      
    svgVertices ((x, y), _) text =
      "<circle cx='" ++ (show x) ++ "' cy='" ++ (show y) ++ "' r='2'/>" ++ text
    
    string = "<svg xmlns='http://www.w3.org/2000/svg'  xmlns:xlink='http://www.w3.org/1999/xlink'>" ++ (foldr svgVertices "" v) ++ (foldr svgPoints "" p) ++ (foldr svgLine "</svg>" e)
  in
    string