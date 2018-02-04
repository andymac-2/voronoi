module Main where

import Voronoi
import qualified Data.Sequence as S
import qualified Data.List as L
import Control.Monad.State

main :: IO ()
main = do
  let 
    points =
      [ (0, 0)
      , (1, 0)
      , (0, 1)
      , (1, 1)
      ] :: [Point2D Integer]
  putStrLn . show . L.sortOn compareByY $ points
  putStrLn . show . voronoi2D $ points