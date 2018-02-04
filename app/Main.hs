module Main where

import Voronoi
import qualified Data.Sequence as S
import Control.Monad.State

main :: IO ()
main = do
    let points =
            [ Point2D (0, 0)
            , Point2D (10, 5)
            , Point2D (20, 10)
            , Point2D (30, 15)
            , Point2D (40, 20)
            ] :: [Point2D Integer]
    putStrLn . show . voronoi2D $ points