module Day10 (day10) where

import Control.Arrow
import Data.Bool
import Control.Lens
import Control.Monad
import Data.List
import Data.List.Split
import System.IO

effect :: [String] -> [Int]
effect (x:xs)
    | x == "noop" = [0]
    | otherwise = [0, read $ last xs]

pixel :: (Int, Int) -> Char
pixel = bool '.' '#' . (<= 1) . abs . uncurry (-)

parse :: String -> [Int]
parse = scanl (+) 1 . concatMap (effect . words) . lines

solution1 = show . sum . map (uncurry (*) . head) . chunksOf 40 . drop 19 . flip zip [1..]
solution2 = intercalate "\n" . map (map pixel . flip zip [0..]) . chunksOf 40

day10 = intercalate "\n\n\n" . (^..each) . (solution1 &&& solution2) . parse
