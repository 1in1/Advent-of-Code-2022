module Day04 (day04) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.List.Split
import System.IO

contains :: [[Int]] -> Bool
contains [[a, b], [u, v]] = u <= a && b <= v

overlap :: [[Int]] -> Bool
overlap [[a, b], [u, v]] = (u <= b && a <= v) || (a <= v && u <= b)

fullContainment = length . filter ((||) <$> contains <*> contains . reverse)
anyOverlap = length . filter overlap

parseInput = map (map (map read . splitOn "-") . splitOn ",") . lines
day04 = (fullContainment &&& anyOverlap) . parseInput
