module Day06 (day06) where

import Control.Arrow
import Control.Monad
import Data.List
import System.IO

solve :: Int -> Int -> [Char] -> Int
solve m n xs | (uncurry (==) . (nub &&& id) . take m) xs = n
             | otherwise = (solve m (n+1) . tail) xs

-- Alternative
solve' m = (Just (+m) <*>) . findIndex (uncurry (==) . (nub &&& id) . take m) . iterate tail

day06 = (solve 4 4 &&& solve 14 14)
