module Day18 (day18) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Bool
import Data.HashSet (HashSet)
import Data.List
import Data.List.Split
import Data.Maybe
import System.IO
import qualified Data.HashSet as HashSet

type Point = (Int, (Int, Int))

parse :: String -> [Point]
parse = map (((!!0) &&& ((!!1) &&& (!!2))) . map read . splitOn ",") . lines

neighbouring :: Point -> [Point]
neighbouring p = map ($p)
    [((+1) *** (id *** id)), (subtract 1 *** (id *** id)),
     (id *** ((+1) *** id)), (id *** (subtract 1 *** id)),
     (id *** (id *** (+1))), (id *** (id *** subtract 1))]

-- We add one if we haven't seen it before, and subtract one if we have
-- The subtraction accounts for the previous addition
solution1 :: HashSet Point -> [Point] -> Int
solution1 _ [] = 0
solution1 seen (x:xs) = score + solution1 (HashSet.insert x seen) xs where
    score = sum $
        map (bool 1 (-1) . (`HashSet.member` seen)) $ neighbouring x

solution2 :: [Point] -> Int
solution2 points = bfs HashSet.empty [(lowerBoundX, (lowerBoundY, lowerBoundZ))] where
    pointSet = HashSet.fromList points
    upperBoundX = (+1) $ maximum $ map fst points
    upperBoundY = (+1) $ maximum $ map (fst . snd) points
    upperBoundZ = (+1) $ maximum $ map (snd . snd) points
    lowerBoundX = subtract 1 $ minimum $ map fst points
    lowerBoundY = subtract 1 $ minimum $ map (fst . snd) points
    lowerBoundZ = subtract 1 $ minimum $ map (snd . snd) points

    withinBounds :: Point -> Bool
    withinBounds (x,(y,z)) = and [
        x >= lowerBoundX, x <= upperBoundX,
        y >= lowerBoundY, y <= upperBoundY,
        z >= lowerBoundZ, z <= upperBoundZ
        ]

    bfs :: HashSet Point -> [Point] -> Int
    bfs visited [] = 0
    bfs visited (p:ps) 
        | HashSet.member p visited = bfs visited ps
        | otherwise = score + bfs (HashSet.insert p visited) (ps++additional) where
        neighbours = 
            filter withinBounds $
            neighbouring p
        emptyNeighbours = filter (not . (`HashSet.member` pointSet)) neighbours
        additional = filter (not . (`HashSet.member` visited)) emptyNeighbours
        score = length $ filter (`HashSet.member` pointSet) neighbours

day18 = (solution1 HashSet.empty &&& solution2) . parse
