{-# LANGUAGE TupleSections #-}

module Day12 (day12) where

import Control.Arrow
import Control.Monad
import Data.Bool
import Data.IntSet (IntSet)
import Data.List
import Data.Tuple
import System.IO
import qualified Data.IntSet as IntSet

type Point = (Int, Int)
pointAccessor :: Point -> [[Char]] -> Int
pointAccessor (x, y) = fromEnum . (!!x) . (!!y)

charLocations :: Char -> [[Char]] -> [(Int, Int)]
charLocations c = concatMap (map swap . uncurry zip . (repeat *** elemIndices c)) . zip [0..]

firstCharLocation :: Char -> [[Char]] -> (Int, Int)
firstCharLocation = (head .) . charLocations

stripBeginAndEnd :: [[Char]] -> [[Char]]
stripBeginAndEnd = map (map swap) where
    swap 'S' = 'a'
    swap 'E' = 'z'
    swap other = other

-- Hash for use of IntSet
k :: Point -> Int
k (x,y)
    | x' >= y' = x'*x' + x' + y'
    | otherwise = x' + y'*y' where
        x' = bool (-2*x - 1) (2*x) (x >= 0)
        y' = bool (-2*y - 1) (2*y) (y >= 0)

bfs :: IntSet -> [[Char]] -> Point -> [(Int, Point)] -> Int
bfs visited heightMap to (next:toVisit)
    | snd next == to = fst next
    | IntSet.member kNext visited = bfs visited heightMap to toVisit
    | otherwise = bfs (IntSet.insert kNext visited) heightMap to (toVisit ++ newPoints) where
        kNext = k $ snd next
        newPoints = map (1 + fst next,) $ 
            filter possible $ 
            map ($ snd next) [first (+1), first (subtract 1), second (+1), second (subtract 1)]
        possible (x, y) =
            x >= 0 && x < length (head heightMap) &&
            y >= 0 && y < length heightMap && 
            pointAccessor (x,y) heightMap - pointAccessor (snd next) heightMap <= 1 &&
            IntSet.notMember (k (x,y)) visited

solution1 = bfs IntSet.empty <$> stripBeginAndEnd <*> firstCharLocation 'E' <*> return . (0,) . firstCharLocation 'S'
solution2 = bfs IntSet.empty <$> stripBeginAndEnd <*> firstCharLocation 'E' <*> zip (repeat 0) . charLocations 'a'

day12 = (solution1 &&& solution2) . lines
