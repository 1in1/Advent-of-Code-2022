module Day09 (day09) where

import Control.Arrow
import Control.Applicative
import Control.Monad
import System.IO
import qualified Data.HashSet as S

type Point = (Int, Int)

strToFn :: String -> Point -> Point
strToFn "R" = first (+1)
strToFn "L" = first (subtract 1)
strToFn "U" = second (+1)
strToFn "D" = second (subtract 1)

updateTail :: Point -> Point -> Point
updateTail (newHx, newHy) (oldTx, oldTy)
    | abs xDiff == 2 && yDiff == 0 = ((xDiff `div` 2) + oldTx, oldTy)
    | abs yDiff == 2 && xDiff == 0 = (oldTx, (yDiff `div` 2) + oldTy)
    | (abs xDiff <= 1) && (abs yDiff <= 1) = (oldTx, oldTy)
    | otherwise = (signum xDiff + oldTx, signum yDiff + oldTy)
    where
        xDiff = newHx - oldTx
        yDiff = newHy - oldTy

process :: S.HashSet Point -> [Point] -> [String] -> S.HashSet Point
process s _ [] = s
process s currKnots (x:xs) = process updatedHashSet newKnots xs where
    newKnots = (scanl updateTail <$> strToFn x . head <*> tail) currKnots
    updatedHashSet = S.insert (last newKnots) s

solution1 = S.size . process S.empty [(0, 0), (0, 0)]
solution2 = S.size . process S.empty (replicate 10 (0, 0))

parse = concatMap (uncurry replicate . first read . ((!!0) &&& (!!1)) . reverse . words)
day09 = (solution1 &&& solution2) . parse . lines
