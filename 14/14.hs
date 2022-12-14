import Control.Arrow
import Control.Monad
import Data.Bool
import Data.IntSet (IntSet)
import Data.List
import Data.List.Split
import Data.Maybe
import System.IO
import qualified Data.IntSet as IntSet

type Point = (Int, Int)

k :: Point -> Int
k (x,y)
    | x' >= y' = x'*x' + x' + y'
    | otherwise = x' + y'*y' where
        x' = bool (-2*x - 1) (2*x) (x >= 0)
        y' = bool (-2*y - 1) (2*y) (y >= 0)

parse :: String -> [[Point]]
parse = map (map ((read . (!!0) &&& read . (!!1)) . splitOn "," . head) . chunksOf 2 . words) . lines

rockLocations :: [[Point]] -> [Point]
rockLocations = concatMap (concatMap f . (zip <$> id <*> tail)) where
    f ((x,y), (x',y'))
        | x==x' = map (x,) [y, y + signum (y'-y)..y']
        | y==y' = map (,y) [x, x + signum (x'-x)..x']

locationsAsSet :: [Point] -> IntSet
locationsAsSet = foldl (flip IntSet.insert) IntSet.empty . map k

dropSand :: (Point -> Bool) -> IntSet -> Maybe IntSet
dropSand testBlockageFree blockages = scanPositions positionsTaken where
    scanPositions :: [Maybe Point] -> Maybe IntSet
    scanPositions ((Just x):x':xs)
        | isNothing x' = Just (IntSet.insert (k x) blockages)
        | isNothing $ IntSet.lookupGE (k x - 100000000) blockages = Nothing
        | otherwise = scanPositions (x':xs)

    positionsTaken = iterate (nextPosition =<<) (Just (500,0))

    nextPosition :: Point -> Maybe Point
    nextPosition = find ((&&) <$> flip IntSet.notMember blockages . k <*> testBlockageFree) .
        flip map [second (+1), subtract 1 *** (+1), (+1) *** (+1)] . flip id

getFloorTest :: [Point] -> Point -> Bool
getFloorTest = (. snd) . (>) . (+2) . maximum . map snd


solution1 = length . tail . takeWhile isJust . 
    iterate (dropSand (const True) =<<) . Just . locationsAsSet
solution2 = length . takeWhile (maybe False (IntSet.notMember (k (500,0)))) . 
    (iterate <$> (=<<) . dropSand . getFloorTest <*> Just . locationsAsSet)

main = print . (solution1 &&& solution2) . rockLocations . parse =<< readFile "input"
