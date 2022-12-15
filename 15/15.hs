import Control.Arrow
import Control.Monad
import Data.Bool
import Data.List
import Data.List.Split
import Data.Maybe
import System.IO

type Point = (Int, Int)
type XRange = (Int, Int)

parse :: String -> [(Point, Point)]
parse = map (
        (((!!0) &&& (!!1)) &&& ((!!2) &&& (!!3))) . 
        map read . 
        words . 
        filter (`elem` "-0123456789 ")
    ) . lines

d :: Point -> Point -> Int
d (x,y) (x',y') = abs(x-x') + abs(y-y')

xRangeAtY :: Bool -> Int -> (Point, Point) -> Maybe XRange
xRangeAtY accountForOwnBeacon fixedY (sensor, beacon) 
    | uncurry (>) range = Nothing
    | otherwise = Just range where
    
    range' (rMin, rMax)
        | (rMin, fixedY) == beacon = (rMin+1, rMax)
        | (rMax, fixedY) == beacon = (rMin, rMax-1)
        | otherwise = (rMin, rMax)

    range = bool range' id accountForOwnBeacon (fst sensor - maxSize, fst sensor + maxSize)
    maxSize = d sensor beacon - dy
    dy = (abs . subtract fixedY . snd) sensor 

mergeXRanges :: [XRange] -> [XRange]
mergeXRanges [] = []
mergeXRanges (x:xs)
    | hasOverlaps x xs = mergeXRanges (map (merge' x) xs)
    | otherwise = x:mergeXRanges xs where
    
    hasOverlaps :: XRange -> [XRange] -> Bool
    hasOverlaps r = any (overlaps r)
    overlaps r = (||) <$> overlapsAbove r <*> flip overlapsAbove r where
        overlapsAbove (a,b) (a',b') = (a < b') && (b+1 >= a')

    merge' (a,b) (a',b')
        | overlaps (a,b) (a',b') = (min a a', max b b')
        | otherwise = (a',b')

intersectXRanges :: XRange -> XRange -> Maybe XRange
intersectXRanges (a,b) (a',b')
    | b < a' || b' < a = Nothing
    | uncurry (>) range = Nothing
    | otherwise = Just range where
    range = (max a a', min b b')

solution1 = sum . map (uncurry subtract) . mergeXRanges . mapMaybe (xRangeAtY True 2000000)
solution2 pairs = tuningFreq $ first ((+1) . snd . minimum) . head $
        filter ((/= [(0,4000000)]) . fst) $
        map ((,) <$> mapMaybe (intersectXRanges (0,4000000)) . mergeXRanges . catMaybes . flip map pairs . xRangeAtY False <*> id)
        yValuesWorthChecking where

    sensors = map (fst &&& uncurry d) pairs
    yValuesWorthChecking = (filter (>=0) . filter (<4000000) . nub . concat) (getBoundaryIntersections <$> sensors <*> sensors)

    -- Intersect the lines u-(y+d+1) = +-(v-x), and u-(y'+d'+1) = +-(v-x')
    -- This happens at:
    --     2u-(y+d+1)-(y'+d'+1) = x-x'  <==>  u = (x-x'+y+y'+d+d'+2)/2
    --     2u-(y+d+1)-(y'+d'+1) = x'-x  <==>  u = (x'-x+y+y'+d+d'+2)/2
    getBoundaryIntersections :: (Point, Int) -> (Point, Int) -> [Int]
    getBoundaryIntersections ((x,y), ds) ((x',y'), ds')
        | (x,y) == (x',y') = []
        | otherwise = [(t+dx) `div` 2, (t-dx) `div` 2] where
        dx = x-x'
        t = y+y'+ds+ds'+2

    tuningFreq :: Point -> Int
    tuningFreq (x,y) = 4000000*x + y

main = print . (solution1 &&& solution2) . parse =<< readFile "input"
