module Day23 (day23) where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Bool
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.List
import Data.Maybe
import System.IO
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

type Point = (Int, Int)

parse :: String -> HashSet Point
parse = HashSet.fromList . 
    concatMap (\(y,xs) -> (map (\(x,_) -> (x,y)) $ filter ((=='#') . snd) $ zip [0..] xs)) .
    zip [0..] .
    lines

doRound :: ([(Point -> Point, [Point -> Point])], HashSet Point) -> ([(Point -> Point, [Point -> Point])], HashSet Point)
doRound (directions, elfLocations) = (cycle directions, enactProposals $ getProposals elfLocations) where
    cycle (x:xs) = xs ++ [x]

    getProposals :: HashSet Point -> HashMap Point [Point]
    getProposals elves = foldl (HashMap.unionWith (++)) HashMap.empty $ 
        map (HashMap.fromList . singleton . f) $ 
        HashSet.toList elves where
        f :: Point -> (Point, [Point])
        f elf
            | allSurroundingsClear = (elf, [elf]) -- No other elf will try and propose moving here!
            | otherwise = fromMaybe (elf, [elf]) $
                listToMaybe $
                map ((,[elf]) . ($ elf) . fst) $
                filter (not . any (flip HashSet.member elves . ($ elf)) . snd) directions where
            allSurroundingsClear = not $ any (flip HashSet.member elves . ($ elf)) $ concatMap snd directions

    enactProposals :: HashMap Point [Point] -> HashSet Point
    enactProposals props = HashSet.fromList $ concatMap f $ HashMap.toList props where
        f :: (Point, [Point]) -> [Point]
        f (next, [elf]) = [next]
        f (next, elves) = elves

score :: HashSet Point -> Int
score pts = (w*h) - HashSet.size pts where
    ptsList = HashSet.toList pts
    (maxX, minX) = (maximum &&& minimum) $ map fst ptsList
    (maxY, minY) = (maximum &&& minimum) $ map snd ptsList
    w = maxX - minX + 1
    h = maxY - minY + 1

directions = [
    (second (subtract 1), [subtract 1 *** subtract 1, id *** subtract 1, (+1) *** subtract 1]),
    (second (+1)        , [subtract 1 *** (+1)      , id *** (+1)      , (+1) *** (+1)      ]),
    (first  (subtract 1), [subtract 1 *** subtract 1, subtract 1 *** id, subtract 1 *** (+1)]),
    (first  (+1)        , [(+1) *** subtract 1      , (+1) *** id      , (+1) *** (+1)      ])
    ]

solution1 = score . snd . (!!10) . iterate doRound . (directions,)
solution2 = (+1) . length . takeWhile (uncurry (/=)) . (zip <$> id <*> tail) . map snd . iterate doRound . (directions,)

day23 = (solution1 &&& solution2) . parse
