module Day17 (day17) where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Bool
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable
import Data.List
import Data.List.Split
import Data.Maybe
import System.IO
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

type Point = (Int, Int)
data Piece = HLine | Plus | Corner | VLine | Square deriving (Enum, Eq, Show)
nextPiece :: Piece -> Piece
nextPiece = toEnum . (`mod` 5) . (+1) . fromEnum

-- We track the bottom left corner of each piece
pointsRelativeToBasepoint :: Piece -> [Point]
pointsRelativeToBasepoint HLine = [(0,0),(1,0),(2,0),(3,0)]
pointsRelativeToBasepoint Plus = [(1,0), (0,1), (1,1), (2,1), (1,2)]
pointsRelativeToBasepoint Corner = [(0,0), (1,0), (2,0), (2,1), (2,2)]
pointsRelativeToBasepoint VLine = [(0,0),(0,1),(0,2),(0,3)]
pointsRelativeToBasepoint Square = [(0,0), (0,1), (1,0), (1,1)]

constituentPoints :: Piece -> Point -> [Point]
constituentPoints = flip (map . uncurry (***) . ((+) *** (+))) . pointsRelativeToBasepoint

-- Is the given point blocked off?
blockedOff :: HashSet Point -> Int -> Point -> Bool
blockedOff set height (x,y)
    | x < 0 || x >= 7 || y < 0 || y >= height = True
    | otherwise = HashSet.member (x,y) set

dropNext :: (Piece, String, HashSet Point, Int) -> (Piece, String, HashSet Point, Int) 
dropNext (piece, str, currState, height) = (nextPiece piece, remainingJets, newState, newHeight) where
    height' = height+7
    nextInitialPos = (2, height+3)
    (remainingJets, finalPos) = followDrop (str, nextInitialPos)

    newState = HashSet.union currState $
        HashSet.fromList $
        constituentPoints piece finalPos
    newHeight = max height $ 
        (+1) $
        maximum $ 
        map snd $
        constituentPoints piece finalPos

    canDropLower :: Point -> Bool
    canDropLower p = not $ any (blockedOff currState height' . second (subtract 1)) $ constituentPoints piece p
    moveAcross :: Char -> Point -> Point
    moveAcross x p = bool p (first (+ direction x) p) $ canMoveAcross x p where
        canMoveAcross x p = not $ any (blockedOff currState height' . first (+ direction x)) $ constituentPoints piece p
        direction '>' = 1
        direction '<' = - 1
    
    followDrop :: (String, Point) -> (String, Point)
    followDrop (x:xs, p)
        | canDropLower p' = followDrop (xs, second (subtract 1) p')
        | otherwise = (xs, p') where
        p' = moveAcross x p

-- Define a projection onto a smaller space, on which we can compare state at different times
-- Turns out for our data n=20 suffices
projection :: Int -> Int -> HashSet Point -> HashSet Point
projection n h = HashSet.map (second (subtract h)) . HashSet.filter ((>= (h-n)) . snd)

findDupes :: (Eq a, Hashable a) => Int -> [a] -> (Int, Int)
findDupes jump ls = find' HashMap.empty 0 where
    find' seen i
        | HashMap.member (ls!!i) seen = check
        | otherwise = find' (HashMap.insert (ls!!i) [i] seen) (i+jump) where

        check
            | length prev == 2 = (i', i - i')
            | otherwise = find' (HashMap.insert (ls!!i) (i:prev) seen) (i+jump)
        prev = seen HashMap.! (ls!!i)
        i' = head prev

solution1 = (^._4) .
    (!!2022) .
    iterate dropNext .
    (HLine,,HashSet.empty,0)

solution2 s = heightBeforeAndAfter + q*delta where
    m = 1000000000000
    heightBeforeAndAfter = f (offset + r)
    delta = f (loopLength + offset) - f offset
    (q, r) = over both ($ (m-offset, loopLength)) (uncurry div, uncurry mod)

    (offset, loopLength) = findDupes 5 $ 
        map (projection 20 <$> (^._4) <*> (^._3))
        states

    states = iterate dropNext (HLine,s,HashSet.empty,0)
    f :: Int -> Int
    f i = (states!!i)^._4

day17 = (solution1 &&& solution2) . cycle . init
