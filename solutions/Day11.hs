module Day11 (day11) where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Bool
import Data.Foldable (toList)
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Sequence (Seq)
import Data.String.Utils
import System.IO
import qualified Data.Sequence as Seq


data Monkey = Monkey { 
    op' :: Int -> Int, 
    next :: Int -> Int, 
    divN :: Int,
    items :: Seq Int,
    interactionCount :: Int
    }

readOperation :: [String] -> Int -> Int
readOperation = uncurry (.) . 
        (readOp *** (uncurry (&&&) . over both (maybe id const . maybeRead))) . 
        ((!!1) &&& ((!!0) &&& (!!2))) .
        words .
        last .
        splitOn "=" .
        (!!2) where
    
    readOp :: String -> (Int, Int) -> Int
    readOp "+" = uncurry (+)
    readOp "*" = uncurry (*)

readNext :: [String] -> Int -> Int
readNext xs = bool falseCase trueCase . (== 0) . (`mod` n) where
    [n, trueCase, falseCase] = map (read . head . words . filter (`elem` "0123456789 ")) $ drop 3 xs

readDivN :: [String] -> Int
readDivN = read . last . words . (!!3)

readStartingItems :: [String] -> Seq Int
readStartingItems = Seq.fromList . map read . words . filter (`elem` "0123456789 ") . (!!1)

readMonkey :: [String] -> Monkey
readMonkey = Monkey <$> readOperation <*> readNext <*> readDivN <*> readStartingItems <*> const 0

readMonkeys :: String -> Seq Monkey
readMonkeys = Seq.fromList . map (readMonkey . lines) . splitOn "\n\n"


foldEndosIgnoreIdx :: (a -> a) -> b -> (a -> a) -> (a -> a)
foldEndosIgnoreIdx = const . flip (.)

processMonkey :: (Int -> Int) -> Int -> Monkey -> Seq Monkey -> Seq Monkey
processMonkey worryCtl ownIdx monkey = cleanSelf . foldedEffects where
    foldedEffects = Seq.foldlWithIndex foldEndosIgnoreIdx id (Seq.mapWithIndex (const fFromItem) $ items monkey)

    fFromItem :: Int -> Seq Monkey -> Seq Monkey
    fFromItem n = Seq.adjust g idx where
        updatedVal = worryCtl $ op' monkey n
        idx = next monkey updatedVal
        g m = m { items = items m Seq.|> updatedVal } -- There must be a lens way...

    cleanSelf :: Seq Monkey -> Seq Monkey
    cleanSelf = Seq.adjust g ownIdx where
        g m = m { items = Seq.empty, interactionCount = interactionCount m + Seq.length (items m) }

doRound :: (Int -> Int) -> Seq Monkey -> Seq Monkey
doRound worryCtl ms = doRound' [0..(Seq.length ms -1)] ms where
    doRound' [] = id
    doRound' (n:ns) = doRound' ns . (processMonkey worryCtl n <$> (`Seq.index` n) <*> id)

-- Alternatively as a fold - slightly harder to read
-- doRound worryCtl ms = foldl (flip $ (\n -> processMonkey worryCtl n <$> (`Seq.index` n) <*> id)) ms [0..(Seq.length ms - 1)] 

monkeyBusiness :: Seq Monkey -> Int
monkeyBusiness = product . take 2 . sortBy (flip compare) . map interactionCount . toList

solution1 = monkeyBusiness . (!!20) . iterate (doRound (`div` 3))
solution2 = monkeyBusiness . (!!10000) . (iterate <$> (doRound . flip mod . Seq.foldrWithIndex (const $ lcm . divN) 1) <*> id)

day11 = (solution1 &&& solution2) . readMonkeys
