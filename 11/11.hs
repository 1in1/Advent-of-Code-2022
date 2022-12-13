import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Bool
import Data.List
import Data.List.Split
import Data.Map(Map, (!))
import Data.Maybe
import Data.String.Utils
import System.IO
import qualified Data.Map as Map
import Debug.Trace


data Monkey = Monkey { op' :: Int -> Int, next :: Int -> Int, divN :: Int }

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

startingItems :: [String] -> [Int]
startingItems = map read . words . filter (`elem` "0123456789 ") . (!!1)

readMonkey :: [String] -> ([Int], Monkey)
readMonkey = startingItems &&& (Monkey <$> readOperation <*> readNext <*> readDivN)


processMonkey :: (Int -> Int) -> Int -> Monkey -> [Int] -> [[Int]]
processMonkey worryControl n monkey = foldl (flip (\(a, b) -> over (element a) (++[b]))) (replicate n []) . map ((next monkey &&& id) . worryControl . op' monkey)

doRound :: (Int -> Int) -> Int -> [(Int, Monkey)] -> ([[Int]], [Int]) -> ([[Int]], [Int])
doRound worryControl n [] (arrs, counts) = (arrs, counts)
doRound worryControl n ((idx, monkey):xs) (arrs, counts) = doRound worryControl n xs (flip (zipWith (++)) (processMonkey worryControl n monkey $ arrs!!idx) $ (element idx .~ []) arrs, over (element idx) (+ length (arrs!!idx)) counts)

monkeyBusiness :: ([[Int]], [Int]) -> Int
monkeyBusiness = product . take 2 . sortBy (flip compare) . snd


monkeys :: String -> (([[Int]], [Int]), [Monkey])
monkeys = first (id &&& flip replicate 0 . length) . unzip . map (readMonkey . lines) . splitOn "\n\n"

solution1 = monkeyBusiness . (!!20) . (iterate <$> (doRound (`div` 3) <$> length <*> zip [0..]) . snd <*> fst)
solution2 = monkeyBusiness . (!!10000) . (iterate <$> (doRound <$> flip mod . foldl lcm 1 . map divN <*> length <*> zip [0..]) . snd <*> fst)

main = print . (solution1 &&& solution2) . monkeys =<< readFile "input"
