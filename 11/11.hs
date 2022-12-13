import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Bool
import Data.IntMap.Lazy(IntMap)
import Data.List
import Data.List.Split
import Data.Maybe
import Data.String.Utils
import System.IO
import qualified Data.IntMap.Lazy as IntMap


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

readStartingItems :: [String] -> [Int]
readStartingItems = map read . words . filter (`elem` "0123456789 ") . (!!1)

readMonkey :: [String] -> ([Int], Monkey)
readMonkey = readStartingItems &&& (Monkey <$> readOperation <*> readNext <*> readDivN)


processMonkey :: (Int -> Int) -> Int -> Monkey -> [Int] -> IntMap [Int]
processMonkey worryControl n monkey = foldl foldFn IntMap.empty . map ((next monkey &&& id) . worryControl . op' monkey) where
    foldFn curr (newMonkey, newVal) = IntMap.insertWith (flip (++)) newMonkey [newVal] curr

doRound :: (Int -> Int) -> Int -> [(Int, Monkey)] -> (IntMap [Int], [Int]) -> (IntMap [Int], [Int])
doRound worryControl n [] = id
doRound worryControl n ((idx, monkey):xs) = doRound worryControl n xs . (updateItems &&& updateInteractions) where
    updateItems = (IntMap.unionWith (++) <$> processMonkey worryControl n monkey . (IntMap.! idx) <*> IntMap.insert idx []) . fst
    updateInteractions = uncurry (over (element idx) . (+) . length . (IntMap.! idx))

monkeyBusiness :: (a, [Int]) -> Int
monkeyBusiness = product . take 2 . sortBy (flip compare) . snd


monkeys :: String -> ((IntMap [Int], [Int]), [Monkey])
monkeys = first (IntMap.fromAscList . zip [0..] &&& flip replicate 0 . length) . unzip . map (readMonkey . lines) . splitOn "\n\n"

solution1 = monkeyBusiness . (!!20) . (iterate <$> (doRound (`div` 3) <$> length <*> zip [0..]) . snd <*> fst)
solution2 = monkeyBusiness . (!!10000) . (iterate <$> (doRound <$> flip mod . foldl lcm 1 . map divN <*> length <*> zip [0..]) . snd <*> fst)

main = print . (solution1 &&& solution2) . monkeys =<< readFile "input"
