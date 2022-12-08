import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.List
import System.IO

combineMatrices ::  (a -> a -> a) -> ([[a]], [[a]]) -> [[a]]
combineMatrices = uncurry . zipWith . zipWith

rowProcessForest :: (a -> a -> a) -> ([Int] -> [a]) -> [[Int]] -> [[a]]
rowProcessForest f = (combineMatrices f .) . liftM2 (&&&) map (map . (reverse .) . (. reverse))

fullProcessForest :: (a -> a -> a) -> ([Int] -> [a]) -> [[Int]] -> [[a]]
fullProcessForest f = (combineMatrices f .) . liftM2 (&&&) (rowProcessForest f) ((transpose .) . (. transpose) . rowProcessForest f)


leftVisibleRow :: [Int] -> [Bool]
leftVisibleRow = (True:) . map (uncurry (<)) . (zip <$> id <*> tail) . scanl1 max

leftScenicScoreRow :: [Int] -> [Int]
leftScenicScoreRow = leftScenicScoreRow' (replicate 10 0)

leftScenicScoreRow' :: [Int] -> [Int] -> [Int]
leftScenicScoreRow' cache [] = []
leftScenicScoreRow' cache (x:xs) = (cache !! x):leftScenicScoreRow' cache' xs where
    cache' = uncurry (++) $ (map (const 1) *** map (+1)) $ splitAt (x+1) cache

solution1 = sum . map (length . filter id) . fullProcessForest (||) leftVisibleRow
solution2 = maximum . map maximum . fullProcessForest (*) leftScenicScoreRow

main = print . (solution1 &&& solution2) . (map . map) (read . pure) . lines =<< readFile "input"
