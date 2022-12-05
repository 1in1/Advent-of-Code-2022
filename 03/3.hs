import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Data.List.Split
import System.IO
import qualified Data.HashSet as HashSet

priority :: Char -> Int
priority c
    | x < 97 = x - (65 - 27)
    | otherwise = x - (97 - 1) where
        x = fromEnum c

intersectChars :: [[Char]] -> Int
intersectChars = priority . head . HashSet.toList . foldl1 HashSet.intersection . map HashSet.fromList

itemsSolution = sum . map (intersectChars . (^..each) . (splitAt <$> (`div` 2) . length <*> id))
badgesSolution = sum . map intersectChars . chunksOf 3

main = print . (itemsSolution &&& badgesSolution) . lines =<< readFile "input"
