module Day07 (day07) where

import Control.Arrow
import Control.Monad
import Data.List
import Data.List.Split
import Data.Tree
import System.IO

-- Easier chunks to process
blocks :: [String] -> [[String]]
blocks = filter (/= []) . split (whenElt ("$ cd" `isInfixOf`))

process :: [String] -> Int
process = sum . map read . filter (/= "dir") . map (head . words) . tail

dirs :: [[String]] -> ([Int], [[String]])
dirs [cd, ls] = ([process ls], [])
dirs (cd:ls:nextCd:t) = (currLabel:concat subDirs, remaining) where
    currLabel = process ls + (sum . map head) subDirs
    update :: ([[Int]], [[String]]) -> ([[Int]], [[String]])
    update (parsedSubDirs, remainingText)
        | null remainingText = (parsedSubDirs, [])
        | head remainingText == ["$ cd .."] = (parsedSubDirs, tail remainingText)
        | otherwise = update $ first (:parsedSubDirs) $ dirs remainingText
    (subDirs, remaining) = update ([], nextCd:t)


solution1 = sum . filter (<= 100000)
-- We need (head) - n <= 40000000   <=> -40000000 + head >= n
solution2 = minimum . (filter <$> ((<=) . (+(-40000000)) . head) <*> id)

day07 = (solution1 &&& solution2) . fst . dirs . blocks . lines
