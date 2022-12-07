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

dirs :: [[String]] -> (Tree Int, [[String]])
dirs [cd, ls] = (Node { rootLabel = process ls, subForest = []}, [])
dirs (cd:ls:nextCd:t) = (Node { rootLabel = currLabel, subForest = subDirs }, remaining) where
    currLabel = process ls + (sum . map rootLabel) subDirs
    update (parsedSubDirs, remainingText)
        | null remainingText = (parsedSubDirs, [])
        | head remainingText == ["$ cd .."] = (parsedSubDirs, tail remainingText)
        | otherwise = update $ first (:parsedSubDirs) $ dirs remainingText
    (subDirs, remaining) = update ([], nextCd:t)


solution1 = sum . filter (<= 100000) . flatten
-- We need (head) - n <= 40000000   <=> -40000000 + head >= n
solution2 = minimum . (filter <$> ((<=) . (+(-40000000)) . head) <*> id) . flatten

main = print . (solution1 &&& solution2) . fst . dirs . blocks . lines =<< readFile "input"
