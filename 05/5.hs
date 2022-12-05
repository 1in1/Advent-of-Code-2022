import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Data.List.Split
import Data.String.Utils
import System.IO

emptyStacks = (`replicate` []) . length . words . head . filter (notElem '[')
stackText = reverse . map (map strip . chunksOf 4) . takeWhile (elem '[')
initialState = map (filter (/= "")) . (foldl (zipWith (flip (:))) <$> emptyStacks <*> stackText)

moves = map (((,,) <$> (!!1) <*> (!!3) <*> (!!5)) . map read . words) . filter (elem 'm')

type ShiftFunction a = Int -> [a] -> [a] -> ([a], [a])
stackShift :: ShiftFunction a
stackShift = flip ((<<<) . (<<< first reverse) . first . flip (++)) . splitAt

listShift :: ShiftFunction a
listShift = flip ((<<<) . first . flip (++)) . splitAt

updateStacks :: ShiftFunction a -> [[a]] -> (Int, Int, Int) -> [[a]]
updateStacks shiftFn s (n, from, to) = s'' where
    (y', x') = (shiftFn n <$> (!! (to-1)) <*> (!! (from-1))) s
    s' = s & element (from-1) .~ x'
    s'' = s' & element (to-1) .~ y'

applyMoves :: ShiftFunction a -> [[a]] -> [(Int, Int, Int)] -> [[a]]
applyMoves = foldl . updateStacks

solve = (map ((!!1) . head) . ) . (<*> moves) . (<$> initialState) . applyMoves
main = print . (solve stackShift &&& solve listShift) . lines =<< readFile "input"
