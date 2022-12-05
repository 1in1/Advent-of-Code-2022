import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Data.List.Split
import Data.String.Utils
import System.IO

stackCount = length . words . head . filter (notElem '[')
stacks = reverse . map (map strip . chunksOf 4) . takeWhile (elem '[')
initialState = map (filter (/= "")) . (foldl (zipWith (flip (:))) <$> ((`replicate` []) . stackCount) <*> stacks)

moves = map (((,,) <$> (!!1) <*> (!!3) <*> (!!5)) . map read . words) . filter (elem 'm')

type ShiftFunction a = Int -> [a] -> [a] -> ([a], [a])
stackShift :: ShiftFunction a
stackShift n y = first (++y) <<< first reverse <<< splitAt n

listShift :: ShiftFunction a
listShift n y = first (++y) <<< splitAt n

updateStacks :: ShiftFunction a -> [[a]] -> (Int, Int, Int) -> [[a]]
updateStacks shiftFn s (n, from, to) = s'' where
    (y', x') = (shiftFn n <$> (!! (to-1)) <*> (!! (from-1))) s
    s' = s & element (from-1) .~ x'
    s'' = s' & element (to-1) .~ y'

applyMoves :: ShiftFunction a -> [[a]] -> [(Int, Int, Int)] -> [[a]]
applyMoves = foldl . updateStacks

solve shiftFn = map ((!!1) . head) . (applyMoves shiftFn <$> initialState <*> moves)
main = print . (solve stackShift &&& solve listShift) . lines =<< readFile "input"
