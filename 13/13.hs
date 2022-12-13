import Control.Arrow
import Control.Lens
import Control.Monad
import Data.List
import Data.List.Split
import System.IO

data Packet = Raw Int | Nested [Packet] deriving (Show, Eq)
instance Ord Packet where
    compare (Raw x) (Raw y) = compare x y
    compare (Nested []) (Nested []) = EQ
    compare (Nested []) (Nested (_:_)) = LT
    compare (Nested (_:_)) (Nested []) = GT
    compare (Nested (x:xs)) (Nested (y:ys)) = case compare x y of
                                                   EQ -> compare (Nested xs) (Nested ys)
                                                   other -> other
    compare (Raw x) (Nested y) = compare (Nested [Raw x]) (Nested y)
    compare (Nested x) (Raw y) = compare (Nested x) (Nested [Raw y])

parse :: String -> Packet
parse "" = Nested []
parse s 
    | head s == '[' = Nested (map parse $ parse' (init $ tail s) "" 0)
    | otherwise = Raw (read s)

-- The strings representing packets in an internal list of packets
parse' :: String -> String -> Int -> [String]
parse' [] buffer _ = [buffer]
parse' (x:xs) buffer bracketDepth
    | x == ',' && bracketDepth == 0 = buffer:parse' xs "" bracketDepth
    | x == '[' = parse' xs (buffer++['[']) (bracketDepth+1)
    | x == ']' = parse' xs (buffer++[']']) (bracketDepth-1)
    | otherwise = parse' xs (buffer++[x]) bracketDepth

parsePairs :: String -> [(Packet, Packet)]
parsePairs = map (over both parse . ((!!0) &&& (!!1)) . lines) . splitOn "\n\n"
parseAll :: String -> [Packet]
parseAll = map parse . filter (/= "") . lines

solution1 = sum . map fst . filter (uncurry (<=) . snd) . zip [1..] . parsePairs
dividers = [Nested [Nested [Raw 2]], Nested [Nested [Raw 6]]]
solution2 = product . map (+1) . findIndices (`elem` dividers) . sort . (++dividers) . parseAll

main = print . (solution1 &&& solution2) =<< readFile "input"
