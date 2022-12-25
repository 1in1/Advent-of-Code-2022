import Control.Arrow
import System.IO

readSNAFU :: String -> Int
readSNAFU = foldl f 0 where
    f :: Int -> Char -> Int
    f acc curr = 5*acc + v curr
    v '2' = 2
    v '1' = 1
    v '0' = 0
    v '-' = -1
    v '=' = -2

toSNAFU :: Int -> String
toSNAFU n
    | n == 0 = ""
    | otherwise = toSNAFU (n' `div` 5)++[c!!r] where
    c = "012=-"
    r = n `mod` 5
    n' = n + shifts!!r
    shifts = [0,-1,-2,2,1]

main = print . toSNAFU . sum . map readSNAFU . lines =<< readFile "input"
