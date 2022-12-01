import Control.Applicative
import Control.Monad
import Data.List
import Data.List.Split
import System.IO

readInt :: String -> Int
readInt = read

calList = map (sum . map readInt . words) . splitOn "\n\n"
sumThreeCals = sum . topThree
topThree (x:xs)
    | length xs < 3 = x:xs
    | x <= mini = z
    | otherwise = zz where 
        z = topThree xs
        mini = minimum z
        zz = delete mini (x:z)

main = print . ((,) <$> maximum <*> sumThreeCals) . calList =<< readFile "input"
