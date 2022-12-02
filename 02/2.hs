import Control.Applicative
import Control.Monad
import System.IO

getScore :: (Int, Int) -> Int
getScore (u, v) = shapeValue v + convertToScore ((u - v) `mod` 3) where
    convertToScore 0 = 3
    convertToScore 1 = 0
    convertToScore 2 = 6
    shapeValue = (+1)

stringsToInt :: [String] -> (Int, Int)
stringsToInt = ((,) <$> (+(- fromEnum 'A')) . head <*> (+(- fromEnum 'X')) . last) . map (fromEnum . head)

adjust :: (Int, Int) -> (Int, Int)
adjust = (,) <$> fst <*> (`mod` 3) . (+2) . uncurry (+)

strategy1 = sum . map (getScore . stringsToInt)
strategy2 = sum . map (getScore . adjust . stringsToInt)

main = print . ((,) <$> strategy1 <*> strategy2) . map words . lines =<< readFile "input"
