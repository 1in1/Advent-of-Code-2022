module Day21 (day21) where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Bool
import Data.HashMap.Strict (HashMap)
import Data.List
import Data.Maybe
import Data.Ratio
import System.IO
import qualified Data.HashMap.Strict as HashMap

data Yell = Number Rational | Dependent String String (Rational -> Rational -> Rational)

parse :: String -> HashMap String Yell
parse = HashMap.fromList . map ((head &&& f . tail) . words . filter (/= ':')) . lines where
    f x = bool (Number $ (% 1) $ read $ head x) (parseOp x) (length x > 1)
    parseOp [x, o, y] = Dependent x y (g o)
    g "+" = (+)
    g "-" = (-)
    g "/" = (/)
    g "*" = (*)

findNumber :: String -> HashMap String Yell -> Rational
findNumber s yells = findNumber' (yells HashMap.! s) where
    findNumber' (Number n) = n
    findNumber' (Dependent x y op) = op (findNumber' (yells HashMap.! x)) (findNumber' (yells HashMap.! y))

getDiff :: HashMap String Yell -> String -> String -> Rational
getDiff yells x y = findNumber x yells - findNumber y yells

getRootDiff :: HashMap String Yell -> Rational
getRootDiff yells = uncurry (getDiff yells) (pair $ yells HashMap.! "root") where
    pair (Dependent x y _) = (x,y)

getRootDiffGivenHumn :: HashMap String Yell -> Rational -> Rational
getRootDiffGivenHumn yells n = getRootDiff $ HashMap.insert "humn" (Number n) yells

secant :: Rational -> Rational -> HashMap String Yell -> Rational
secant x0 x1 yells
    | f0 == 0 = x0
    | otherwise = secant x1 x2 yells where
        [f0, f1] = map (getRootDiffGivenHumn yells) [x0, x1]
        x2 = x1 - f1*(x1 - x0)/(f1 - f0)


solution1 = findNumber "root"
solution2 = secant 400000 400001

day21 = (solution1 &&& solution2) . parse
