import Control.Arrow
import Control.Monad
import Data.List
import System.IO

solve m n xs | (uncurry (==) . (nub &&& id) . take m) xs = n
             | otherwise = (solve m (n+1) . tail) xs

main = print . (solve 4 4 &&& solve 14 14) =<< readFile "input"
