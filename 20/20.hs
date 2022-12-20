import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Bool
import Data.List
import Data.Maybe
import Data.Sequence (Seq)
import System.IO
import qualified Data.Sequence as Seq

parse :: String -> Seq (Int, Int)
parse = Seq.fromList . zip [0..] . map read . lines

shiftEltAtIdx :: Int -> Seq (Int, Int) -> Seq (Int, Int)
shiftEltAtIdx idx xs = Seq.insertAt idx' elt xs' where
    elt = xs `Seq.index` idx
    xs' = Seq.deleteAt idx xs
    idx' = (idx + snd elt) `mod` length xs'

shiftGivenElt :: (Int, Int) -> Seq (Int, Int) -> Seq (Int, Int)
shiftGivenElt p = shiftEltAtIdx <$> fromMaybe (-1) . Seq.elemIndexL p <*> id

shift :: Seq (Int, Int) -> Seq (Int, Int) -> Seq (Int, Int)
shift Seq.Empty = id
shift ys = shift (Seq.drop 1 ys) . shiftGivenElt (ys `Seq.index` 0)

extractIndicesCircular :: [Int] -> Int -> Seq a -> [a]
extractIndicesCircular idxs base xs = mapMaybe (($xs) . Seq.lookup . (`mod` length xs) . (+base)) idxs

groveCoords :: Seq (Int, Int) -> [Int]
groveCoords = (extractIndicesCircular [1000, 2000, 3000] <$> fromMaybe 0 . Seq.elemIndexL 0 <*> id) .
    Seq.mapWithIndex (const snd)

solution1 = sum . groveCoords . (shift <$> id <*> id)
solution2 = sum . groveCoords . (!!10) . 
    (iterate <$> shift <*> id) . 
    Seq.mapWithIndex (const $ second (*811589153))

main = print . (solution1 &&& solution2) . parse =<< readFile "input"
