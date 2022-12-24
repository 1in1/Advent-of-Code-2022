import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Bool
import Data.HashSet (HashSet)
import Data.List
import Data.Maybe
import Data.MultiSet (MultiSet)
import System.IO
import qualified Data.HashSet as HashSet
import qualified Data.MultiSet as MultiSet

type Point = (Int, Int)
data Parse = Parse {
    end :: Point,
    start :: Point,
    blizzards :: MultiSet (Point, Char),
    walls :: HashSet Point,
    w :: Int,
    h :: Int
    } deriving (Show, Eq)

parse :: String -> Parse
parse = (Parse <$>
    (fromJust . elemIndex '.' . last &&& subtract 1 . length) <*>
    (,0) . fromJust . elemIndex '.' . head <*>
    MultiSet.fromList .
        filter ((`elem` "^v<>") . snd) . 
        concatMap ((\(y,xs) -> [((x,y),c) | (x,c) <- xs]) . second (zip [0..])) . 
        zip [0..] <*>
    HashSet.fromList .
        map fst .
        filter ((=='#') . snd) .
        concatMap ((\(y,xs) -> [((x,y),c) | (x,c) <- xs]) . second (zip [0..])) . 
        zip [0..] <*>
    length . head <*>
    length)
     . lines

moveBlizzards :: Int -> Int -> HashSet Point -> MultiSet (Point, Char) -> MultiSet (Point, Char)
moveBlizzards w h walls = MultiSet.map (wrap . move) where
    move (p,c) = (p, dir c p, c)
    dir '^' = second (subtract 1)
    dir 'v' = second (+1)
    dir '<' = first (subtract 1)
    dir '>' = first (+1)
    wrap (p,p',c)
        | HashSet.member p' walls = (cross c p, c)
        | otherwise = (p',c) where
        -- We assume there are no up (resp. down) blizzards in the beginning (resp. end) columns
        -- If there were, then cycling back would be undefined
        cross '^' (x,y) = (x,h-2)
        cross 'v' (x,y) = (x,1)
        cross '<' (x,y) = (w-2,y)
        cross '>' (x,y) = (1,y)

bfs :: Int -> Point -> HashSet Point -> HashSet (Int, Point) -> [MultiSet (Point, Char)] -> [(Int, Point)] -> Int
bfs h end walls visited blizzardsAtTimes (x:xs)
    | HashSet.member x visited = bfs h end walls visited blizzardsAtTimes xs
    | end == snd x = fst x
    | otherwise = bfs h end walls (HashSet.insert x visited) blizzardsAtTimes (xs ++ ongoing) where
    t' = 1 + fst x
    ongoing = map (t',) $
        filter ((< h) . snd) $
        filter ((>= 0) . snd) $
        filter willNotHitBlizzard $
        filter (not . flip HashSet.member walls) $
        map ($ snd x) [
            id,
            second (subtract 1),
            first (subtract 1),
            second (+1),
            first (+1)
            ]
    willNotHitBlizzard p = not $
        any (`MultiSet.member` (blizzardsAtTimes!!t')) [
            (p,'^'),
            (p,'v'),
            (p,'<'),
            (p,'>')
            ]

solution1 = bfs <$>
    h <*>
    end <*>
    walls <*>
    const HashSet.empty <*>
    (iterate <$> (moveBlizzards <$> w <*> h <*> walls) <*> blizzards) <*>
    singleton . (0,) . start
solution2 p = t1 + t2 + t3 where
    blizzardsInTime = (iterate <$> (moveBlizzards <$> w <*> h <*> walls) <*> blizzards) p
    t1 = (bfs <$>
        h <*>
        end <*>
        walls <*>
        const HashSet.empty <*>
        const blizzardsInTime <*>
        singleton . (0,) . start) p
    t2 = (bfs <$>
        h <*>
        start <*>
        walls <*>
        const HashSet.empty <*>
        const (drop t1 blizzardsInTime) <*>
        singleton . (0,) . end) p
    t3 = (bfs <$>
        h <*>
        end <*>
        walls <*>
        const HashSet.empty <*>
        const (drop (t1 + t2) blizzardsInTime) <*>
        singleton . (0,) . start) p

main = print . (solution1 &&& solution2) . parse =<< readFile "input"
