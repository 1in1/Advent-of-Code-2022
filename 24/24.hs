import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Bool
import Data.HashSet (HashSet)
import Data.List
import Data.Maybe
import System.IO
import qualified Data.HashSet as HashSet
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Debug.Trace

type Point = (Int, Int)
data Parse = Parse {
    end :: Point,
    start :: Point,
    blizzards :: HashSet (Point, Char),
    blizzardsMap :: HashMap (Point, Char) (Point, Char),
    walls :: HashSet Point,
    w :: Int,
    h :: Int
    } deriving (Show, Eq)

parse :: String -> Parse
parse s = Parse end start blizzards blizzardsMap walls w h where
    s' = lines s
    end = (fromJust . elemIndex '.' . last &&& subtract 1 . length) s'
    start = ((,0) . fromJust . elemIndex '.' . head) s'
    coords = concatMap ((\(y,xs) -> [((x,y),c) | (x,c) <- xs]) . second (zip [0..])) $
        zip [0..] s'
    blizzards = HashSet.fromList $ filter ((`elem` "^v<>") . snd) coords
    blizzardsMap = blizzardMoveMap w h walls
    walls = HashSet.fromList $ map fst $ filter ((=='#') . snd) coords
    w = length $ head s'
    h = length s'

moveBlizzards :: Parse -> HashSet (Point, Char) -> HashSet (Point, Char)
moveBlizzards = HashSet.map . (HashMap.!) . blizzardsMap

blizzardMoveMap :: Int -> Int -> HashSet Point -> HashMap (Point, Char) (Point, Char)
blizzardMoveMap w h walls = HashMap.fromList $ map (id &&& wrap . move) allPossibilePositions where
    allPossibilePositions = concatMap (\p -> [(p,d) | d <- "^v<>"]) $
        filter (not . flip HashSet.member walls)
        [(x,y) | x <- [1..w-2], y <- [0..h-1]]
    move (p,c) = (dir c p, c)
    dir '^' = second (subtract 1)
    dir 'v' = second (+1)
    dir '<' = first (subtract 1)
    dir '>' = first (+1)
    wrap (p',c)
        | HashSet.member p' walls = (cross c p', c)
        | otherwise = (p',c) where
        -- We assume there are no up (resp. down) blizzards in the beginning (resp. end) columns
        -- If there were, then cycling back would be undefined
        cross '^' (x,y) = (x,h-2)
        cross 'v' (x,y) = (x,1)
        cross '<' (x,y) = (w-2,y)
        cross '>' (x,y) = (1,y)

bfs :: Int -> Int -> Point -> HashSet Point -> HashSet (Int, Point) -> [HashSet (Point, Char)] -> [(Int, Point)] -> Int
bfs loopLength h end walls visited blizzardsAtTimes (x:xs)
    | HashSet.member x visited = bfs loopLength h end walls visited blizzardsAtTimes xs
    | end == snd x = fst x
    | otherwise = bfs loopLength h end walls (HashSet.insert x visited) blizzardsAtTimes (xs ++ ongoing) where
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
        any (`HashSet.member` (blizzardsAtTimes!!(t' `mod` loopLength))) [
            (p,'^'),
            (p,'v'),
            (p,'<'),
            (p,'>')
            ]

solution1 = bfs <$>
    (lcm <$> subtract 2 . w <*> subtract 2 . h) <*>
    h <*>
    end <*>
    walls <*>
    const HashSet.empty <*>
    (iterate <$> moveBlizzards <*> blizzards) <*>
    singleton . (0,) . start
solution2 (t1, p) = t3 where
    blizzardsInTime = (iterate <$> moveBlizzards <*> blizzards) p
    t2 = (bfs <$>
        (lcm <$> subtract 2 . w <*> subtract 2 . h) <*>
        h <*>
        start <*>
        walls  <*>
        const HashSet.empty <*>
        const blizzardsInTime <*>
        singleton . (t1,) . end) p
    t3 = (bfs <$>
        (lcm <$> subtract 2 . w <*> subtract 2 . h) <*>
        h <*>
        end <*>
        walls <*>
        const HashSet.empty <*>
        const blizzardsInTime <*>
        singleton . (t2,) . start) p

main = print . (fst &&& solution2) . (solution1 &&& id) . parse =<< readFile "input"
