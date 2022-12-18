import Control.Arrow
import Data.Tuple.Extra (uncurry3)
import Data.Ord
import Debug.Trace
import Control.Applicative
import Control.Monad
import Data.Bool
import Data.List
import Data.List.Split
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import System.IO

data Valve = Valve { label :: Int, flowRate :: Int, adjacentValves :: IntSet } deriving(Show, Eq)

parse :: String -> (Int, [Valve])
parse s = (table HashMap.! "AA",) $ map (Valve <$> 
    (table HashMap.!) . (!!1) <*>
    read . filter (`elem` "0123457689") . (!!4) <*>
    IntSet.fromList . map (table HashMap.!) . splitOn "," . concat . drop 9) s' where

    s' = map words $ lines s
    table :: HashMap String Int
    table = HashMap.fromList $ zip (map (!!1) s') [0..]

min' :: Maybe Int -> Maybe Int -> Maybe Int
min' x Nothing = x
min' Nothing y = y
min' (Just x) (Just y) = Just (min x y)

max' :: Maybe Int -> Maybe Int -> Maybe Int
max' x Nothing = x
max' Nothing y = y
max' (Just x) (Just y) = Just (max x y)

-- The graph is connected so we don't need to worry about using catMaybes
getMinimalPaths :: [Valve] -> [[Int]]
getMinimalPaths valves = map catMaybes $ getMinimalPaths' (n-1) where
    n = length valves

    getMinimalPaths' :: Int -> [[Maybe Int]]
    getMinimalPaths' (-1) = [[ determineInitial x y | y <- [0..(n-1)]] | x <- [0..(n-1)]]
    getMinimalPaths' k = [[ min' (cache!!x!!y) (liftA2 (+) (cache!!x!!k) (cache!!k!!y)) | y <- [0..(n-1)]] | x <- [0..(n-1)]] where
        cache = getMinimalPaths' (k-1)
    determineInitial x y
        | x == y = Just 0
        | IntSet.member y $ adjacentValves (valves!!x) = Just 1
        | otherwise = Nothing

reduceSearchSpace :: Int -> [Valve] -> [[Int]] -> ([Valve], Int, [[Int]])
reduceSearchSpace starting valves = (valves',aaIdx',) . f . map f where
    valvesToKeep = sort $ (starting:) $ map label $ filter ((>0) . flowRate) valves
    f = map snd . filter ((`elem` valvesToKeep) . fst) . zip [0..]
    aaIdx' = 0
    valves' = zipWith (curry (\v -> (snd v) { label = fst v })) [0..] $ 
        filter ((`elem` valvesToKeep) . label) valves

dfs :: Int -> [Valve] -> Int -> [[Int]] -> HashMap IntSet Int
dfs t v s a = dfs' v a s t (IntSet.fromList [s]) where
    -- Starting position, time remaining, visited (and turned) valves
    -- Returns best possible flow
    dfs' :: [Valve] -> [[Int]] -> Int -> Int -> IntSet -> HashMap IntSet Int
    dfs' valves adj starting t visited 
        | t <= 0 = HashMap.fromList [(visited, 0)]
        | null ongoingPaths = curr
        | otherwise = foldl (HashMap.unionWith max) curr ongoingPaths where

        curr = HashMap.fromList [(visited, releasedThisTurn*t)]
        ongoingPaths = map f $ filter (`IntSet.notMember` visited) $ map label valves
        f nextValve = HashMap.map (+(releasedThisTurn * min t (movingTime nextValve + 1))) $ dfs' valves adj nextValve (t- 1 - movingTime nextValve) (IntSet.insert nextValve visited)
        movingTime nextValue = adj!!starting!!nextValue
        releasedThisTurn = sum $ map flowRate $ filter ((`IntSet.member` visited) . label) valves


bestDisjointPair :: [(IntSet, Int)] -> Maybe Int
bestDisjointPair xs = search sortedList Nothing where
    sortedList = sortOn (Down . snd) $ map (first $ IntSet.delete 0) xs
    search :: [(IntSet, Int)] -> Maybe Int -> Maybe Int
    search [] currTop = currTop
    search (l:ls) currTop = search ls $
        max' currTop $
        (+ snd l) . snd <$>
        find (IntSet.disjoint (fst l) . fst) ls

solution1 = maximum .
    map snd .
    HashMap.toList .
    uncurry3 (dfs 30) .
    (reduceSearchSpace <$> fst <*> snd <*> getMinimalPaths . snd)

solution2 = bestDisjointPair .
    HashMap.toList .
    uncurry3 (dfs 26) .
    (reduceSearchSpace <$> fst <*> snd <*> getMinimalPaths . snd)

main = print . (solution1 &&& solution2) . parse =<< readFile "input"
