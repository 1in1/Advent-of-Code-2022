import Control.Applicative
import Control.Arrow
import Debug.Trace
import Control.Lens
import Control.Monad
import Data.Bool
import Data.HashSet (HashSet)
import Data.List
import Data.List.Split
import Data.Maybe
import System.IO
import qualified Data.HashSet as HashSet

data Blueprint = Blueprint { 
    oreCost :: Int, 
    clayCost :: Int, 
    obsidianCost :: (Int, Int), 
    geodeCost :: (Int, Int) } 
    deriving (Eq, Show)
data State = State { 
    time :: Int,
    ore :: Int,
    clay :: Int, 
    obsidian :: Int, 
    geodes :: Int,
    oreBots :: Int,
    clayBots :: Int, 
    obsidianBots :: Int, 
    geodeBots :: Int } 
    deriving (Eq, Show) 
initialState :: State
initialState = State 0 0 0 0 0 1 0 0 0 

parse :: String -> [Blueprint]
parse = map (
    (Blueprint <$>
    (!!1) <*>
    (!!2) <*>
    ((,) <$> (!!3) <*> (!!4)) <*>
    ((,) <$> (!!5) <*> (!!6))) . 
    map read .
    words . 
    filter (`elem` "0123456789 ")
    ) . lines

ongoingOptions :: Int -> Blueprint -> State -> [State]
ongoingOptions maxTime blueprint s
    | maxTime <= time s = []
    | otherwise = map gainResources $
        filter ((<= maxTime) . time) $
        catMaybes [
            buildGeodeRobot, 
            buildObsidianRobot, 
            buildClayRobot, 
            buildOreRobot, 
            waitTillEnd
            ] where

    gainResources s' = s' { 
        ore = (dt * oreBots s) + ore s',
        clay = (dt * clayBots s) + clay s',
        obsidian = (dt * obsidianBots s) + obsidian s',
        geodes = (dt * geodeBots s) + geodes s'
        } where
        dt = time s' - time s

    -- Assumes rate > 0
    timeToWait :: Int -> Int -> Int -> Int
    timeToWait curr cost rate
        | curr >= cost = 1 -- We still have to wait till the next time!
        | otherwise = (+1) $ (cost - curr + (rate-1)) `div` rate

    waitTillEnd = Just s {
        time = maxTime
        }
    buildOreRobot
        | oreBots s == 0 = Nothing
        | (oreBots s >=) $ maximum $ map ($ blueprint) [oreCost, clayCost, fst . obsidianCost, fst . geodeCost] = Nothing
        | otherwise = Just s {
        time = timeToWait (ore s) (oreCost blueprint) (oreBots s) + time s,
        ore = (subtract (oreCost blueprint) . ore) s,
        oreBots = ((+1) . oreBots) s
        }
    buildClayRobot
        | oreBots s == 0 = Nothing
        | clayBots s >= snd (obsidianCost blueprint) = Nothing
        | otherwise = Just s {
        time = timeToWait (ore s) (clayCost blueprint) (oreBots s) + time s,
        ore = (subtract (clayCost blueprint) . ore) s,
        clayBots = ((+1) . clayBots) s
        }
    buildObsidianRobot
        | (oreBots s == 0) || (clayBots s == 0) = Nothing
        | obsidianBots s >= snd (geodeCost blueprint) = Nothing
        | otherwise = Just s {
        time = max
            (timeToWait (ore s) (fst $ obsidianCost blueprint) (oreBots s))
            (timeToWait (clay s) (snd $ obsidianCost blueprint) (clayBots s))
            + time s,
        ore = (subtract (fst $ obsidianCost blueprint) . ore) s,
        clay = (subtract (snd $ obsidianCost blueprint) . clay) s,
        obsidianBots = ((+1) . obsidianBots) s
        }
    buildGeodeRobot
        | (oreBots s == 0) || (obsidianBots s == 0) = Nothing
        | otherwise = Just s {
        time = max
            (timeToWait (ore s) (fst $ geodeCost blueprint) (oreBots s))
            (timeToWait (obsidian s) (snd $ geodeCost blueprint) (obsidianBots s))
            + time s,
        ore = (subtract (fst $ geodeCost blueprint) . ore) s,
        obsidian = (subtract (snd $ geodeCost blueprint) . obsidian) s,
        geodeBots = ((+1) . geodeBots) s
        }

dfs :: Int -> Blueprint -> Int -> [State] -> Int
dfs _ _ prevMax [] = prevMax
dfs maxTime blueprint prevMax (x:xs) 
    | prevMax >= maxEndScore = dfs maxTime blueprint prevMax xs -- We aren't going to beat this score
    | otherwise = dfs maxTime blueprint (max score prevMax) (ongoing++xs) where
    c = geodeBots x
    dt = maxTime - time x
    maxEndScore = score + (((2*c + dt)*(dt+1)) `div` 2)

    score = geodes x
    ongoing = ongoingOptions maxTime blueprint x

solution1 = sum .
    zipWith (*) [1..] .
    map (\b -> dfs 24 b 0 [initialState])

solution2 = product .
    map (\b -> dfs 32 b 0 [initialState]) .
    take 3

main = print . solution2 . parse =<< readFile "input"
