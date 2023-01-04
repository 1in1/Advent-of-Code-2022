module Day22 (day22) where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Bool
import Data.HashMap.Strict (HashMap)
import Data.List
import Data.List.Extra ((!?))
import Data.Maybe
import Data.Ratio
import System.IO
import qualified Data.HashMap.Strict as HashMap
import Data.List.Split

type WalkDirection = Char
type TurnDirection = Char
type Point = (Int, Int)
type State = (Point, WalkDirection)
data Tile = Open | Solid | Off deriving (Show, Eq)
charToTile :: Char -> Tile
charToTile '.' = Open
charToTile '#' = Solid
charToTile ' ' = Off
data Instruction = Walk Int | Turn TurnDirection deriving (Show, Eq)

parse :: String -> ((Point, HashMap Point Tile), [Instruction])
parse = (parseMap *** parseInstructions) . (init . init &&& last) . lines where
    parseMap = (,0) . fromJust . elemIndex '.' . (!!0) &&&
        HashMap.filter (/= Off) .
        HashMap.fromList .
        concatMap (f . second (zip [0..])) .
        zip [0..] where
        f (y,[]) = []
        f (y,(x,c):xs) = ((x,y), charToTile c):f (y,xs)

    parseInstructions = uncurry (:) .
        (Walk . read . head &&& 
            concatMap ((^..each) . (Turn . head &&& Walk . read . tail)) . tail) .
        split (keepDelimsL $ oneOf "LR")


type WrapFunction = State -> State
doInstruction :: HashMap Point Tile -> WrapFunction -> Instruction -> State -> State
doInstruction tiles wrapFn (Turn 'R') (p, dir) = (p, nextDir dir) where
    nextDir 'U' = 'R'
    nextDir 'R' = 'D'
    nextDir 'D' = 'L'
    nextDir 'L' = 'U'
doInstruction tiles wrapFn (Turn 'L') (p, dir) = (p, prevDir dir) where
    prevDir 'U' = 'L'
    prevDir 'R' = 'U'
    prevDir 'D' = 'R'
    prevDir 'L' = 'D'
doInstruction tiles wrapFn (Walk n) currState = firstOpenState where
    firstOpenState = last $
        takeWhile ((== Just Open) . (tiles HashMap.!?) . fst) path
    path = pathFromInDirectionFor currState n
    
    -- Ignore walls - deal with that above
    pathFromInDirectionFor :: State -> Int -> [State]
    pathFromInDirectionFor curr 0 = [curr]
    pathFromInDirectionFor ((x,y),dir) m = ((x,y),dir):pathFromInDirectionFor nextState' (m-1) where

        nextState = next dir
        nextState' = bool (wrapFn ((x,y),dir)) nextState $ HashMap.member (fst nextState) tiles

        next :: WalkDirection -> State
        next 'R' = ((x+1,y), dir)
        next 'L' = ((x-1,y), dir)
        next 'D' = ((x,y+1), dir)
        next 'U' = ((x,y-1), dir)


doInstructions :: HashMap Point Tile -> WrapFunction -> [Instruction] -> State -> State
doInstructions tiles wrapFn = foldl1 (flip (.)) . map (doInstruction tiles wrapFn)


password :: State -> Int
password ((x,y),d) = 1000*(y+1) + 4*(x+1) + f d where
    f 'R' = 0
    f 'D' = 1
    f 'L' = 2
    f 'U' = 3


solution1 ((starting, tiles), instructions) = password $ doInstructions tiles plainWrap instructions (starting, 'R') where
    validPoint = flip HashMap.member tiles
    plainWrap :: State -> State
    plainWrap ((x,y),d)
        | d == 'L' = ((maxX,y),d)
        | d == 'R' = ((minX,y),d)
        | d == 'U' = ((x,maxY),d)
        | d == 'D' = ((x,minY),d) where
        minX = last $ takeWhile (validPoint . (,y)) [x,x-1..]
        maxX = last $ takeWhile (validPoint . (,y)) [x,x+1..]
        minY = last $ takeWhile (validPoint . (x,)) [y,y-1..]
        maxY = last $ takeWhile (validPoint . (x,)) [y,y+1..]

type FaceLoc = (Int, Int)
type CubeEdge = (FaceLoc, FaceLoc)
type FaceDirPair = (FaceLoc, WalkDirection)
data NextEdge = Oriented FaceDirPair | Flipped FaceDirPair deriving (Show)

solution2 ((starting, tiles), instructions) =
    password $ 
    doInstructions tiles cubeWrap instructions (starting, 'R') where
    n = 50

    face = (`div` n) *** (`div` n)
    faces = nub $ map face $ HashMap.keys tiles

    swap (a, Oriented b) = (b, Oriented a)
    swap (a, Flipped b) = (b, Flipped a)

    -- Hardcoded :(
    edges :: HashMap FaceDirPair NextEdge
    edges = HashMap.fromList $ edges' ++ map swap edges' where
        edges' = [
            -- Adjacents
            (((1,0), 'R'), Oriented ((2,0), 'L')),
            (((1,0), 'D'), Oriented ((1,1), 'U')),
            (((1,1), 'D'), Oriented ((1,2), 'U')),
            (((0,2), 'R'), Oriented ((1,2), 'L')),
            (((0,2), 'D'), Oriented ((0,3), 'U')),
            -- Wrapped round corners
            (((1,1), 'R'), Oriented ((2,0), 'D')),
            (((1,1), 'L'), Oriented ((0,2), 'U')),
            (((1,2), 'D'), Oriented ((0,3), 'R')),
            -- Others
            (((1,0), 'L'), Flipped  ((0,2), 'L')),
            (((0,3), 'L'), Oriented ((1,0), 'U')),
            (((1,2), 'R'), Flipped  ((2,0), 'R')),
            (((2,0), 'U'), Oriented ((0,3), 'D')) 
            ]

    -- Can we derive it?
    -- Try and derive automatically
    -- Set up a space of the 12 edges of an abstract cube, and keep note of their relative directions
    -- Then walk through the 
    --
    -- Can we do this with faces instead?
    --
    -- Take a standard cube net, with:
    --    1
    --  2 3 4
    --    5
    --    6

    edgeMap :: FaceDirPair -> NextEdge -> Point -> Point
    edgeMap = f where
        -- We change to different coords - we just represent it as a value t for how
        -- far along the edge we are
        f a (Oriented b) = fromRep b . toRep a
        f a (Flipped b) = fromRep b . flip subtract (n-1) . toRep a

        toRep :: FaceDirPair -> Point -> Int
        toRep (_, 'R') = (`mod` n) . snd
        toRep (_, 'L') = (`mod` n) . snd
        toRep (_, 'D') = (`mod` n) . fst
        toRep (_, 'U') = (`mod` n) . fst

        fromRep :: FaceDirPair -> Int -> Point
        fromRep ((p,q), 'R') = ((n*p)+(n-1),) . (+(n*q))
        fromRep ((p,q), 'L') = (n*p   ,) . (+(n*q))
        fromRep ((p,q), 'D') = (,(n*q)+(n-1)) . (+(n*p))
        fromRep ((p,q), 'U') = (,n*q   ) . (+(n*p))

    cubeWrap :: State -> State
    cubeWrap ((x,y),d) = ((x'',y''),d'') where

        originalFace = (face (x,y), d)
        edge = edges HashMap.! originalFace

        (x'', y'') = (edgeMap ((x,y),d) edge) (x,y)
        d'' = f edge where
            f (Oriented (_, d')) = opposite d'
            f (Flipped (_, d')) = opposite d'
            opposite 'R' = 'L'
            opposite 'L' = 'R'
            opposite 'D' = 'U'
            opposite 'U' = 'D'

day22 = (solution1 &&& solution2) . parse
