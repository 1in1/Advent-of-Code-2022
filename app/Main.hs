module Main (main) where

import Data.Semigroup ((<>))
import Options.Applicative
import System.IO

import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)
import Day05 (day05)
import Day06 (day06)
import Day07 (day07)
import Day08 (day08)
import Day09 (day09)
import Day10 (day10)
import Day11 (day11)
import Day12 (day12)
import Day13 (day13)
import Day14 (day14)
import Day15 (day15)
import Day16 (day16)
import Day17 (day17)
import Day18 (day18)
import Day19 (day19)
import Day20 (day20)
import Day21 (day21)
import Day22 (day22)
import Day23 (day23)
import Day24 (day24)
import Day25 (day25)

days :: Int -> String -> String
days 01 = show . day01
days 02 = show . day02
days 03 = show . day03
days 04 = show . day04
days 05 = show . day05
days 06 = show . day06
days 07 = show . day07
days 08 = show . day08
days 09 = show . day09
days 10 = show . day10
days 11 = show . day11
days 12 = show . day12
days 13 = show . day13
days 14 = show . day14
days 15 = show . day15
days 16 = show . day16
days 17 = show . day17
days 18 = show . day18
days 19 = show . day19
days 20 = show . day20
days 21 = show . day21
days 22 = show . day22
days 23 = show . day23
days 24 = show . day24
days 25 = show . day25

runDay :: Int -> IO String
runDay n 
    | 1 <= n && n <= 25 = days n <$> readFile path
    | otherwise = undefined where
    path = "inputs/" ++ pad
    pad | 1 <= n && n <= 9 = '0':show n
        | otherwise = show n

target :: Parser Int
target = option auto
    (  long "day"
    <> help "Solution to run"
    <> metavar "n" )

main = putStrLn =<< runDay =<< execParser opts where
    opts = info target
        (   fullDesc
        <>  progDesc "Advent of Code 2022 solutions" )
