## Perf

Some tidying done after original write for readability, but no significant perf optimisations. Built with `ghc` and `-O2` flag, ran on i5 CPU @ 4GHz.

```
» printf '"stack exec -- aoc2022 --day %s"\n' {01..25} | xargs hyperfine
Benchmark 1: stack exec -- aoc2022 --day 01
  Time (mean ± σ):     246.4 ms ±   3.2 ms    [User: 217.0 ms, System: 30.7 ms]
  Range (min … max):   243.9 ms … 255.6 ms    12 runs
 
Benchmark 2: stack exec -- aoc2022 --day 02
  Time (mean ± σ):     252.5 ms ±   9.7 ms    [User: 223.0 ms, System: 27.8 ms]
  Range (min … max):   244.8 ms … 271.7 ms    11 runs
 
Benchmark 3: stack exec -- aoc2022 --day 03
  Time (mean ± σ):     256.6 ms ±   9.9 ms    [User: 227.2 ms, System: 28.8 ms]
  Range (min … max):   245.5 ms … 272.7 ms    11 runs
 
Benchmark 4: stack exec -- aoc2022 --day 04
  Time (mean ± σ):     266.7 ms ±  13.0 ms    [User: 237.0 ms, System: 26.7 ms]
  Range (min … max):   255.0 ms … 289.4 ms    11 runs
 
Benchmark 5: stack exec -- aoc2022 --day 05
  Time (mean ± σ):     273.6 ms ±  15.5 ms    [User: 245.0 ms, System: 28.5 ms]
  Range (min … max):   257.9 ms … 307.1 ms    11 runs
 
Benchmark 6: stack exec -- aoc2022 --day 06
  Time (mean ± σ):     256.7 ms ±  13.9 ms    [User: 224.0 ms, System: 30.7 ms]
  Range (min … max):   244.7 ms … 278.6 ms    10 runs
 
Benchmark 7: stack exec -- aoc2022 --day 07
  Time (mean ± σ):     253.9 ms ±  11.0 ms    [User: 223.3 ms, System: 28.9 ms]
  Range (min … max):   244.5 ms … 274.6 ms    11 runs
 
Benchmark 8: stack exec -- aoc2022 --day 08
  Time (mean ± σ):     306.9 ms ±  11.1 ms    [User: 292.8 ms, System: 33.7 ms]
  Range (min … max):   297.4 ms … 322.4 ms    10 runs
 
Benchmark 9: stack exec -- aoc2022 --day 09
  Time (mean ± σ):     273.8 ms ±  25.5 ms    [User: 250.3 ms, System: 26.2 ms]
  Range (min … max):   255.0 ms … 325.7 ms    11 runs
 
  Warning: Statistical outliers were detected. Consider re-running this benchmark on a quiet PC without any interferences from other programs. It might help to use the '--warmup' or '--prepare' options.
 
Benchmark 10: stack exec -- aoc2022 --day 10
  Time (mean ± σ):     252.0 ms ±   7.8 ms    [User: 218.7 ms, System: 28.8 ms]
  Range (min … max):   243.8 ms … 267.2 ms    12 runs
 
Benchmark 11: stack exec -- aoc2022 --day 11
  Time (mean ± σ):     447.9 ms ±  13.9 ms    [User: 452.9 ms, System: 70.0 ms]
  Range (min … max):   428.5 ms … 467.3 ms    10 runs
 
Benchmark 12: stack exec -- aoc2022 --day 12
  Time (mean ± σ):     387.4 ms ±   9.1 ms    [User: 373.8 ms, System: 34.3 ms]
  Range (min … max):   376.9 ms … 400.7 ms    10 runs
 
Benchmark 13: stack exec -- aoc2022 --day 13
  Time (mean ± σ):     260.1 ms ±   7.2 ms    [User: 226.7 ms, System: 31.7 ms]
  Range (min … max):   254.2 ms … 275.2 ms    11 runs
 
Benchmark 14: stack exec -- aoc2022 --day 14
  Time (mean ± σ):     716.9 ms ±  12.6 ms    [User: 699.5 ms, System: 43.1 ms]
  Range (min … max):   699.3 ms … 740.0 ms    10 runs
 
Benchmark 15: stack exec -- aoc2022 --day 15
  Time (mean ± σ):     253.5 ms ±   7.0 ms    [User: 222.3 ms, System: 27.4 ms]
  Range (min … max):   245.8 ms … 270.6 ms    10 runs
 
Benchmark 16: stack exec -- aoc2022 --day 16
  Time (mean ± σ):     911.0 ms ±  10.6 ms    [User: 998.0 ms, System: 78.1 ms]
  Range (min … max):   898.5 ms … 934.4 ms    10 runs
 
Benchmark 17: stack exec -- aoc2022 --day 17
  Time (mean ± σ):     758.7 ms ±  16.7 ms    [User: 790.9 ms, System: 76.2 ms]
  Range (min … max):   729.6 ms … 784.5 ms    10 runs
 
Benchmark 18: stack exec -- aoc2022 --day 18
  Time (mean ± σ):     341.0 ms ±   9.6 ms    [User: 332.4 ms, System: 30.9 ms]
  Range (min … max):   329.1 ms … 356.4 ms    10 runs
 
Benchmark 19: stack exec -- aoc2022 --day 19
  Time (mean ± σ):      1.786 s ±  0.018 s    [User: 1.864 s, System: 0.142 s]
  Range (min … max):    1.760 s …  1.816 s    10 runs
 
Benchmark 20: stack exec -- aoc2022 --day 20
  Time (mean ± σ):      3.302 s ±  0.026 s    [User: 3.332 s, System: 0.149 s]
  Range (min … max):    3.266 s …  3.337 s    10 runs
 
Benchmark 21: stack exec -- aoc2022 --day 21
  Time (mean ± σ):     260.9 ms ±   6.6 ms    [User: 233.2 ms, System: 25.6 ms]
  Range (min … max):   255.9 ms … 278.8 ms    11 runs
 
Benchmark 22: stack exec -- aoc2022 --day 22
  Time (mean ± σ):     274.0 ms ±   8.8 ms    [User: 248.1 ms, System: 28.3 ms]
  Range (min … max):   265.5 ms … 289.7 ms    11 runs
 
Benchmark 23: stack exec -- aoc2022 --day 23
  Time (mean ± σ):      2.565 s ±  0.022 s    [User: 2.734 s, System: 0.212 s]
  Range (min … max):    2.543 s …  2.619 s    10 runs
 
Benchmark 24: stack exec -- aoc2022 --day 24
  Time (mean ± σ):      2.984 s ±  0.049 s    [User: 3.608 s, System: 0.418 s]
  Range (min … max):    2.943 s …  3.107 s    10 runs
 
Benchmark 25: stack exec -- aoc2022 --day 25
  Time (mean ± σ):     256.9 ms ±  10.2 ms    [User: 222.9 ms, System: 29.7 ms]
  Range (min … max):   246.8 ms … 274.8 ms    12 runs
```
