# Advent Of Code

Advent of code solutions written in Haskell.

## Running Haskell Stuff
- do `stack build`
- do `stack ghci`
- go to the place where your file is (e.g. `:cd src/2020/`)
- load your file (e.g. `:l Day4`)
- do `:set +s` so you can time your answers
    - although this might just make you feel sad!
    - Remember: "every problem has a solution that completes in at most 15 seconds on ten-year-old hardware"
    - So if yours don't you're a slow boi
- run your command (e.g. `part1`)
- make sure you have your puzzle input under "fixtures"
    - I've been saving puzzle inputs in files like `input{dayNumber}.txt` in a `fixtures` folder for each year
    - So you'd expect a file at `advent-of-code/src/2018/fixtures/input3.txt` to contain the input for day 3 of 2018
    - The puzzle input files are gitignored

Happy Haskelling my future self!

## TODO

- Reading input is basically always the same so maybe only define it once
- Are .cabal files usually gitignored???
- I feel like there's a more idiomatic and/or better way for parsing than me just using `splitOn` ALL THE TIME lol
- Historical: looks like some files might not be loadable until I sort out some problems...
  - `Control.Monad.Trans.State.Lazy` warning in 2022/Day5
  - `Text.Parsec` warning in 2022/Day13
  - `Data.Text` warning in 2021/Day4