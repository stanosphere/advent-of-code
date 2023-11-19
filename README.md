# Advent Of Code

Advent of code solutions written in Haskell.

## Running Haskell Stuff
- do `stack build`
- do `stack ghci`
- go to the place where your file is (e.g. `:cd src/2020/`)
- load your file (e.g. `:l Day4`)
- run your command (e.g. `part1`)
- make sure you have your puzzle input under "fixtures"
    - I've been saving puzzle inputs in files like `input{dayNumber}.txt` in a `fixtures` folder for each year
    - So you'd expect a file at `advent-of-code/src/2018/fixtures/input3.txt` to contain the input for day 3 of 2018
    - The puzzle input files are gitignored

Happy Haskelling my future self!

## TODO

- Reading input is basically always the same so maybe only define it once
- Are .cabal files usually gitignored???