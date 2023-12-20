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
- Maybe make a parsing utils file
  - For example `intParser` is used all over the place
- Are .cabal files usually gitignored???
- I feel like there's a more idiomatic and/or better way for parsing than me just using `splitOn` ALL THE TIME lol
- Might as well use proper like module paths
  - i.e. rename folders to be like `Y2023` rather than `2023` and then in files I can have `module Y2023.Day15 where` rather than `module Day15 where`
- You might need `{-# LANGUAGE ImportQualifiedPost #-}` occasionally
- Dijkstra
  - I think the right abstraction might be a `class` rather than a function that takes like loads of HoFs as its argument
  - Maybe use something like a Sorted Set for the unvisited nodes
  - Revisit whether splitting up the Map actually was a good idea (remember previously you had one map to store both unvisited and visited nodes)
  - Don't use `dist` nomenclature, go with `score`
    - Day 12 2022 uses distances so it made sense back then
    - But other problems use a more general score I guess
  - Generalise the score to be something other than `Int` 
    - I guess you have to be able to combine scores so that feels `Monoid`y or rather `SemiGroup`y
    - And you have to compare scores so that feels `Ord`y
    - Might already be an appropriate type class actually
  - I feel like a common version of Dijkstra starts with just a graph (e.g. adjacency lists) so maybe just make a version that works for that sort of input
  - It's still abysmally slow for Day 17 2023 so see if there's anything you can do there
    - I suspect I'm generating too many edges and nodes so there might be a nice trick I can do
    - I guess what I'm saying is maybe my dijkstra implementation is kinda fine, it just needs to be passed in a nicer structure



## Misc

### Parsing

- I started trying to use Pasec properly in Day 2 of 2023
- I read (this)[https://jsdw.me/posts/haskell-parsec-basics/] for inspiration, was quite helpful!
- I've gone back and parsecified some of the older solutions
- But sometimes all you really need is `chunksOf` and `splitOn` and things like that so I usually leave those alone