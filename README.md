# Advent of Code 2020

This repository contains my solutions to the puzzles of [Advent of Code 2020](https://adventofcode.com/2020) in Clojure.

## Code structure

I use one namespace per day, named `advent-of-code-2020.dayN` for day `N`.

Each namespace contains the functions required to solve both puzzles of the day.
The functions `solve-1` and `solve-2` solve the first and second puzzle of the day, respectively.
Inputs are passed in as parameters. The puzzle inputs are def'ed in the day namespace.

`advent-of-code-2020.core/-main` prints the solutions to all puzzles.

## License

I place my solutions in the public domain. Feel free to use them however you see fit.
