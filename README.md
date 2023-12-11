# Advent-of-Code-2023
[Advent of Code 2023](https://adventofcode.com/2023) - An attempt in Haskell.

# Code Summaries

<details><summary>## Day 1: Trebuchet?!</summary>

Part 1 works by simply extracting all the digits from each line, and then reading the number made from the first and last digit.

Part 2 is much trickier, but can be solved by stopping at every position in the line and testing whether a digit name (or digit itself) matches (as prefix of) the remaining string.

</details>

<details><summary>## [Day 2: Cube Conundrum](https://adventofcode.com/2023/day/2)</summary>

The main work is done in the parsing of the input:
We end up with a list of numbered 'games' which are essentially lists of `(r,g,b)` triples.

In Part 1 we just filter by games which only have valid triples.

In Part 2 we can just go over all triples of a game and to find the maximum `(r,g,b)` values.

</details>
