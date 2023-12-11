# Advent-of-Code-2023
[Advent of Code 2023](https://adventofcode.com/2023) - An attempt in Haskell.

# Code Summaries

<details>
<summary><h2><a href="https://adventofcode.com/2023/day/1">Day 1: Trebuchet?!</a></h2></summary>
<details>
Part 1 works by simply extracting all the digits from each line, and then reading the number made from the first and last digit.

Part 2 is trickier, and can be solved by stopping at every position in the line and testing whether a digit name (or digit itself) matches (as prefix of) the remaining string.
</details>

<details>
<summary><h2><a href="https://adventofcode.com/2023/day/2">Day 2: Cube Conundrum</a></h2></summary>
The main work is done in the parsing of the input:
We end up with a list of numbered 'games' which are essentially lists of `(r,g,b)` triples.

In Part 1 we just filter by games which only have valid triples.

In Part 2 we can just go over all triples of a game and to find the maximum `(r,g,b)` values.
</details>
