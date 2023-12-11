# Advent-of-Code-2023
[Advent of Code 2023](https://adventofcode.com/2023) - An attempt in Haskell.

## Day Summaries

To explain how each day is approached see the following summaries (spoilers!)


<details>
<summary>

### Day 1: [Trebuchet?!](https://adventofcode.com/2023/day/1)

</summary>
Day 1 is about parsing some numbers out of some lines.

Part 1 works by simply extracting all the digits from each line, and then reading the number made from the first and last digit.

Part 2 is trickier, and can be solved by stopping at every position in the line and testing whether a digit name (or digit itself) matches (as prefix of) the remaining string.
</details>


<details>
<summary>

### Day 2: [Cube Conundrum](https://adventofcode.com/2023/day/2)

</summary>
Day 2 is about handling sequences of number triples.

The main work is done in the parsing of the input:
We end up with a list of numbered 'games' which are essentially lists of `(r,g,b)` triples.

In Part 1 we just filter by games which only have valid triples.

In Part 2 we can just go over all triples of a game and to find the maximum `(r,g,b)` values.
</details>


<details>
<summary>

### TODO Day 3: [Gear Ratios](https://adventofcode.com/2023/day/3)

</summary>
Day 3 is about parsing some numbers and symbols from a grid, and doing operations on the numbers depending on nearby symbols.

Part 1

Part 2
</details>


<details>
<summary>

### TODO Day 4: [Scratchcards](https://adventofcode.com/2023/day/4)

</summary>
Day 4 is about counting duplicates between number lists, then using these counts to do some DP-like number cascading to calculate a final sum.

Part 1

Part 2
</details>


<details>
<summary>

### TODO Day 5: [If You Give A Seed A Fertilizer](https://adventofcode.com/2023/day/5)

</summary>
Day 5 is about mapping numbers between intervals, and then chaining several of these mappings.

Part 1

Part 2
</details>


<details>
<summary>

### TODO Day 6: [Wait For It](https://adventofcode.com/2023/day/6)

</summary>
Day 6 is about finding the size of some intervals containing valid numbers (keyword: quadratic equation).

Part 1

Part 2
</details>


<details>
<summary>

### TODO Day 7: [Camel Cards](https://adventofcode.com/2023/day/7)

</summary>
Day 7 is about sorting some poker cards and then doing some evaluations based on the result.

Part 1

Part 2
</details>


<details>
<summary>

### TODO Day 8: [Haunted Wasteland](https://adventofcode.com/2023/day/8)

</summary>
Day 8 is about following some paths (/'multiple at once') until a node is reached (keyword: least common multiple).

Part 1

Part 2
</details>


<details>
<summary>

### TODO Day 9: [Mirage Maintenance](https://adventofcode.com/2023/day/9)

</summary>
Day 9 is about extrapolating some number sequences.

Part 1

Part 2
</details>


<details>
<summary>

### TODO Day 10: [Pipe Maze](https://adventofcode.com/2023/day/10)

</summary>
Day 10 is about navigating a grid loop and calculating some area enclosed by it.

Part 1

Part 2
</details>


<details>
<summary>

### TODO Day 11: [Cosmic Expansion](https://adventofcode.com/2023/day/11)

</summary>
Day 11 is about finding distances between grid points given that the grid has some expansion factor.

Part 1

Part 2
</details>
