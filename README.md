# Advent-of-Code-2023
[Advent of Code 2023](https://adventofcode.com/2023) - An attempt in Haskell.

## Day Summaries

To explain how each day is approached see the following summaries (spoilers!)


<details>
<summary>

### Day 1: [Trebuchet?!](https://adventofcode.com/2023/day/1)

</summary>

Day 1 is about parsing some numbers out of some lines.
- Part 1 works by simply extracting all the digits from each line, and then reading the number made from the first and last digit.
- Part 2 is trickier, and can be solved by stopping at every position in the line and testing whether a digit name (or digit itself) matches (as prefix of) the remaining string.
</details>


<details>
<summary>

### Day 2: [Cube Conundrum](https://adventofcode.com/2023/day/2)

</summary>

Day 2 is about handling sequences of number triples.
- The main work is done in the parsing of the input:
We end up with a list of numbered 'games' which are essentially lists of `(r,g,b)` triples.
- In Part 1 we just filter by games which only have valid triples.
- In Part 2 we can just go over all triples of a game and to find the maximum `(r,g,b)` values.
</details>


<details>
<summary>

### Day 3: [Gear Ratios](https://adventofcode.com/2023/day/3)

</summary>

Day 3 is about parsing some numbers and symbols from a grid, and doing operations on the numbers depending on nearby symbols.
- The parsing carries a bit (prepares all the numbers and symbols) and the `adjacent` function handily calculates all valid neighboring coordinates.
- In Part 1 we filter all numbers which have any neighboring symbols.
- Part 2 is about going through all stars, finding their neighboring numbers and multiplying them out if there are exactly two.
</details>


<details>
<summary>

### Day 4: [Scratchcards](https://adventofcode.com/2023/day/4)

</summary>

Day 4 is about counting duplicates between number lists, then using these counts to do some DP-like number cascading to calculate a final sum.
- In Part 1 we just calculate the duplicates
- Part 2 additionally does some cascading operation, which can luckily be done quite elegantly and efficiently.
</details>


<details>
<summary>

### Day 5: [If You Give A Seed A Fertilizer](https://adventofcode.com/2023/day/5)

</summary>

Day 5 is about mapping numbers between intervals, and then chaining several of these mappings.
- Part 1 focuses on creating the simple mappings (`converter`) and feeding through them the individual seed values.
- Part 2 is actually analogous but complicated by the fact that we must now map entire seed ranges (which might be cut into separate smaller ranges by the mappings).
</details>


<details>
<summary>

### Day 6: [Wait For It](https://adventofcode.com/2023/day/6)

</summary>

Day 6 is about finding the size of some intervals containing valid numbers (keyword: quadratic equation).
- Part 1 simply bruteforces by checking all the numbers in the interval that satisfy the predicate.
- Part 2 is easily feasible by bruteforce but can be efficiently and not much more complicatedly solved by finding roots of quadratics.
</details>


<details>
<summary>

### Day 7: [Camel Cards](https://adventofcode.com/2023/day/7)

</summary>

Day 7 is about sorting some poker cards and then doing some evaluations based on the result.
- Part 1 can simply sort the hand values by relying on the fact that the are actually in reverse lexicographic order.
- Part 2 does the same but must first account for jokers which are strategically used to modify the hand value now (luckily in a straighforward way).
</details>


<details>
<summary>

### Day 8: [Haunted Wasteland](https://adventofcode.com/2023/day/8)

</summary>

Day 8 is about following some paths (/'multiple at once') until a node is reached (keyword: least common multiple).
- We parse the 'graph' into a map for quicker access.
- Part 1 does a simple walk from start to finish.
- Part 2 does walks for all possible starts to their first finish, then takes the `lcm` of all.
</details>


<details>
<summary>

### Day 9: [Mirage Maintenance](https://adventofcode.com/2023/day/9)

</summary>

Day 9 is about extrapolating some number sequences.
- Part 1 simply makes use of a recursive extrapolation function,
- while Part 2 does the same but reverses the list to extrapolate.
</details>


<details>
<summary>

### Day 10: [Pipe Maze](https://adventofcode.com/2023/day/10)

</summary>

Day 10 is about navigating a grid loop and calculating some area enclosed by it.
- The parsing mainly prepares the grid as an array and als finds (and fixes) the start position.
- Part 1 is about performing breadth-first search to map out the loop (and find the largest distance in it).
- Part 2 is about determining which of the non-loop cells are enclosed by the loop. This is done by marching left and checking how often the loop boundary is crossed (`odd` is inside and outside otherwise).
</details>


<details>
<summary>

### Day 11: [Cosmic Expansion](https://adventofcode.com/2023/day/11)

</summary>

Day 11 is about finding distances between grid points given that the grid has some expansion factor.
- Part 1 is solved naïvely by inserting additional rows and then calculating the distance between the indexed galaxies.
- Part 2 is done by doing custom indexing in each direction instead.
</details>
