import AoC2023.Util
import AoC2023.Day1.Part1
import AoC2023.Day1.Part2
import AoC2023.Day2.Part1
import AoC2023.Day2.Part2
import AoC2023.Day3.Part1
import AoC2023.Day3.Part2
import AoC2023.Day4.Part1
import AoC2023.Day4.Part2
import AoC2023.Day5.Part1
import AoC2023.Day5.Part2
import AoC2023.Day6.Part1
import AoC2023.Day6.Part2
import AoC2023.Day7.Part1
import AoC2023.Day7.Part2
import AoC2023.Day8.Part1
import AoC2023.Day8.Part2
import AoC2023.Day9.Part1
import AoC2023.Day9.Part2
import AoC2023.Day10.Part1
import AoC2023.Day10.Part2

/-!
Rather than modifying the lakefile (which causes a rebuild of std),
each program is added to the `aoc` executable.

The `aoc_dispatch%` elaborator creates a dispatcher to every function with the `@[aoc_main cmdName]` attribute.
-/

def main (args : List String) : IO Unit := do
  aoc_dispatch% args
