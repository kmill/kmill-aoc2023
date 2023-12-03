import AoC2023.Day1.Part1
import AoC2023.Day1.Part2
import AoC2023.Day2.Part1
import AoC2023.Day2.Part2
import AoC2023.Day3.Main

/-!
Rather than modifying the lakefile (which causes a rebuild of std),
each program is added to the `aoc` executable.
-/

def main (args : List String) : IO Unit := do
  let cmd :: args := args | throw <| IO.userError "first argument must be a command name"
  match cmd with
  | "day1_1" => Day1_1.main args
  | "day1_2" => Day1_2.main args
  | "day2_1" => Day2_1.main args
  | "day2_2" => Day2_2.main args
  | "day3_1" => Day3.main1 args
  | "day3_2" => Day3.main2 args
  | _ => throw <| IO.userError "first argument is not a recognized command"
