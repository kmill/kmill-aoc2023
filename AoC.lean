import AoC2023.Day1.Part1
import AoC2023.Day1.Part2

/-!
Rather than modifying the lakefile (which causes a rebuild of std),
each program is added to the `aoc` executable.
-/

def main (args : List String) : IO Unit := do
  let cmd :: args := args | throw <| IO.userError "first argument must be a command name"
  match cmd with
  | "day1_1" => Day1_1.main args
  | "day1_2" => Day1_2.main args
  | _ => throw <| IO.userError "first argument is not a recognized command"
