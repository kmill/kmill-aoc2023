import AoC2023.Day2.Format

namespace Day2_2

open Day2

def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let lines ← IO.FS.lines filename
  let mut sum := 0
  for line in lines do
    let (n, cs) ← IO.ofExcept <| parseLine.run line
    let m := Array.foldl (init := {}) max cs
    let p := m.r * m.g * m.b
    sum := sum + p
  IO.println s!"{sum}"
