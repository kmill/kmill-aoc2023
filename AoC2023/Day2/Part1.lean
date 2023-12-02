import AoC2023.Day2.Format

namespace Day2_1

open Day2

def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let lines ← IO.FS.lines filename
  let mut sum := 0
  for line in lines do
    let (n, cs) ← IO.ofExcept <| runParseLine line
    let m := Array.foldl (init := {}) max cs
    if m.r ≤ 12 ∧ m.g ≤ 13 ∧ m.b ≤ 14 then
      sum := sum + n
  IO.println s!"{sum}"
