import AoC2023.Util

namespace Day4_1

open Lean (Parsec)
open Lean.Parsec

def parseLine : Parsec (Nat × Array Nat × Array Nat) := do
  ws *> skipString "Card" *> ws
  let n ← nat
  ws *> skipString ":" *> ws
  let winning ← many (nat <* ws)
  ws *> skipChar '|' *> ws
  let numbersHave ← many (nat <* ws)
  ws *> eof
  return (n, winning, numbersHave)

def scoreLine (winning numbersHave : Array Nat) : Nat := Id.run do
  let mut score := 0
  for n in numbersHave do
    if winning.contains n then
      score := max 1 (2 * score)
  return score

def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let lines ← IO.FS.lines filename
  let mut sum := 0
  for line in lines do
    let (n, winning, numbersHave) ← IO.ofExcept <| parseLine.run line
    --dbg_trace "Game {n} {winning} {numbersHave}"
    sum := sum + scoreLine winning numbersHave
  IO.println s!"{sum}"
