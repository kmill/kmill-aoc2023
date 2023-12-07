import AoC2023.Util

namespace Day6_1

open Lean (Parsec)
open Lean.Parsec

def parseFile : Parsec (Array (Nat × Nat)) := do
  ws *> skipString "Time:" *> ws
  let times ← many (nat <* ws)
  ws *> skipString "Distance:" *> ws
  let dists ← many (nat <* ws)
  return times.zip dists

-- Counting moves could probably done with the quadratic formula and thinking carefully about rounding...
def feasibleMoves (time record : Nat) : Array Nat := Id.run do
  let mut moves := #[]
  for i in [0:time] do
    if (time - i) * i > record then
      moves := moves.push i
  return moves

def process (input : String) : IO Unit := do
  let games ← IO.ofExcept <| parseFile.run input
  let mut prod := 1
  for (time, record) in games do
    let moves := feasibleMoves time record
    prod := prod * moves.size
  IO.println s!"{prod}"

def exampleInput :=
"Time:      7  15   30
Distance:  9  40  200"

/-- info: 288 -/
#guard_msgs in #eval process exampleInput

def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let input ← IO.FS.readFile filename
  process input
