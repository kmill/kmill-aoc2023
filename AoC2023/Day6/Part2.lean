import AoC2023.Util

namespace Day6_2

open Lean (Parsec)
open Lean.Parsec

def nat' : Parsec Nat := do
  let parts ← many1 (many1Chars digit <* ws)
  let s := String.join parts.toList
  return s.toNat!

def parseFile : Parsec (Nat × Nat) := do
  ws *> skipString "Time:" *> ws
  let time ← nat'
  ws *> skipString "Distance:" *> ws
  let dist ← nat'
  return (time, dist)

-- Last time we did a brute force solution.
-- Maybe this time we solve for the bounds using a binary search.

/-- Look for the least value that makes `f` nonzero. `f` should be monotonic increasing on `[lo...hi]`. -/
partial def binSearch (f : Nat → Nat) (lo hi : Nat) : Nat :=
  --dbg_trace s!"{lo} {hi}"
  if lo ≥ hi then
    lo
  else
    let mid := (lo + hi) / 2
    let fmid := f mid
    if fmid = 0 then
      binSearch f (mid + 1) hi
    else
      binSearch f lo mid

-- Counting moves could probably done with the quadratic formula and thinking carefully about rounding...
def feasibleRange (time record : Nat) : Nat × Nat :=
  let i := binSearch (fun h => (time - h) * h - record) 1 (time / 2)
  (i, time - i)

def process (input : String) : IO Unit := do
  let (time, record) ← IO.ofExcept <| parseFile.run input
  let (lo, hi) := feasibleRange time record
  --IO.println s!"{lo} {hi}"
  IO.println s!"{hi - lo + 1}"

def exampleInput :=
"Time:      7  15   30
Distance:  9  40  200"

/-- info: 71503 -/
#guard_msgs in #eval process exampleInput

@[aoc_main day6_2]
def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let input ← IO.FS.readFile filename
  process input
