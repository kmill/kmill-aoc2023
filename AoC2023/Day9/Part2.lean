import AoC2023.Util

namespace Day9_2

/-!
For part 2, just have to reverse the array before processing.
-/

open Lean (Parsec)
open Lean.Parsec

def parseInt : Parsec Int :=
  (Int.ofNat <$> nat) <|> (skipChar '-' *> Int.negOfNat <$> nat)

def parseLine : Parsec (Array Int) :=
  many1 (parseInt <* ws)

def fwd_diffs (seq : Array Int) : Array Int := Id.run do
  let mut diffs : Array Int := #[]
  for i in [0:seq.size - 1] do
    diffs := diffs.push (seq[i+1]! - seq[i]!)
  return diffs

partial def extrapolate (seq : Array Int) : Int :=
  if seq.all (· == 0) then
    0
  else
    let diffs := fwd_diffs seq
    let diffs_next := extrapolate diffs
    seq[seq.size - 1]! + diffs_next

partial def process (input : String) : IO Unit := do
  let lines := input.split (· == '\n') |>.filter (·.length != 0)
  let mut sum := 0
  for line in lines do
    let seq ← IO.ofExcept <| (parseLine <* eof).run line
    sum := sum + extrapolate seq.reverse
  IO.println s!"{sum}"

def exampleInput :=
"0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"

/-- info: 2 -/
#guard_msgs in #eval process exampleInput

@[aoc_main day9_2]
def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let input ← IO.FS.readFile filename
  process input
