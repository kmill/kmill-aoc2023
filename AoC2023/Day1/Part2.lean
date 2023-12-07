import AoC2023.Util

namespace Day1_2

def spelledDigits : List (Nat × String) :=
  [(0, "zero"), (1, "one"), (2, "two"), (3, "three"), (4, "four"),
   (5, "five"), (6, "six"), (7, "seven"), (8, "eight"), (9, "nine")]
  ++ (List.range 10).map (fun i => (i, toString (Char.ofNat (i + 48))))

-- Generating a custom state machine would be more fun, or coming up with a monoid law, but this will do.

partial def getFirstDigit (s : String) : Option Nat := Id.run do
  let mut d? : Option Nat := none
  let mut pos : String.Pos := ⟨10000⟩
  for (d, ds) in spelledDigits do
    for m in s.findAllSubstr ds do
      if m.startPos ≤ pos then
        d? := d
        pos := m.startPos
  return d?

partial def getLastDigit (s : String) : Option Nat := Id.run do
  let mut d? : Option Nat := none
  let mut pos : String.Pos := ⟨0⟩
  for (d, ds) in spelledDigits do
    for m in s.findAllSubstr ds do
      if m.stopPos ≥ pos then
        d? := d
        pos := m.stopPos
  return d?

/-- info: some 2 -/
#guard_msgs in #eval getFirstDigit "a2b34c"
/-- info: some 4 -/
#guard_msgs in #eval getLastDigit "a2b34c"
/-- info: some 1 -/
#guard_msgs in #eval getFirstDigit "zoneight234"
/-- info: some 4 -/
#guard_msgs in #eval getLastDigit "zoneight234"
/-- info: some 7 -/
#guard_msgs in #eval getFirstDigit "7pqrstsixteen"
/-- info: some 6 -/
#guard_msgs in #eval getLastDigit "7pqrstsixteen"

def getCalibration? (s : String) : Option Nat := do
  let d1 ← getFirstDigit s
  let d2 ← getLastDigit s
  return 10 * d1 + d2

def getCalibration (s : String) : Nat := (getCalibration? s).getD 0

/-- info: 49 -/
#guard_msgs in #eval getCalibration "znzdrj4526fjtszspfour9pk"

@[aoc_main day1_2]
def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let lines ← IO.FS.lines filename
  let sum := lines.foldl (init := 0) (· + getCalibration ·)
  IO.println s!"{sum}"
