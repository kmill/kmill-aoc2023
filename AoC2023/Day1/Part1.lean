import AoC2023.Util

namespace Day1_1

/-- Collect the digits appearing in `s`. -/
def getDigits (s : String) : Array Nat :=
  s.foldl (init := #[]) fun acc c =>
    if let some d := c.digit? then
      acc.push d
    else
      acc

/-- info: #[2, 3, 4] -/
#guard_msgs in #eval getDigits "a2b34c"

def getCalibration (s : String) : Nat :=
  let digits := getDigits s
  10*digits[0]! + digits[digits.size-1]!

/-- info: 49 -/
#guard_msgs in #eval getCalibration "znzdrj4526fjtszspfour9pk"

def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let lines ← IO.FS.lines filename
  let sum := lines.foldl (init := 0) (· + getCalibration ·)
  IO.println s!"{sum}"
