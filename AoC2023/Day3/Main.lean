import AoC2023.Util

namespace Day3

/-- A number on the schematic -/
structure Num where
  /-- The number -/
  n : Nat
  /-- The starting position in the grid -/
  p : Nat × Nat
  /-- The length of the number in cells -/
  len : Nat
  deriving DecidableEq, Hashable

def Num.append (num : Num) (d : Nat) : Num :=
  {num with n := num.n * 10 + d, len := num.len + 1}

/-- A map from positions to the number that is present there (if any). -/
abbrev PosMap := Std.HashMap (Nat × Nat) Num

/-- Insert a `Num` that is `len` cells wide. -/
def PosMap.insertNum (m : PosMap) (x : Num) (len : Nat) : PosMap := Id.run do
  let mut m := m
  for i in [0:len] do
    m := m.insert (x.p.1, x.p.2 + i) x
  return m

structure State where
  /-- The current position -/
  p : Nat × Nat := (0, 0)
  /-- Array of positions of symbols. -/
  symbols : Array (Nat × Nat) := #[]
  /-- Whether we are currently visiting a num -/
  num? : Option Num := none
  /-- The PosMap of numbers -/
  posMap : PosMap := {}

/-- Commit a number in progress, if any. -/
def State.commit (s : State) : State :=
  if let some num := s.num? then
    {s with num? := none, posMap := s.posMap.insertNum num num.len}
  else
    s

/-- Process a digit, updating the `num?` state. -/
def State.digit (s : State) (d : Nat) : State :=
  if let some num := s.num? then
    {s with num? := some (num.append d)}
  else
    {s with num? := some {n := d, p := s.p, len := 1}}

/-- Handle end of a line -/
def State.endl (s : State) : State :=
  let s := s.commit
  {s with p := (s.p.1 + 1, 0)}

def State.addSymbol (s : State) : State :=
  {s with symbols := s.symbols.push s.p}

def State.incP (s : State) : State :=
  {s with p := (s.p.1, s.p.2 + 1)}

def State.read1 (s : State) (c : Char) : State :=
  if c == '.' then
    s |>.commit |>.incP
  else if let some d := c.digit? then
    s |>.digit d |>.incP
  else
    s |>.commit |>.addSymbol |>.incP

def main1 (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let lines ← IO.FS.lines filename
  let mut s : State := {}
  for line in lines do
    s := line.foldl (init := s) fun s c => s.read1 c
    s := s.endl
  let mut seen : Lean.HashSet Num := {}
  let mut sum := 0
  for p in s.symbols do
    for i in [p.1-1:p.1+2] do
      for j in [p.2-1:p.2+2] do
        if let some num := s.posMap.find? (i, j) then
          unless seen.contains num do
            seen := seen.insert num
            sum := sum + num.n
  IO.println s!"{sum}"

def main2 (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let lines ← IO.FS.lines filename
  let mut sum := 0
  IO.println s!"{sum}"
