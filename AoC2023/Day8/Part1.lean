import AoC2023.Util

namespace Day8_1

open Lean (Parsec)
open Lean.Parsec

inductive Dir | L | R
  deriving Repr, BEq, Inhabited

def Dir.select (d : Dir) (p : α × α) : α :=
  match d with
  | L => p.1
  | R => p.2

def parseInstr : Parsec Dir := do
  (skipChar 'L' *> pure Dir.L) <|> (skipChar 'R' *> pure Dir.R)

def parseInstrs : Parsec (Array Dir) :=
  many parseInstr

def parseNode : Parsec (String × String × String) := do
  let node ← manyChars asciiLetter
  ws; skipChar '='; ws; skipChar '('
  let left ← manyChars asciiLetter
  ws; skipChar ','; ws
  let right ← manyChars asciiLetter
  ws; skipChar ')'
  return (node, left, right)

def parseFile : Parsec (Array Dir × Std.HashMap String (String × String)) := do
  let instrs ← parseInstrs
  ws
  let nodes ← many1 (parseNode <* ws)
  ws
  let map := nodes.foldl (init := {}) (fun m (node, p) => m.insert node p)
  return (instrs, map)

structure InstrState where
  instrs : Array Dir
  idx : Nat

def InstrState.init (instrs : Array Dir) : InstrState := {instrs, idx := 0}

def InstrState.next (s : InstrState) : Dir × InstrState :=
  (s.instrs[s.idx]!, {s with idx := (s.idx + 1) % s.instrs.size})

/-
-- This version causes panics in `Std.HashMap.find!`?
def process (input : String) : IO Unit := do
  let (dirs, map) ← IO.ofExcept <| parseFile.run input
  --dbg_trace "map = {map.toArray}"
  let mut pos := "AAA"
  let mut s := InstrState.init dirs
  let mut nsteps := 0
  while pos != "ZZZ" do
    --dbg_trace "pos = {pos}"
    let (d, s') := s.next
    s := s'
    pos := d.select <| map.find! pos
    nsteps := nsteps + 1
    -- else
    --   dbg_trace "Error! {pos}"
    --   break
  IO.println s!"{nsteps}"
-/

partial def process (input : String) : IO Unit := do
  let (dirs, map) ← IO.ofExcept <| parseFile.run input
  let nsteps := go map (.init dirs) "AAA" 0
  IO.println s!"{nsteps}"
where
  go (map : Std.HashMap String (String × String)) (s : InstrState) (pos : String) (nsteps : Nat) : Nat :=
    --dbg_trace "pos = {pos}"
    if pos == "ZZZ" then
      nsteps
    else
      let (d, s) := s.next
      go map s (d.select <| map.findD pos ("ZZZ", "ZZZ")) (nsteps + 1)

def exampleInput :=
"RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"

/-- info: 2 -/
#guard_msgs in #eval process exampleInput

def exampleInput2 :=
"LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"

/--
info: 6
-/
#guard_msgs in #eval process exampleInput2

@[aoc_main day8_1]
def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let input ← IO.FS.readFile filename
  process input
