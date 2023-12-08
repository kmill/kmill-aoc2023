import AoC2023.Util

namespace Day8_2

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
  let node ← manyChars (digit <|> asciiLetter)
  ws; skipChar '='; ws; skipChar '('
  let left ← manyChars (digit <|> asciiLetter)
  ws; skipChar ','; ws
  let right ← manyChars (digit <|> asciiLetter)
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

partial def process (input : String) : IO Unit := do
  let (dirs, map) ← IO.ofExcept <| parseFile.run input
  let starts : Array String := map.toArray |>.map (fun (k, _) => k) |>.filter fun k => k.endsWith "A"
  let nstepss := starts.map (go map (.init dirs) · 0)
  let nsteps := nstepss.foldl Nat.lcm 1
  IO.println s!"{nsteps}"
where
  go (map : Std.HashMap String (String × String)) (s : InstrState) (pos : String) (nsteps : Nat) : Nat :=
    --dbg_trace "pos = {pos}"
    if pos.endsWith "Z" then
      nsteps
    else
      let (d, s) := s.next
      go map s (d.select <| map.findD pos ("ZZZ", "ZZZ")) (nsteps + 1)

def exampleInput :=
"LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"

/-- info: 6 -/
#guard_msgs in #eval process exampleInput

@[aoc_main day8_2]
def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let input ← IO.FS.readFile filename
  process input
