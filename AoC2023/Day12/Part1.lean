import AoC2023.Util

namespace Day12_1

open Lean (Parsec)
open Lean.Parsec

inductive Entry
  | Operational | Unknown | Damaged
  deriving Repr, BEq

def parseEntry : Parsec Entry :=
  (skipChar '.' *> pure .Operational) <|> (skipChar '?' *> pure .Unknown) <|> (skipChar '#' *> pure .Damaged)

def parseState : Parsec (List Entry) := do
  let s ← many1 parseEntry
  return s.toList

def parseOther : Parsec (List Nat) := do
  let fst ← nat
  let rest ← many (skipChar ',' *> nat)
  return fst :: rest.toList

def parseLine : Parsec (List Entry × List Nat) := do
  let s ← parseState
  ws
  let oth ← parseOther
  ws
  return (s, oth)

def parseFile : Parsec (Array (List Entry × List Nat)) := do
  many parseLine

def consume (state : List Entry) (count : Nat) : Option (List Entry) :=
  match state, count with
  | [], 0 => some []
  | [], (n + 1) => none
  | .Operational :: s, 0 => s
  | .Operational :: _, (n + 1) => none
  | .Damaged :: s, 0 => none
  | .Damaged :: s, (n + 1) => consume s n
  | .Unknown :: s, 0 => s
  | .Unknown :: s, (n + 1) => consume s n

partial def count (state : List Entry) (inop : List Nat) : Nat :=
  match state, inop with
  | [], _ :: _ => 0
  | _, [] => if state.all (· != .Damaged) then 1 else 0
  | .Operational :: state, inop => count state inop
  | .Damaged :: _, dmg :: inop =>
    if let some state' := consume state dmg then
      count state' inop
    else
      0
  | .Unknown :: state', dmg :: inop' =>
    (if let some state' := consume state dmg then count state' inop' else 0)
      + count state' inop

partial def process (input : String) : IO Unit := do
  let data ← IO.ofExcept <| parseFile.run input
  let res := data.map fun (s, inop) => count s inop
  --dbg_trace s!"{repr res}"
  IO.println s!"{res.foldl (· + ·) 0}"


def input1 :=
"???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"

/-- info: 21 -/
#guard_msgs in
#eval process input1

@[aoc_main day12_1]
def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let input ← IO.FS.readFile filename
  process input
