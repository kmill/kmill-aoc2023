import AoC2023.Util

namespace Day12_2

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

partial def count (state : List Entry) (inop : List Nat) :
    StateM (Lean.HashMap (Nat × Nat) Nat) Nat := do
  if let some res := (← get).find? (state.length, inop.length) then
    return res
  else
    let res ← do
      match state, inop with
      | [], _ :: _ => pure 0
      | _, [] => pure <| if state.all (· != .Damaged) then 1 else 0
      | .Operational :: state, inop => count state inop
      | .Damaged :: _, dmg :: inop =>
        if let some state' := consume state dmg then
          count state' inop
        else
          pure 0
      | .Unknown :: state', dmg :: inop' =>
        let r1 ← if let some state' := consume state dmg then count state' inop' else pure 0
        let r2 ← count state' inop
        pure <| r1 + r2
    modify (fun m => m.insert (state.length, inop.length) res)
    return res

partial def process (input : String) : IO Unit := do
  let data ← IO.ofExcept <| parseFile.run input
  let res := data.map fun (s, inop) =>
    let s' := List.intercalate [.Unknown] (List.replicate 5 s)
    let inop' := List.join (List.replicate 5 inop)
    count s' inop' |>.run' {}
  --dbg_trace s!"{repr res}"
  IO.println s!"{res.foldl (· + ·) 0}"


def input1 :=
"???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"

/-- info: 525152 -/
#guard_msgs in #eval process input1

@[aoc_main day12_2]
def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let input ← IO.FS.readFile filename
  process input
