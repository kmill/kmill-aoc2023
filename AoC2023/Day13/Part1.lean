import AoC2023.Util

namespace Day13_1

open Lean (Parsec)
open Lean.Parsec

def newline : Parsec Unit :=
  (skipChar '\r' *> skipChar '\n') <|> skipChar '\n'

/-- `true` for rocks ('#') -/
def parseLine : Parsec (Array Bool) :=
  many1 ((skipChar '#' *> pure true) <|> (skipChar '.' *> pure false))

def parseBlock : Parsec (Array (Array Bool)) :=
  many1 (parseLine <* (newline <|> pure ()))

def parseBlocks : Parsec (Array (Array (Array Bool))) :=
  many (parseBlock <* many newline)

def transpose [Inhabited α] (mat : Array (Array α)) : Array (Array α) := Id.run do
  let mut mat' : Array (Array α) := Array.mkArray mat[0]!.size #[]
  for row in mat do
    for i in [0:row.size] do
      mat' := mat'.modify i (fun row' => row'.push row[i]!)
  return mat'

/-- info: #[#[1, 4], #[2, 5], #[3, 6]] -/
#guard_msgs in #eval transpose #[#[1,2,3],#[4,5,6]]

def findRefl (mat : Array (Array Bool)) : Option Nat := Id.run do
  for i in [0:mat.size] do
    let len := min (i + 1) (mat.size - (i + 1))
    if len == 0 then continue
    let check (i len : Nat) : Bool := Id.run do
      --dbg_trace s!"{len} {i}"
      for j in [0:len] do
        --dbg_trace s!"  {i-j}, {i+j+1}"
        if mat[i - j]! != mat[i + j + 1]! then
          return false
      return true
    if check i len then
      return i
  return none

partial def process (input : String) : IO Unit := do
  let blocks ← IO.ofExcept <| parseBlocks.run input
  let mut sum := 0
  for b in [0:blocks.size] do
    let block := blocks[b]!
    if let some i := findRefl block then
      sum := sum + 100 * (i + 1)
    else if let some i := findRefl (transpose block) then
      sum := sum + (i + 1)
    else
      IO.println s!"error in {b}"
  --let res := data.map fun (s, inop) => count s inop
  --dbg_trace s!"{repr res}"
  IO.println s!"{sum}"


def input1 :=
"#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#"

/-- info: 405 -/
#guard_msgs in
#eval process input1

def input2 :=
"###...###..
###...###..
#.#.#.#...#
...#..##.#.
...#..#....
.###.#.###.
#..##.###..
..#..#.#.##
..#..#.#.##
#..##.###..
.###.#.###.
...#..#....
...#..##.#.
#.#.#.#.#.#
###...###.."

/-- info: 100 -/
#guard_msgs in
#eval process input2

@[aoc_main day13_1]
def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let input ← IO.FS.readFile filename
  process input
