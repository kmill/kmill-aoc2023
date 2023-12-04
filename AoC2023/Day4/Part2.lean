import AoC2023.Util

namespace Day4_2

open Lean (Parsec)
open Lean.Parsec

def parseLine : Parsec (Nat × Array Nat × Array Nat) := do
  ws *> skipString "Card" *> ws
  let n ← nat
  ws *> skipString ":" *> ws
  let winning ← many (nat <* ws)
  ws *> skipChar '|' *> ws
  let numbersHave ← many (nat <* ws)
  ws *> eof
  return (n, winning, numbersHave)

def runParseLine (s : String) : Except String (Nat × Array Nat × Array Nat) :=
  match parseLine s.mkIterator with
  | .success _ res => .ok res
  | .error it err  => .error s!"offset {repr it.i.byteIdx}: {err}"

def numWinning (winning numbersHave : Array Nat) : Nat :=
  numbersHave |>.filter (winning.contains ·) |>.size

def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let lines ← IO.FS.lines filename
  let mut games : Array Nat := #[]
  for line in lines do
    let (n, winning, numbersHave) ← IO.ofExcept <| runParseLine line
    unless n = games.size + 1 do throw <| IO.userError "game numbers not 1, 2, 3, ..."
    let k := numWinning winning numbersHave
    games := games.push k
  --dbg_trace "games = {games}"
  let mut points : Array Nat := Array.mkArray games.size 0
  for i' in [0 : games.size] do
    let i := games.size - 1 - i'
    let k := games[i]!
    let mut p := 1
    for j in [0 : k] do
      p := p + points[i + 1 + j]!
    points := points.set! i p
  IO.println s!"{points.foldl (· + ·) 0}"
