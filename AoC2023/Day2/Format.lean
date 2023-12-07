import AoC2023.Util

namespace Day2

open Lean (Parsec)
open Lean.Parsec

def digit' : Parsec Nat := do
  let c ← digit
  return (c.val - 48).toNat

def nat : Parsec Nat := do
  return (← many1 digit').foldl (init := 0) (fun a d => 10 * a + d)

structure Colors where
  (r g b : Nat := 0)
  deriving Repr

def Colors.map (f : Nat → Nat) (x : Colors) : Colors :=
  {r := f x.r, g := f x.g, b := f x.b}

def Colors.map₂ (f : Nat → Nat → Nat) (x y : Colors) : Colors :=
  {r := f x.r y.r, g := f x.g y.g, b := f x.b y.b}

instance : Add Colors where
  add := Colors.map₂ (· + ·)

instance : HMul Nat Colors Colors where
  hMul n := Colors.map (n * ·)

instance : Max Colors where
  max := Colors.map₂ max

def parseRed : Parsec Colors := skipString "red" *> pure {r := 1, g := 0, b := 0}
def parseGreen : Parsec Colors := skipString "green" *> pure {r := 0, g := 1, b := 0}
def parseBlue : Parsec Colors := skipString "blue" *> pure {r := 0, g := 0, b := 1}

def parseColor : Parsec Colors := do
  let n ← nat
  ws
  let c ← parseRed <|> parseGreen <|> parseBlue
  return n * c

partial def parseColors : Parsec Colors := do
  go {}
where
  go (acc : Colors) : Parsec Colors := do
    ws
    let acc := acc + (← parseColor)
    ws
    if (← peek?) == ',' then
      skip
      go acc
    else
      return acc

partial def parseColorList : Parsec (Array Colors) := do
  go #[]
where
  go (acc : Array Colors) : Parsec (Array Colors) := do
    ws
    let acc := acc |>.push (← parseColors)
    ws
    if (← peek?) == ';' then
      skip
      go acc
    else
      return acc

def parseLine : Parsec (Nat × Array Colors) := do
  ws
  skipString "Game"
  ws
  let n ← nat
  ws
  skipString ":"
  let cs ← parseColorList
  ws <* eof
  return (n, cs)
