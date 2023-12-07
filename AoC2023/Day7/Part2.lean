import AoC2023.Util

namespace Day7_2

open Lean (Parsec)
open Lean.Parsec

/-- Cards in order of increasing strength -/
def cardTypes : Array Char := "AKQT98765432J".toList.reverse.toArray

structure Hand where
  -- The cards
  (c1 c2 c3 c4 c5 : Nat)
  deriving Ord, Inhabited

def Hand.cards (h : Hand) : Array Nat := #[h.c1, h.c2, h.c3, h.c4, h.c5]

instance : ToString Hand where
  toString h := String.mk (h.cards.map (fun i => cardTypes[i]!)).toList

def parseCard : Parsec Nat := do
  let c ← peek!
  let some i := cardTypes.findIdx? (· = c) | fail "Expecting a card"
  skip
  return i

def parseHand : Parsec Hand := do
  let c1 ← parseCard
  let c2 ← parseCard
  let c3 ← parseCard
  let c4 ← parseCard
  let c5 ← parseCard
  return {c1, c2, c3, c4, c5}

def parseLine : Parsec (Hand × Nat) := do
  let hand ← parseHand
  ws
  let bid ← nat
  return (hand, bid)

inductive HandType
  | highCard
  | onePair
  | twoPair
  | threeOfAKind
  | fullHouse
  | fourOfAKind
  | fiveOfAKind
  deriving Ord, Repr, Inhabited

def Hand.evaluate (h : Hand) : HandType :=
  -- Non-joker cards
  let cards := h.cards.filter (· ≠ 0)
  let countsMap : Std.HashMap Nat Nat :=
    cards.foldl (init := {}) fun m c => m.insert c (1 + m.findD c 0)
  let counts := countsMap.fold (init := #[]) fun cs _ c => cs.push c
  let counts := counts.insertionSort (· > ·)
  match counts.toList with
  | [5] | [4] | [3] | [2] | [1] | [] => .fiveOfAKind
  | [4, 1] | [3, 1] | [2, 1] | [1, 1] => .fourOfAKind
  | [3, 2] | [2, 2] => .fullHouse
  | [3, 1, 1] | [2, 1, 1] | [1, 1, 1] => .threeOfAKind
  | [2, 2, 1] => .twoPair
  | [2, 1, 1, 1] | [1, 1, 1, 1] => .onePair
  | [1, 1, 1, 1, 1] => .highCard
  | _ => panic! "unhandled"

def testEval (s : String) : Except String HandType := do
  let h ← parseHand.run s
  return h.evaluate

/--
info: Except.ok #[Day7_2.HandType.onePair, Day7_2.HandType.fourOfAKind, Day7_2.HandType.twoPair,
Day7_2.HandType.fourOfAKind,
  Day7_2.HandType.fourOfAKind]
-/
#guard_msgs in
#eval #["32T3K", "T55J5", "KK677", "KTJJT", "QQQJA"].mapM testEval
/--
info: Except.ok #[Day7_2.HandType.fiveOfAKind, Day7_2.HandType.fourOfAKind, Day7_2.HandType.fullHouse,
  Day7_2.HandType.threeOfAKind, Day7_2.HandType.twoPair, Day7_2.HandType.onePair, Day7_2.HandType.highCard]
-/
#guard_msgs in
#eval #["22222", "22223", "22233", "22234", "22334", "22345", "23456"].mapM testEval

def parseFile : Parsec (Array (Hand × Nat)) :=
  many (parseLine <* ws)

deriving instance Ord for Prod

def process (input : String) : IO Unit := do
  let lines ← IO.ofExcept <| parseFile.run input
  let lines := lines.map fun (hand, bid) => ((hand.evaluate, hand), bid)
  let lines := lines.insertionSort (fun p1 p2 => compare p1.1 p2.1 == .lt)
  let mut winnings := 0
  for i in [0:lines.size] do
    let ((type, hand), bid) := lines[i]!
    --dbg_trace "hand = {hand}, type = {repr type}"
    winnings := winnings + (i + 1) * bid
  IO.println s!"{winnings}"

def exampleInput :=
"32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"

/-- info: 5905 -/
#guard_msgs in #eval process exampleInput

@[aoc_main day7_2]
def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let input ← IO.FS.readFile filename
  process input
