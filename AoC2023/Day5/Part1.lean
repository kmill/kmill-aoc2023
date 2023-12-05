import AoC2023.Util

namespace Day5_1

/-- Represents a mapping of the interval `[source:source+length]` to `[dest:dest+length]`.
If `length = none` then this is an unbounded interval. -/
structure MapRange where
  (source dest : Nat)
  length : Option Nat -- should be ℕ∞ but I didn't import mathlib

instance : Repr MapRange where
  reprPrec r _ := s!"⟨{r.source}, {r.dest}, {r.length}⟩"

/-- Identity range. -/
protected def MapRange.id : MapRange where
  source := 0
  dest := 0
  length := none

def MapRange.sourceEnd (r : MapRange) : Option Nat := r.length.map (fun length => r.source + length)
def MapRange.destEnd (r : MapRange) : Option Nat := r.length.map (fun length => r.dest + length)

def MapRange.sourceContains (r : MapRange) (i : Nat) : Bool :=
  r.source ≤ i && i < r.sourceEnd.getD (i + 1)

def MapRange.destContains (r : MapRange) (i : Nat) : Bool :=
  r.dest ≤ i && i < r.destEnd.getD (i + 1)

/-- Apply the `MapRange` to the natural number, if it's in range. -/
def MapRange.apply? (r : MapRange) (i : Nat) : Option Nat :=
  if r.sourceContains i then
    some <| i - r.source + r.dest
  else
    none

/-- Represents a mapping of `Nat` to `Nat` defined piecewise by the given array of `MapRange`s.
The domains of the `MapRange`s are assumed to be non-overlapping. -/
structure Mapping where
  maps : Array MapRange
  deriving Repr

protected def Mapping.id : Mapping where
  maps := #[.id]

/-- Applies an applicable `MapRange`, and otherwise returns `i`. -/
def Mapping.apply (m : Mapping) (i : Nat) : Nat := Id.run do
  for r in m.maps do
    if let some i' := r.apply? i then
      return i'
  return i

/-!
It is tempting to come up with a composition law for `Mapping`, since that way we can get
a full view of the seed-to-location map. Of course, we do not need to do this.
We can just use `Mapping.apply` mapping-by-mapping for the handful of seeds that are given to us.
However, I have been tempted, so composition law here we come! :-)
-/

/-- Sort the ranges and insert the omitted identity ranges.
Assumes the mapping is non-overlapping. -/
def Mapping.normalize (m : Mapping) : Mapping := Id.run do
  let maps := m.maps.filter (fun r => r.length != some 0)
  let maps := maps.insertionSort (fun r1 r2 => r1.source < r2.source)
  let mut maps' : Array MapRange := #[]
  let mut sIdx := 0
  for r in maps do
    if sIdx < r.source then
      -- Insert partial identity range
      maps' := maps'.push {source := sIdx, dest := sIdx, length := r.source - sIdx}
    maps' := maps'.push r
    if let some length := r.length then
      sIdx := r.source + length
    else
      return {maps := maps'}
  maps' := maps'.push {source := sIdx, dest := sIdx, length := none}
  return {maps := maps'}

def exMapping : Mapping where
  maps := #[MapRange.mk 49 53 (some 8), MapRange.mk 0 11 (some 42), MapRange.mk 42 0 (some 7), MapRange.mk 57 7 (some 4)]
def exMapping' : Mapping where
  maps := #[MapRange.mk 49 53 (some 8), MapRange.mk 0 11 (some 42), MapRange.mk 57 7 (some 4)]

/--
info: { maps := #[⟨0, 11, (some 42)⟩, ⟨42, 0, (some 7)⟩, ⟨49, 53, (some 8)⟩, ⟨57, 7, (some 4)⟩, ⟨61, 61, none⟩] }
-/
#guard_msgs in #eval exMapping.normalize
/--
info: { maps := #[⟨0, 11, (some 42)⟩, ⟨42, 42, (some 7)⟩, ⟨49, 53, (some 8)⟩, ⟨57, 7, (some 4)⟩, ⟨61, 61, none⟩] }
-/
#guard_msgs in #eval exMapping'.normalize

/-- Create a new `MapRange` by making the dest start at the index that's at least the given index. -/
def MapRange.restrictByDestStart (r : MapRange) (newStart : Nat) : MapRange :=
  if newStart ≤ r.dest then
    r
  else
    if r.destContains newStart then
      let d := newStart - r.dest
      {source := r.source + d, dest := r.dest + d, length := r.length.map (· - d)}
    else
      -- Will be filtered out
      {r with length := some 0}

/-- Create a new `MapRange` by making the dest end at the index that's at most the given index. -/
def MapRange.restrictByDestEnd (r : MapRange) (newEnd : Option Nat) : MapRange :=
  if let some newEnd := newEnd then
    if newEnd < r.destEnd.getD (newEnd + 1) then
      if r.destContains newEnd then
        {source := r.source, dest := r.dest, length := newEnd - r.dest}
      else
        -- Will be filtered out
        {r with length := some 0}
    else
      r
  else
    r

/-- Assumes `m` has been normalized. Computes restrictions of `r` to each `MapRange` in `m`. -/
def Mapping.compAux (r : MapRange) (m : Mapping) (acc : Array MapRange) : Array MapRange := Id.run do
  let mut maps := acc
  for r' in m.maps do
    let r := r |>.restrictByDestStart r'.source |>.restrictByDestEnd r'.sourceEnd
    if r.length != some 0 then
      maps := maps.push {r with dest := (r'.apply? r.dest).get!}
  return maps

/-- Compose two `Mapping`s.
Gives a mapping `m` such that `(m1.comp m2).apply i = m2.apply (m1.apply i)`. -/
partial def Mapping.comp (m1 m2 : Mapping) : Mapping := Id.run do
  let m1 := m1.normalize
  let m2 := m2.normalize
  let mut maps : Array MapRange := #[]
  for r1 in m1.maps do
    maps := Mapping.compAux r1 m2 maps
  -- No need to normalize
  return {maps := maps}

open Lean (Parsec)
open Lean.Parsec

def parseMapRange : Parsec MapRange := do
  let dest ← nat
  ws
  let source ← nat
  ws
  let length ← nat
  ws
  return {source, dest, length}

def parseMapping : Parsec Mapping := do
  let maps ← many parseMapRange
  ws
  return {maps}

def parseMap : Parsec (String × String × Mapping) := do
  let src ← manyChars asciiLetter
  skipString "-to-"
  let dst ← manyChars asciiLetter
  ws *> skipString "map:" *> ws
  let mapping ← parseMapping
  ws
  return (src, dst, mapping)

def parseAlmanac : Parsec (Array Nat × Array (String × String × Mapping)) := do
  ws
  skipString "seeds:" *> ws
  let seeds ← many (nat <* ws)
  let almanac ← many parseMap
  ws *> eof
  return (seeds, almanac)

def runParseAlmanac (s : String) : Except String (Array Nat × Array (String × String × Mapping)) :=
  match parseAlmanac s.mkIterator with
  | .success _ res => .ok res
  | .error it err  => .error s!"offset {repr it.i.byteIdx}: {err}"

def process (input : String) (debug : Bool := false) : IO Unit := do
  let (seeds, almanac) ← IO.ofExcept <| runParseAlmanac input
  if debug then
    IO.println s!"seeds = {seeds}\nalmanac = {almanac.map repr}"
  let mut mapping := Mapping.id
  let mut seeds' := seeds
  for (src, dst, m) in almanac do
    mapping := mapping.comp m
    seeds' := seeds'.map m.apply
    if debug then IO.println s!"seeds' = {seeds'}"
  if debug then IO.println s!"mapping = {repr mapping}"
  let seeds'' := seeds.map mapping.apply
  if seeds' != seeds'' then panic!"Composition law implemented incorrectly"
  IO.println s!"{seeds'.foldl min seeds'[0]!}"

def exampleInput :=
"seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"
/-- info: 35 -/
#guard_msgs in #eval process exampleInput

def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let input ← IO.FS.readFile filename
  process input
