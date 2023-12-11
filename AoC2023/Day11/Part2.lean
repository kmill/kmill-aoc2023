import AoC2023.Util

namespace Day11_2

/-!
Since we set this up by adjusting the graph metric, for part two we can just increase those weights.
-/

/-- An entry is `true` if there is a galaxy there. -/
abbrev Grid := Array (Array Bool)

def Grid.get (g : Grid) (p : Nat × Nat) : Bool :=
  if p.1 ≥ g.size then
    false
  else
    let row := g[p.1]!
    if p.2 ≥ row.size then
      false
    else
      row[p.2]!

def Grid.numRows (g : Grid) : Nat := g.size
def Grid.numCols (g : Grid) : Nat := g[0]!.size

def parseGrid (input : String) : Grid :=
  let lines := input.split (· == '\n') |>.filter (·.length != 0)
  lines.toArray.map (·.toList.toArray |>.map (· == '#'))

def Grid.galaxies (g : Grid) : Array (Nat × Nat) := Id.run do
  let mut set : Array (Nat × Nat) := #[]
  for i in [0:g.numRows] do
    for j in [0:g.numCols] do
      if g.get (i, j) then
        set := set.push (i, j)
  return set


/-- Returns an array where position `i` is `true` iff there is no galaxy in row `i`. -/
def Grid.expandedRows (g : Grid) : Array Bool :=
  g.map (fun row => row.all (· == false))

/-- Returns an array where position `i` is `true` iff there is no galaxy in column `i`. -/
def Grid.expandedCols (g : Grid) : Array Bool :=
  g.foldl (init := g[0]!) (start := 1) (fun r1 r2 => r1.zipWith r2 or) |>.map not

/-- Foldl, but collect all states *after* `init`. -/
def collectFoldl {α σ : Type _} (xs : Array α) (init : σ) (f : σ → α → σ) : Array σ :=
  let (ss, _) := xs.foldl (init := (#[], init)) fun (arr, s) x => let s' := f s x; (arr.push s', s')
  ss

/-- info: #[1, 3, 6] -/
#guard_msgs in #eval collectFoldl #[1,2,3] 0 (· + ·)

def natDist (a b : Nat) : Nat :=
  if a ≤ b then b - a else a - b

partial def process (input : String) (factor : Nat := 1000000): IO Unit := do
  let g : Grid := parseGrid input
  let erows := g.expandedRows
  let ecols := g.expandedCols
  -- Convert these into counts of how many expanded rows/cols there are up to and including this point.
  -- This way we can efficiently count how many there are between any two points.
  let erows : Array Nat := collectFoldl erows 0 (fun s e => s + if e then 1 else 0)
  let ecols : Array Nat := collectFoldl ecols 0 (fun s e => s + if e then 1 else 0)
  --dbg_trace "erows = {erows}, ecols = {ecols}"
  let galaxies := g.galaxies
  --dbg_trace "galaxies = {galaxies}"
  let mut sum := 0
  for i in [0:galaxies.size] do
    let g1 := galaxies[i]!
    for j in [i+1:galaxies.size] do
      let g2 := galaxies[j]!
      let preDist := natDist g1.1 g2.1 + natDist g1.2 g2.2
      let expansion := natDist erows[g2.1]! erows[g1.1]! + natDist ecols[g2.2]! ecols[g1.2]!
      let dist := preDist + expansion * (factor - 1)
      --dbg_trace s!"({i+1}, {j+1}) => {dist}"
      sum := sum + dist
  IO.println s!"{sum}"


def input1 :=
"...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....."

/-- info: 1030 -/
#guard_msgs in #eval process input1 (factor := 10)

/-- info: 8410 -/
#guard_msgs in #eval process input1 (factor := 100)

@[aoc_main day11_2]
def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let input ← IO.FS.readFile filename
  process input
