import AoC2023.Util

namespace Day10_1

abbrev Grid := Array (Array Char)

def Grid.get (g : Grid) (p : Int × Int) : Char :=
  if p.1 < 0 || p.1 ≥ g.size then
    '.'
  else
    let row := g[p.1.toNat]!
    if p.2 < 0 || p.2 ≥ row.size then
      '.'
    else
      row[p.2.toNat]!

def Grid.numRows (g : Grid) : Nat := g.size
def Grid.numCols (g : Grid) : Nat := g[0]!.size

def Grid.startPos (g : Grid) : Int × Int := Id.run do
  for i in [0:g.numRows] do
    for j in [0:g.numCols] do
      if g[i]![j]! == 'S' then
        return (i, j)
  panic! "Missing start position"

-- returns array of (delta row, delta col)
def connections (c : Char) : Array (Int × Int) :=
  match c with
  | '|' => #[(-1, 0), (1, 0)]
  | '-' => #[(0, -1), (0, 1)]
  | 'L' => #[(-1, 0), (0, 1)]
  | 'J' => #[(-1, 0), (0, -1)]
  | '7' => #[(1, 0), (0, -1)]
  | 'F' => #[(1, 0), (0, 1)]
  | 'S' => #[(-1, 0), (1, 0), (0, -1), (0, 1)]
  | _ => #[]


/-- Returns all neighbors implied by `connections`, but doesn't check if they reciprocate. -/
def Grid.neighborsAux (g : Grid) (p : Int × Int) : Array (Int × Int) :=
  connections (g.get p) |>.map fun d => (p.1 + d.1, p.2 + d.2)

/-- Returns all neighbors of `p` that are actually bidirectionally connected. -/
def Grid.neighbors (g : Grid) (p : Int × Int) : Array (Int × Int) :=
  let nbs := g.neighborsAux p
  nbs.filter fun q => (g.neighborsAux q).contains p

def parseGrid (input : String) : Grid :=
  let lines := input.split (· == '\n') |>.filter (·.length != 0)
  lines.toArray.map (·.toList.toArray)

partial def process (input : String) : IO Unit := do
  let g : Grid := parseGrid input
  let init := g.startPos
  let mut queue : Array ((Int × Int) × Nat) := g.neighbors init |>.map fun p => (p, 1)
  let mut queueIdx := 0
  let mut dists : Lean.HashMap (Int × Int) Nat := ({} : Lean.HashMap _ _).insert init 0
  while queueIdx < queue.size do
    let (p, dist) := queue[queueIdx]!
    queueIdx := queueIdx + 1
    --dbg_trace "{p}, {dist}, {queueIdx}"
    if let some dist' := dists.find? p then
      if dist == dist' then
        IO.println s!"{dist}"
        return
      else
        -- It's from switching directions along the path
        continue
    else
      dists := dists.insert p dist
      for q in g.neighbors p do
        queue := queue.push (q, dist + 1)

  --IO.println s!"{g}"

  --IO.println s!"start = {init}"

def exampleInput :=
".....
.S-7.
.|.|.
.L-J.
....."

/-- info: 4 -/
#guard_msgs in
#eval process exampleInput

def exampleInput2 :=
"..F7.
.FJ|.
SJ.L7
|F--J
LJ..."

/-- info: 8 -/
#guard_msgs in
#eval process exampleInput2

@[aoc_main day10_1]
def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let input ← IO.FS.readFile filename
  process input
