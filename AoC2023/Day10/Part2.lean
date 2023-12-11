import AoC2023.Util

namespace Day10_2

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

def findLoop (g : Grid) (init : Int × Int) : Array (Int × Int) := Id.run do
  -- The queue records paths to the current point
  let mut queue : Array (Array (Int × Int)) := g.neighbors init |>.map fun p => #[init, p]
  let mut queueIdx := 0
  -- Records for each point the shortest path to the point
  let mut dists : Lean.HashMap (Int × Int) (Array (Int × Int)) := ({} : Lean.HashMap _ _).insert init #[]
  while queueIdx < queue.size do
    let path := queue[queueIdx]!
    let p := path.back
    queueIdx := queueIdx + 1
    if let some path' := dists.find? p then
      if path.size == path'.size then
        let loop := path.pop ++ path'.reverse.pop
        return loop
      else
        -- It's from switching directions along the path
        continue
    else
      dists := dists.insert p path
      for q in g.neighbors p do
        queue := queue.push (path.push q)
  panic! "No loop found!!!"

partial def process (input : String) : IO Unit := do
  let g : Grid := parseGrid input
  let init := g.startPos
  let loop := findLoop g init
  --IO.println s!"loop = {loop}"
  /-
  For determining the "inside", use Z/2Z intersection numbers.
  We take a ray from the point outwards, find out how many times the ray intersects
  the loop, and the point is "inside" iff this count is odd.
  We will have our rays go directly to the left from a point somewhere inside the cell.
  Let's say that point is at (2/3, 2/3), which ensures the ray intersects the loop transversely.
  With these conventions (1) only vertical segments of the loop contribute to the count
  and (2) only vertical segments whose top endpoint is in the cell of the portion of the ray contribute.

  -------
  |     |
  |  ·  |
  | -+-*|  The * is a point along the ray going leftwards
  ---|---
  |  |  |
  |  ·  |
  | ---*|
  -------

  The + is an intersection point between a ray and a vertical segment. Notice the top cell has an intersection,
  but the lower cell doesn't.
  -/
  -- Segments are pairs of consecutive points along the loop
  let segments := loop.zip (loop.extract 1 loop.size |>.push loop[0]!)
  --IO.println s!"segments = {segments}"
  -- Vertical segments are those whose columns are equal
  let vsegments := segments.filter fun (p, q) => p.2 = q.2
  -- Segment tops are the points in the segments with the least row number
  let vsegtops := vsegments.map fun (p, q) => (min p.1 q.1, p.2)
  --IO.println s!"vsegtops = {vsegtops}"
  let tops : Lean.HashSet (Int × Int) := Lean.HashSet.ofArray vsegtops
  let loopCells : Lean.HashSet (Int × Int) := Lean.HashSet.ofArray loop
  let mut area : Nat := 0
  for i in [0:g.numRows] do
    let mut inside : Bool := false
    for j in [0:g.numCols] do
      if tops.contains (i, j) then
        inside := !inside
      if inside then
        -- Technically we're inside, but we are supposed to exclude loop cells
        if !loopCells.contains (i, j) then
          area := area + 1
  IO.println s!"{area}"

def exampleInput :=
".....
.S-7.
.|.|.
.L-J.
....."

/-- info: 1 -/
#guard_msgs in
#eval process exampleInput

def exampleInput2 :=
"..F7.
.FJ|.
SJ.L7
|F--J
LJ..."

/-- info: 1 -/
#guard_msgs in
#eval process exampleInput2

def exampleInput3 :=
"...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
..........."

/-- info: 4 -/
#guard_msgs in
#eval process exampleInput3

def exampleInput4 :=
"..........
.S------7.
.|F----7|.
.||....||.
.||....||.
.|L-7F-J|.
.|..||..|.
.L--JL--J.
.........."

/-- info: 4 -/
#guard_msgs in
#eval process exampleInput4

def exampleInput5 :=
".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ..."

/-- info: 8 -/
#guard_msgs in
#eval process exampleInput5

def exampleInput6 :=
"FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L"

/-- info: 10 -/
#guard_msgs in
#eval process exampleInput6

@[aoc_main day10_2]
def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let input ← IO.FS.readFile filename
  process input
