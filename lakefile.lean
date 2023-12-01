import Lake
open Lake DSL

package «AoC2023» where
  -- add package configuration options here

lean_lib «AoC2023» where
  -- add library configuration options here

-- @[default_target]
-- lean_exe «aoc2023» where
--   root := `Main
--   -- Enables the use of the Lean interpreter by the executable (e.g.,
--   -- `runFrontend`) at the expense of increased binary size on Linux.
--   -- Remove this line if you do not need such functionality.
--   supportInterpreter := true

-- Run with `lake exe aoc cmdName ...`
lean_exe aoc where
  root := `AoC

require std from git "https://github.com/leanprover/std4" @ "main"
