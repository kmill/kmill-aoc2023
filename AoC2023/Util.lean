import Lean
import Std

def Char.digit? (c : Char) : Option Nat :=
  if 48 ≤ c.val && c.val ≤ 57 then
    some (c.val - 48).toNat
  else
    none

/-- info: some 0 -/
#guard_msgs in #eval '0'.digit?
/-- info: some 9 -/
#guard_msgs in #eval '9'.digit?
/-- info: none -/
#guard_msgs in #eval 'a'.digit?
