/- Shared utilities -/
namespace AoC2025.Util

def readFile (path : String) : IO String := do
  IO.FS.readFile path

def lines (s : String) : List String :=
  s.splitOn "\n" |>.map String.trim |>.filter (· ≠ "")

def linesRaw (s : String) : List String :=
  s.splitOn "\n" |>.filter (·.any (· != ' '))

def splitBy (s : String) (delim : String) : List String :=
  s.splitOn delim |>.map String.trim |>.filter (· ≠ "")

def numDigitsAux (m : Nat) (count : Nat) : Nat :=
  if h : m == 0 then count
  else numDigitsAux (m / 10) (count + 1)
termination_by m
decreasing_by
  simp_wf
  have : m ≠ 0 := by simp_all
  omega

def numDigits (n : Nat) : Nat :=
  if n == 0 then 1
  else numDigitsAux n 0

def pow10 (n : Nat) : Nat :=
  10 ^ n

def sum (xs : List Nat) : Nat :=
  xs.foldl (· + ·) 0

def charToDigit (c : Char) : Nat :=
  c.toNat - '0'.toNat

end AoC2025.Util
