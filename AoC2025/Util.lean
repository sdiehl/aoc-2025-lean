/-
Common utilities for Advent of Code 2025 solutions.
-/
namespace AoC2025.Util

/-- Read a file and return its contents as a string. -/
def readFile (path : String) : IO String := do
  IO.FS.readFile path

/-- Split a string by newlines, filtering empty lines. -/
def lines (s : String) : List String :=
  s.splitOn "\n" |>.map String.trim |>.filter (· ≠ "")

/-- Split a string by a delimiter. -/
def splitBy (s : String) (delim : String) : List String :=
  s.splitOn delim |>.map String.trim |>.filter (· ≠ "")

/-- Parse a string as a natural number. Returns 0 if parsing fails. -/
def parseNat (s : String) : Nat :=
  s.toNat?.getD 0

/-- Parse a string as an integer. Returns 0 if parsing fails. -/
def parseInt (s : String) : Int :=
  s.toInt?.getD 0

/-- Helper for digits: extract digits from a number. -/
def digitsAux (m : Nat) (acc : List Nat) : List Nat :=
  if h : m == 0 then acc
  else digitsAux (m / 10) ((m % 10) :: acc)
termination_by m
decreasing_by
  simp_wf
  have : m ≠ 0 := by simp_all
  omega

/-- Get all digits of a natural number as a list. -/
def digits (n : Nat) : List Nat :=
  if n == 0 then [0]
  else digitsAux n []

/-- Convert a list of digits to a natural number. -/
def fromDigits (ds : List Nat) : Nat :=
  ds.foldl (fun acc d => acc * 10 + d) 0

/-- Helper for numDigits. -/
def numDigitsAux (m : Nat) (count : Nat) : Nat :=
  if h : m == 0 then count
  else numDigitsAux (m / 10) (count + 1)
termination_by m
decreasing_by
  simp_wf
  have : m ≠ 0 := by simp_all
  omega

/-- Count the number of digits in a natural number. -/
def numDigits (n : Nat) : Nat :=
  if n == 0 then 1
  else numDigitsAux n 0

/-- Compute 10^n using Nat.pow. -/
def pow10 (n : Nat) : Nat :=
  10 ^ n

/-- Sum a list of natural numbers. -/
def sum (xs : List Nat) : Nat :=
  xs.foldl (· + ·) 0

/-- Sum a list of integers. -/
def sumInt (xs : List Int) : Int :=
  xs.foldl (· + ·) 0

/-- Maximum of a list of natural numbers. -/
def maxList (xs : List Nat) : Nat :=
  xs.foldl max 0

end AoC2025.Util
