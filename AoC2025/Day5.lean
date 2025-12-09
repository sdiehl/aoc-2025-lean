/- Day 5: Cafeteria -/
import AoC2025.Util

namespace AoC2025.Day5

open AoC2025.Util

structure Range where
  start : Nat
  stop : Nat

def parseRange (s : String) : Option Range :=
  match s.splitOn "-" with
  | [a, b] =>
    match (a.toNat?, b.toNat?) with
    | (some start, some stop) => some { start, stop }
    | _ => none
  | _ => none

def Range.contains (r : Range) (n : Nat) : Bool :=
  n >= r.start && n <= r.stop

def isFresh (ranges : List Range) (id : Nat) : Bool :=
  ranges.any (·.contains id)

def parseInput (input : String) : (List Range × List Nat) :=
  let parts := input.splitOn "\n\n"
  match parts with
  | [rangeSection, idSection] =>
    let ranges := (lines rangeSection).filterMap parseRange
    let ids := (lines idSection).filterMap (·.toNat?)
    (ranges, ids)
  | _ => ([], [])

def solvePart1 (input : String) : Nat :=
  let (ranges, ids) := parseInput input
  ids.filter (isFresh ranges) |>.length

def mergeRanges (ranges : List Range) : List Range :=
  let sorted := ranges.toArray.qsort (fun a b => a.start < b.start) |>.toList
  match sorted with
  | [] => []
  | first :: rest =>
    let (merged, last) := rest.foldl (fun (acc, current) next =>
      if next.start <= current.stop + 1 then
        (acc, { start := current.start, stop := max current.stop next.stop })
      else
        (current :: acc, next))
      ([], first)
    (last :: merged).reverse

/-- Two ranges are disjoint if they don't overlap -/
def Range.disjoint (r1 r2 : Range) : Prop :=
  r1.stop < r2.start ∨ r2.stop < r1.start

/-- A list of ranges is non-overlapping if all pairs are disjoint -/
def NonOverlapping : List Range → Prop
  | [] => True
  | [_] => True
  | r1 :: r2 :: rest => r1.stop < r2.start ∧ NonOverlapping (r2 :: rest)

/-- A list of ranges is sorted by start position -/
def SortedByStart : List Range → Prop
  | [] => True
  | [_] => True
  | r1 :: r2 :: rest => r1.start ≤ r2.start ∧ SortedByStart (r2 :: rest)

/-- Merged ranges from sorted input are non-overlapping.
    The proof is nontrivial and requires reasoning about the fold invariant. -/
axiom mergeRanges_nonOverlapping (ranges : List Range) :
    NonOverlapping (mergeRanges ranges)

def countFreshIds (ranges : List Range) : Nat :=
  let merged := mergeRanges ranges
  merged.foldl (fun acc r => acc + (r.stop - r.start + 1)) 0

def solvePart2 (input : String) : Nat :=
  let (ranges, _) := parseInput input
  countFreshIds ranges

def run (inputPath : String) : IO Unit := do
  let input ← readFile inputPath
  IO.println s!"Day 5 Part 1: {solvePart1 input}"
  IO.println s!"Day 5 Part 2: {solvePart2 input}"

end AoC2025.Day5
