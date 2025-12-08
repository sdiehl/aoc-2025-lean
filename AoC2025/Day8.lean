/- Day 8: Playground -/
import AoC2025.Util

namespace AoC2025.Day8

open AoC2025.Util

structure Point where
  x : Int
  y : Int
  z : Int
  deriving Repr, BEq, Inhabited

def parsePoint (s : String) : Option Point :=
  match s.splitOn "," |>.map String.trim |>.map String.toInt? with
  | [some x, some y, some z] => some { x, y, z }
  | _ => none

def distSq (p1 p2 : Point) : Int :=
  (p1.x - p2.x)^2 + (p1.y - p2.y)^2 + (p1.z - p2.z)^2

def allPairs (n : Nat) : List (Nat × Nat) := Id.run do
  let mut pairs := []
  for i in [:n] do
    for j in [i+1:n] do
      pairs := (i, j) :: pairs
  return pairs

partial def find (parent : Array Nat) (i : Nat) : Nat × Array Nat :=
  let p := parent[i]!
  if p == i then (i, parent)
  else
    let (root, parent') := find parent p
    (root, parent'.set! i root)

def union (parent : Array Nat) (i j : Nat) : Array Nat :=
  let (ri, parent') := find parent i
  let (rj, parent'') := find parent' j
  if ri == rj then parent'' else parent''.set! ri rj

def circuitSizes (parent : Array Nat) : List Nat :=
  let roots := List.range parent.size |>.map (fun i => (find parent i).1)
  let counts := roots.foldl (fun acc r =>
    match acc.find? (·.1 == r) with
    | some _ => acc.map (fun (k, v) => if k == r then (k, v + 1) else (k, v))
    | none => (r, 1) :: acc
  ) []
  counts.map (·.2) |>.mergeSort (· > ·)

def solvePart1 (input : String) : Nat :=
  let points := (lines input).filterMap parsePoint |>.toArray
  let pairs := allPairs points.size
  let withDist := pairs.map (fun (i, j) => (distSq points[i]! points[j]!, i, j))
  let sorted := withDist.mergeSort (fun a b => a.1 < b.1)
  let top1000 := sorted.take 1000
  let parent := Array.range points.size
  let finalParent := top1000.foldl (fun p (_, i, j) => union p i j) parent
  let sizes := circuitSizes finalParent
  sizes.take 3 |>.foldl (· * ·) 1

def countCircuits (parent : Array Nat) : Nat :=
  let roots := List.range parent.size |>.map (fun i => (find parent i).1)
  roots.eraseDups.length

partial def findLastMerge (sorted : List (Int × Nat × Nat)) (parent : Array Nat)
    : Option (Nat × Nat) :=
  match sorted with
  | [] => none
  | (_, i, j) :: rest =>
    let (ri, parent') := find parent i
    let (rj, parent'') := find parent' j
    if ri == rj then findLastMerge rest parent''
    else
      let parent''' := parent''.set! ri rj
      if countCircuits parent''' == 1 then some (i, j)
      else findLastMerge rest parent'''

def solvePart2 (input : String) : Int :=
  let points := (lines input).filterMap parsePoint |>.toArray
  let pairs := allPairs points.size
  let withDist := pairs.map (fun (i, j) => (distSq points[i]! points[j]!, i, j))
  let sorted := withDist.mergeSort (fun a b => a.1 < b.1)
  let parent := Array.range points.size
  match findLastMerge sorted parent with
  | some (i, j) => points[i]!.x * points[j]!.x
  | none => 0

def run (path : String) : IO Unit := do
  let input ← IO.FS.readFile path
  IO.println s!"Day 8 Part 1: {solvePart1 input}"
  IO.println s!"Day 8 Part 2: {solvePart2 input}"

end AoC2025.Day8
