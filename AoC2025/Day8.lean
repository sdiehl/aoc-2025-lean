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

/-- Union-Find data structure -/
structure UnionFind where
  parent : Array Nat
  deriving Repr

namespace UnionFind

/-- Create a new UnionFind where each element is its own root -/
def create (n : Nat) : UnionFind := { parent := Array.range n }

/-- Find root with path compression. Terminates because path length is bounded by array size. -/
def findRoot (uf : UnionFind) (i : Nat) (fuel : Nat := uf.parent.size) : Nat × UnionFind :=
  match fuel with
  | 0 => (i, uf)
  | fuel' + 1 =>
    let p := uf.parent[i]!
    if p == i then (i, uf)
    else
      let (root, uf') := findRoot uf p fuel'
      (root, { parent := uf'.parent.set! i root })

/-- Check if two elements are in the same component -/
def connected (uf : UnionFind) (i j : Nat) : Bool × UnionFind :=
  let (ri, uf') := findRoot uf i
  let (rj, uf'') := findRoot uf' j
  (ri == rj, uf'')

/-- Union two components -/
def union (uf : UnionFind) (i j : Nat) : UnionFind :=
  let (ri, uf') := findRoot uf i
  let (rj, uf'') := findRoot uf' j
  if ri == rj then uf'' else { parent := uf''.parent.set! ri rj }

/-- Count distinct components -/
def componentCount (uf : UnionFind) : Nat :=
  let roots := List.range uf.parent.size |>.map (fun i => (findRoot uf i).1)
  roots.eraseDups.length

/-- Get sizes of all components, sorted descending -/
def componentSizes (uf : UnionFind) : List Nat :=
  let roots := List.range uf.parent.size |>.map (fun i => (findRoot uf i).1)
  let counts := roots.foldl (fun acc r =>
    match acc.find? (·.1 == r) with
    | some _ => acc.map (fun (k, v) => if k == r then (k, v + 1) else (k, v))
    | none => (r, 1) :: acc
  ) []
  counts.map (·.2) |>.mergeSort (· > ·)

end UnionFind

/-- The connectivity relation on node indices -/
def Connected (uf : UnionFind) (i j : Nat) : Prop :=
  (uf.findRoot i).1 = (uf.findRoot j).1

theorem connected_refl (uf : UnionFind) (i : Nat) : Connected uf i i := rfl

theorem connected_symm (uf : UnionFind) (i j : Nat) :
    Connected uf i j → Connected uf j i := fun h => h.symm

theorem connected_trans (uf : UnionFind) (i j k : Nat) :
    Connected uf i j → Connected uf j k → Connected uf i k :=
  fun hij hjk => hij.trans hjk

theorem connected_equivalence (uf : UnionFind) : Equivalence (Connected uf) :=
  ⟨connected_refl uf, fun h => connected_symm uf _ _ h, fun h1 h2 => connected_trans uf _ _ _ h1 h2⟩

/-- Decidable instance allows using Connected in if/decide expressions -/
instance (uf : UnionFind) (i j : Nat) : Decidable (Connected uf i j) :=
  if h : (uf.findRoot i).1 = (uf.findRoot j).1 then isTrue h else isFalse h

def solvePart1 (input : String) : Nat :=
  let points := (lines input).filterMap parsePoint |>.toArray
  let pairs := allPairs points.size
  let withDist := pairs.map (fun (i, j) => (distSq points[i]! points[j]!, i, j))
  let sorted := withDist.mergeSort (fun a b => a.1 < b.1)
  let top1000 := sorted.take 1000
  let uf := top1000.foldl (fun uf (_, i, j) => uf.union i j) (UnionFind.create points.size)
  let sizes := uf.componentSizes
  sizes.take 3 |>.foldl (· * ·) 1

def findLastMerge (sorted : List (Int × Nat × Nat)) (uf : UnionFind)
    : Option (Nat × Nat) :=
  match sorted with
  | [] => none
  | (_, i, j) :: rest =>
    let (conn, uf') := uf.connected i j
    if conn then findLastMerge rest uf'
    else
      let uf'' := uf'.union i j
      if uf''.componentCount == 1 then some (i, j)
      else findLastMerge rest uf''
termination_by sorted.length

def solvePart2 (input : String) : Int :=
  let points := (lines input).filterMap parsePoint |>.toArray
  let pairs := allPairs points.size
  let withDist := pairs.map (fun (i, j) => (distSq points[i]! points[j]!, i, j))
  let sorted := withDist.mergeSort (fun a b => a.1 < b.1)
  match findLastMerge sorted (UnionFind.create points.size) with
  | some (i, j) => points[i]!.x * points[j]!.x
  | none => 0

def run (path : String) : IO Unit := do
  let input ← IO.FS.readFile path
  IO.println s!"Day 8 Part 1: {solvePart1 input}"
  IO.println s!"Day 8 Part 2: {solvePart2 input}"

end AoC2025.Day8
