/- Day 4: Printing Department -/
import AoC2025.Util

namespace AoC2025.Day4

open AoC2025.Util

structure Grid where
  cells : Array (Array Char)
  height : Nat
  width : Nat

def parseGrid (input : String) : Grid :=
  let rows := lines input |>.filter (·.length > 0)
  let cells := rows.map (·.toList.toArray) |>.toArray
  let height := cells.size
  let width := if height > 0 then cells[0]!.size else 0
  { cells, height, width }

def Grid.get (g : Grid) (row col : Int) : Char :=
  if row < 0 || col < 0 then '.'
  else if row.toNat >= g.height || col.toNat >= g.width then '.'
  else g.cells[row.toNat]![col.toNat]!

def countAdjacentRolls (g : Grid) (row col : Nat) : Nat :=
  let r : Int := row
  let c : Int := col
  let neighbors := [
    (r - 1, c - 1), (r - 1, c), (r - 1, c + 1),
    (r,     c - 1),             (r,     c + 1),
    (r + 1, c - 1), (r + 1, c), (r + 1, c + 1)
  ]
  neighbors.foldl (fun acc (nr, nc) =>
    if g.get nr nc == '@' then acc + 1 else acc) 0

def isAccessible (g : Grid) (row col : Nat) : Bool :=
  g.cells[row]![col]! == '@' && countAdjacentRolls g row col < 4

def findAccessibleRolls (g : Grid) : List (Nat × Nat) :=
  let rows := List.range g.height
  let cols := List.range g.width
  rows.foldl (fun acc row =>
    cols.foldl (fun acc' col =>
      if isAccessible g row col then (row, col) :: acc' else acc') acc) []

def countAccessibleRolls (g : Grid) : Nat :=
  (findAccessibleRolls g).length

def removeRolls (g : Grid) (positions : List (Nat × Nat)) : Grid :=
  let newCells := positions.foldl (fun cells (row, col) =>
    cells.modify row (·.set! col '.')) g.cells
  { g with cells := newCells }

partial def removeAllAccessible (g : Grid) (total : Nat) : Nat :=
  let accessible := findAccessibleRolls g
  if accessible.isEmpty then total
  else
    let newGrid := removeRolls g accessible
    removeAllAccessible newGrid (total + accessible.length)

def solvePart1 (input : String) : Nat :=
  let grid := parseGrid input
  countAccessibleRolls grid

def solvePart2 (input : String) : Nat :=
  let grid := parseGrid input
  removeAllAccessible grid 0

def run (inputPath : String) : IO Unit := do
  let input ← readFile inputPath
  IO.println s!"Day 4 Part 1: {solvePart1 input}"
  IO.println s!"Day 4 Part 2: {solvePart2 input}"

end AoC2025.Day4
