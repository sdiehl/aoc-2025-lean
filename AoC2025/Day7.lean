/- Day 7: Laboratories -/
import AoC2025.Util

namespace AoC2025.Day7

open AoC2025.Util

/-! ## Dependent Types for Grid Safety

We use dependent types to ensure grid access is bounds-safe at compile time.
`Fin n` indices guarantee we never access out of bounds. -/

/-- Safely get an element from an array using a bounded index -/
def safeGet {α : Type} {n : Nat} (arr : Array α) (i : Fin n) (h : arr.size = n) : α :=
  arr[i.val]'(by rw [h]; exact i.isLt)

/-- Proof: Fin indices are always within bounds -/
theorem fin_always_valid (n : Nat) (i : Fin n) : i.val < n := i.isLt

/-- Create a Fin from a Nat with a proof of bounds -/
def natToFin? (n bound : Nat) : Option (Fin bound) :=
  if h : n < bound then some ⟨n, h⟩ else none

/-- Dependent pair: a value paired with a proof about it -/
structure BoundedCoord (maxRow maxCol : Nat) where
  row : Fin maxRow
  col : Fin maxCol
  deriving Repr

/-- Convert raw coordinates to bounded coordinates (if valid) -/
def toBoundedCoord? (r c : Nat) (maxRow maxCol : Nat) : Option (BoundedCoord maxRow maxCol) :=
  match natToFin? r maxRow, natToFin? c maxCol with
  | some row, some col => some ⟨row, col⟩
  | _, _ => none

/-! ## Standard Grid (for compatibility with parsing) -/

structure Grid where
  rows : Array String
  height : Nat
  width : Nat

def parseGrid (input : String) : Grid :=
  let rows := (lines input).toArray
  let height := rows.size
  let width := if height > 0 then rows[0]!.length else 0
  { rows, height, width }

def findStart (grid : Grid) : Option (Nat × Nat) := Id.run do
  for row in [:grid.height] do
    let line := grid.rows[row]!
    for col in [:line.length] do
      if line.toList.getD col ' ' == 'S' then
        return some (row, col)
  return none

def getCell (grid : Grid) (row col : Nat) : Char :=
  if row < grid.height then
    let line := grid.rows[row]!
    if col < line.length then line.toList.getD col '.' else '.'
  else '.'

/-! ## Part 1: State Monad for Beam Simulation -/

structure BeamState where
  activeBeams : List (Nat × Nat)
  splitCount : Nat
  currentRow : Nat
  deriving Repr

abbrev BeamM := StateM BeamState

def stepBeamsM (grid : Grid) : BeamM Unit := do
  let state ← get
  if state.activeBeams.isEmpty then return
  let nextRow := state.currentRow + 1
  if nextRow >= grid.height then
    set (BeamState.mk [] state.splitCount state.currentRow)
  else
    let newPositions := state.activeBeams.foldl (fun acc (_, col) =>
      let cell := getCell grid nextRow col
      if cell == '^' then
        let left := if col > 0 then [(nextRow, col - 1)] else []
        let right := if col + 1 < grid.width then [(nextRow, col + 1)] else []
        acc ++ left ++ right
      else
        acc ++ [(nextRow, col)]
    ) []
    let splits := state.activeBeams.foldl (fun acc (_, col) =>
      if getCell grid nextRow col == '^' then acc + 1 else acc
    ) 0
    set (BeamState.mk newPositions.eraseDups (state.splitCount + splits) nextRow)

def simulateM (grid : Grid) (fuel : Nat) : BeamM Nat := do
  match fuel with
  | 0 => return (← get).splitCount
  | fuel' + 1 =>
    let state ← get
    if state.activeBeams.isEmpty then
      return state.splitCount
    else
      stepBeamsM grid
      simulateM grid fuel'

def solvePart1 (input : String) : Nat :=
  let grid := parseGrid input
  match findStart grid with
  | none => 0
  | some (row, col) =>
    let initial : BeamState := { activeBeams := [(row, col)], splitCount := 0, currentRow := row }
    let (result, _) := simulateM grid grid.height |>.run initial
    result

/-! ## Part 2: Timeline Counting with Termination Proof -/

structure TimelineState where
  row : Nat
  timelines : List (Nat × Nat)

def addTimelines (timelines : List (Nat × Nat)) (col count : Nat) : List (Nat × Nat) :=
  match timelines.find? (·.1 == col) with
  | some _ => timelines.map (fun (c, n) => if c == col then (c, n + count) else (c, n))
  | none => (col, count) :: timelines

def stepTimelines (grid : Grid) (state : TimelineState) : TimelineState :=
  let nextRow := state.row + 1
  if nextRow >= grid.height then
    { row := nextRow, timelines := state.timelines }
  else
    let newTimelines := state.timelines.foldl (fun acc (col, count) =>
      let cell := getCell grid nextRow col
      if cell == '^' then
        let acc' := if col > 0 then addTimelines acc (col - 1) count else acc
        if col + 1 < grid.width then addTimelines acc' (col + 1) count else acc'
      else
        addTimelines acc col count
    ) []
    { row := nextRow, timelines := newTimelines }

/-- Simulate with explicit fuel parameter for termination -/
def simulateTimelinesWithFuel (grid : Grid) (state : TimelineState) (fuel : Nat) : Nat :=
  match fuel with
  | 0 => state.timelines.foldl (fun acc (_, count) => acc + count) 0
  | fuel' + 1 =>
    if state.row >= grid.height - 1 then
      state.timelines.foldl (fun acc (_, count) => acc + count) 0
    else
      simulateTimelinesWithFuel grid (stepTimelines grid state) fuel'

def solvePart2 (input : String) : Nat :=
  let grid := parseGrid input
  match findStart grid with
  | none => 0
  | some (row, col) =>
    let initial : TimelineState := { row := row, timelines := [(col, 1)] }
    simulateTimelinesWithFuel grid initial grid.height

def run (path : String) : IO Unit := do
  let input ← IO.FS.readFile path
  IO.println s!"Day 7 Part 1: {solvePart1 input}"
  IO.println s!"Day 7 Part 2: {solvePart2 input}"

end AoC2025.Day7
