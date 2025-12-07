/- Day 7: Laboratories -/
import AoC2025.Util

namespace AoC2025.Day7

open AoC2025.Util

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

structure BeamState where
  activeBeams : List (Nat × Nat)
  splitCount : Nat

def stepBeams (grid : Grid) (state : BeamState) : BeamState :=
  let nextRow := state.activeBeams.head!.1 + 1
  if nextRow >= grid.height then
    { activeBeams := [], splitCount := state.splitCount }
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
    let dedupedPositions := newPositions.eraseDups
    { activeBeams := dedupedPositions, splitCount := state.splitCount + splits }

partial def simulate (grid : Grid) (state : BeamState) : Nat :=
  if state.activeBeams.isEmpty then state.splitCount
  else simulate grid (stepBeams grid state)

def solvePart1 (input : String) : Nat :=
  let grid := parseGrid input
  match findStart grid with
  | none => 0
  | some (row, col) =>
    let initial : BeamState := { activeBeams := [(row, col)], splitCount := 0 }
    simulate grid initial

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

partial def simulateTimelines (grid : Grid) (state : TimelineState) : Nat :=
  if state.row >= grid.height - 1 then
    state.timelines.foldl (fun acc (_, count) => acc + count) 0
  else
    simulateTimelines grid (stepTimelines grid state)

def solvePart2 (input : String) : Nat :=
  let grid := parseGrid input
  match findStart grid with
  | none => 0
  | some (row, col) =>
    let initial : TimelineState := { row := row, timelines := [(col, 1)] }
    simulateTimelines grid initial

def run (path : String) : IO Unit := do
  let input ← IO.FS.readFile path
  IO.println s!"Day 7 Part 1: {solvePart1 input}"
  IO.println s!"Day 7 Part 2: {solvePart2 input}"

end AoC2025.Day7
