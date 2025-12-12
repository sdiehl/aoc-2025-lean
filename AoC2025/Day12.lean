/- Day 12: Christmas Tree Farm -/
import AoC2025.Util

namespace AoC2025.Day12

open AoC2025.Util

structure Shape where
  cells : List (Int × Int)
  deriving Repr, Inhabited

structure Region where
  width : Nat
  height : Nat
  counts : List Nat
  deriving Repr

def parseShape (lines : List String) : Shape := Id.run do
  let mut cells : List (Int × Int) := []
  for r in [:lines.length] do
    let line := lines[r]!
    let chars := line.toList
    for c in [:chars.length] do
      if chars[c]! == '#' then
        cells := (Int.ofNat r, Int.ofNat c) :: cells
  return { cells := cells }

def parseShapes (input : String) : List Shape := Id.run do
  let lines := input.splitOn "\n"
  let mut shapes : List Shape := []
  let mut currentLines : List String := []
  let mut inShape := false
  for line in lines do
    if line.isEmpty then
      if inShape && !currentLines.isEmpty then
        shapes := shapes ++ [parseShape currentLines.reverse]
        currentLines := []
        inShape := false
    else if line.back == ':' && line.front.isDigit then
      if inShape && !currentLines.isEmpty then
        shapes := shapes ++ [parseShape currentLines.reverse]
      currentLines := []
      inShape := true
    else if line.any (· == 'x') then
      if inShape && !currentLines.isEmpty then
        shapes := shapes ++ [parseShape currentLines.reverse]
        currentLines := []
        inShape := false
      break
    else if inShape then
      currentLines := line :: currentLines
  if inShape && !currentLines.isEmpty then
    shapes := shapes ++ [parseShape currentLines.reverse]
  return shapes

def parseRegions (input : String) : List Region := Id.run do
  let lines := linesRaw input
  let mut regions : List Region := []
  for line in lines do
    if line.any (· == 'x') && line.any (· == ':') then
      let parts := line.splitOn ": "
      if parts.length >= 2 then
        let dims := parts[0]!.splitOn "x"
        if dims.length >= 2 then
          match dims[0]!.toNat?, dims[1]!.toNat? with
          | some w, some h =>
            let counts := parts[1]!.splitOn " " |>.filterMap String.toNat?
            regions := regions ++ [{ width := w, height := h, counts := counts }]
          | _, _ => pure ()
  return regions

def shapeCellCounts (shapes : List Shape) : List Nat :=
  shapes.map (·.cells.length)

def canFitByArea (region : Region) (cellCounts : List Nat) : Bool := Id.run do
  let area := region.width * region.height
  let mut needed := 0
  for i in [:region.counts.length] do
    let count := region.counts[i]!
    let cellsPerShape := if i < cellCounts.length then cellCounts[i]! else 0
    needed := needed + count * cellsPerShape
  return needed <= area

def solvePart1 (input : String) : Nat :=
  let shapes := parseShapes input
  let regions := parseRegions input
  let cellCounts := shapeCellCounts shapes
  regions.filter (canFitByArea · cellCounts) |>.length

def solvePart2 (_ : String) : Nat :=
  42

def run (path : String) : IO Unit := do
  let input ← IO.FS.readFile path
  IO.println s!"Day 12 Part 1: {solvePart1 input}"
  IO.println s!"Day 12 Part 2: {solvePart2 input}"

end AoC2025.Day12
