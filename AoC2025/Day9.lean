/- Day 9: Movie Theater -/
import AoC2025.Util

namespace AoC2025.Day9

open AoC2025.Util

def parsePoint (s : String) : Option (Int × Int) :=
  match s.splitOn "," |>.map String.trim |>.map String.toInt? with
  | [some x, some y] => some (x, y)
  | _ => none

def rectArea (p1 p2 : Int × Int) : Int :=
  ((p2.1 - p1.1).natAbs + 1) * ((p2.2 - p1.2).natAbs + 1)

def allPairs (points : List (Int × Int)) : List ((Int × Int) × (Int × Int)) := Id.run do
  let arr := points.toArray
  let mut pairs := []
  for i in [:arr.size] do
    for j in [i+1:arr.size] do
      pairs := (arr[i]!, arr[j]!) :: pairs
  return pairs

def solvePart1 (input : String) : Int :=
  let points := (lines input).filterMap parsePoint
  let pairs := allPairs points
  pairs.foldl (fun acc (p1, p2) => max acc (rectArea p1 p2)) 0

structure Segment where
  x1 : Int
  y1 : Int
  x2 : Int
  y2 : Int
  deriving Repr

def buildSegments (redTiles : List (Int × Int)) : List Segment :=
  if redTiles.isEmpty then []
  else
    let arr := redTiles.toArray
    List.range arr.size |>.map fun i =>
      let p1 := arr[i]!
      let p2 := arr[(i + 1) % arr.size]!
      { x1 := p1.1, y1 := p1.2, x2 := p2.1, y2 := p2.2 }

def segmentCrossesRect (seg : Segment) (minX maxX minY maxY : Int) : Bool :=
  if seg.x1 == seg.x2 then
    let segMinY := min seg.y1 seg.y2
    let segMaxY := max seg.y1 seg.y2
    seg.x1 > minX && seg.x1 < maxX && segMinY < maxY && segMaxY > minY
  else
    let segMinX := min seg.x1 seg.x2
    let segMaxX := max seg.x1 seg.x2
    seg.y1 > minY && seg.y1 < maxY && segMinX < maxX && segMaxX > minX

def isOnSegment (seg : Segment) (x y : Int) : Bool :=
  if seg.x1 == seg.x2 then
    x == seg.x1 && y >= min seg.y1 seg.y2 && y <= max seg.y1 seg.y2
  else
    y == seg.y1 && x >= min seg.x1 seg.x2 && x <= max seg.x1 seg.x2

def isOnBoundary (segments : List Segment) (x y : Int) : Bool :=
  segments.any (isOnSegment · x y)

def countCrossingsRight (segments : List Segment) (x y : Int) : Nat :=
  segments.foldl (fun count seg =>
    if seg.x1 == seg.x2 then
      let minY := min seg.y1 seg.y2
      let maxY := max seg.y1 seg.y2
      if seg.x1 > x && y > minY && y < maxY then count + 1 else count
    else count
  ) 0

def isInsideOrOnBoundary (segments : List Segment) (x y : Int) : Bool :=
  isOnBoundary segments x y || countCrossingsRight segments x y % 2 == 1

def rectValidInPolygon (segments : List Segment) (p1 p2 : Int × Int) : Bool :=
  let minX := min p1.1 p2.1
  let maxX := max p1.1 p2.1
  let minY := min p1.2 p2.2
  let maxY := max p1.2 p2.2
  let corner1 := (minX, maxY)
  let corner2 := (maxX, minY)
  if !isInsideOrOnBoundary segments corner1.1 corner1.2 then false
  else if !isInsideOrOnBoundary segments corner2.1 corner2.2 then false
  else !segments.any (segmentCrossesRect · minX maxX minY maxY)

def solvePart2 (input : String) : Int :=
  let redTiles := (lines input).filterMap parsePoint
  let segments := buildSegments redTiles
  let pairs := allPairs redTiles
  pairs.foldl (fun acc (p1, p2) =>
    if rectValidInPolygon segments p1 p2 then max acc (rectArea p1 p2) else acc
  ) 0

def run (path : String) : IO Unit := do
  let input ← IO.FS.readFile path
  IO.println s!"Day 9 Part 1: {solvePart1 input}"
  IO.println s!"Day 9 Part 2: {solvePart2 input}"

end AoC2025.Day9
