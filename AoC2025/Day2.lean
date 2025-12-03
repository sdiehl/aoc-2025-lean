/- Day 2: Gift Shop -/
import AoC2025.Util

namespace AoC2025.Day2

open AoC2025.Util

def parseRange (s : String) : Option (Nat × Nat) := do
  let parts := s.splitOn "-"
  match parts with
  | [a, b] =>
    let low := a.trim.toNat?
    let high := b.trim.toNat?
    match low, high with
    | some l, some h => some (l, h)
    | _, _ => none
  | _ => none

def getDoubledInRange (low high : Nat) : List Nat :=
  let digitLengths := [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
  digitLengths.flatMap fun numDig =>
    let halfDigits := numDig / 2
    let multiplier := pow10 halfDigits + 1
    let minHalf := if halfDigits == 1 then 1 else pow10 (halfDigits - 1)
    let maxHalf := pow10 halfDigits - 1
    let halfLow := max minHalf ((low + multiplier - 1) / multiplier)
    let halfHigh := min maxHalf (high / multiplier)
    if halfHigh < halfLow then []
    else
      List.range (halfHigh - halfLow + 1)
        |>.map (fun i => (halfLow + i) * multiplier)
        |>.filter (fun n => low <= n && n <= high)

def isPatternRepeated (s : String) (patternLen : Nat) : Bool :=
  let len := s.length
  if len % patternLen != 0 then false
  else
    let k := len / patternLen
    if k < 2 then false
    else
      let pattern := s.take patternLen
      let repeated := String.join (List.replicate k pattern)
      repeated == s

def isRepeatedPattern (n : Nat) : Bool :=
  let s := toString n
  let len := s.length
  let patternLengths := List.range (len / 2) |>.map (· + 1)
  patternLengths.any (isPatternRepeated s)

def getRepeatedPatternsInRange (low high : Nat) : List Nat :=
  let lowDigits := numDigits low
  let highDigits := numDigits high
  let digitCounts := List.range (highDigits - lowDigits + 1) |>.map (· + lowDigits)

  let allNums := digitCounts.flatMap fun nd =>
    let patternLengths := List.range (nd / 2) |>.map (· + 1) |>.filter (fun l => nd % l == 0)
    patternLengths.flatMap fun l =>
      let k := nd / l
      if k < 2 then []
      else
        let minPattern := if l == 1 then 1 else pow10 (l - 1)
        let maxPattern := pow10 l - 1
        List.range (maxPattern - minPattern + 1)
          |>.map (fun i =>
            let pattern := minPattern + i
            let patternStr := toString pattern
            let repeatedStr := String.join (List.replicate k patternStr)
            repeatedStr.toNat?.getD 0)
          |>.filter (fun n => low <= n && n <= high)

  allNums.eraseDups

def solvePart1 (input : String) : Nat :=
  let ranges := splitBy input "," |>.filterMap parseRange
  ranges.foldl (fun acc (low, high) =>
    acc + sum (getDoubledInRange low high)) 0

def solvePart2 (input : String) : Nat :=
  let ranges := splitBy input "," |>.filterMap parseRange
  ranges.foldl (fun acc (low, high) =>
    acc + sum (getRepeatedPatternsInRange low high)) 0

def run (inputPath : String) : IO Unit := do
  let input ← readFile inputPath
  IO.println s!"Day 2 Part 1: {solvePart1 input}"
  IO.println s!"Day 2 Part 2: {solvePart2 input}"

end AoC2025.Day2
