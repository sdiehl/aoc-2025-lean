/- Day 3: Lobby -/
import AoC2025.Util

namespace AoC2025.Day3

open AoC2025.Util

def charToDigit (c : Char) : Nat :=
  c.toNat - '0'.toNat

def maxJoltageK (bank : String) (k : Nat) : Nat :=
  let digits := bank.toList.map charToDigit |>.toArray
  let n := digits.size

  if n < k then 0
  else
    let result := List.range k |>.foldl (fun (start, acc) remaining_idx =>
      let remaining := k - remaining_idx
      let endPos := n - remaining
      let maxInRange := List.range (endPos - start + 1)
        |>.foldl (fun (bestPos, bestVal) i =>
          let pos := start + i
          let v := digits.get! pos
          if v > bestVal then (pos, v) else (bestPos, bestVal))
        (start, digits.get! start)
      let (bestPos, bestDigit) := maxInRange
      (bestPos + 1, acc * 10 + bestDigit))
      (0, 0)
    result.2

def solvePart1 (input : String) : Nat :=
  let banks := lines input
  sum (banks.map (maxJoltageK · 2))

def solvePart2 (input : String) : Nat :=
  let banks := lines input
  sum (banks.map (maxJoltageK · 12))

def run (inputPath : String) : IO Unit := do
  let input ← readFile inputPath
  IO.println s!"Day 3 Part 1: {solvePart1 input}"
  IO.println s!"Day 3 Part 2: {solvePart2 input}"

end AoC2025.Day3
