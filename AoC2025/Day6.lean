/- Day 6: Trash Compactor -/
import AoC2025.Util

namespace AoC2025.Day6

open AoC2025.Util

inductive Op where
  | add
  | mul
  deriving Repr, BEq

def parseOp (c : Char) : Option Op :=
  match c with
  | '+' => some Op.add
  | '*' => some Op.mul
  | _ => none

structure Problem where
  numbers : List Nat
  op : Op

def evalProblem (p : Problem) : Nat :=
  match p.op with
  | Op.add => p.numbers.foldl (· + ·) 0
  | Op.mul => p.numbers.foldl (· * ·) 1

def padRows (rows : List String) : List String :=
  let maxLen := rows.foldl (fun acc r => max acc r.length) 0
  rows.map (fun r => r ++ String.mk (List.replicate (maxLen - r.length) ' '))

def getColumn (rows : List String) (col : Nat) : List Char :=
  rows.map (fun r => if col < r.length then r.get! ⟨col⟩ else ' ')

def isSeparatorColumn (numRows : List String) (col : Nat) : Bool :=
  let chars := getColumn numRows col
  chars.all (· == ' ')

def parseNumberFromColumn (numRows : List String) (col : Nat) : Option Nat :=
  let chars := getColumn numRows col
  let digits := chars.filter Char.isDigit
  if digits.isEmpty then none
  else
    let numStr := String.mk digits
    numStr.toNat?

def getOpFromColumn (opRow : String) (col : Nat) : Option Op :=
  if col < opRow.length then parseOp (opRow.get! ⟨col⟩) else none

-- Part 1: Parse problems left-to-right, numbers are horizontal multi-digit
def parseNumbersInRow (row : String) : List (Nat × Nat × Nat) :=
  let rec helper (pos : Nat) (acc : List (Nat × Nat × Nat)) (currentNum : String) (startPos : Nat) : List (Nat × Nat × Nat) :=
    if pos >= row.length then
      if currentNum.isEmpty then acc
      else match currentNum.toNat? with
        | some n => (n, startPos, pos - 1) :: acc
        | none => acc
    else
      let c := row.get! ⟨pos⟩
      if c.isDigit then
        if currentNum.isEmpty then
          helper (pos + 1) acc (String.singleton c) pos
        else
          helper (pos + 1) acc (currentNum ++ String.singleton c) startPos
      else
        if currentNum.isEmpty then
          helper (pos + 1) acc "" pos
        else
          match currentNum.toNat? with
          | some n => helper (pos + 1) ((n, startPos, pos - 1) :: acc) "" pos
          | none => helper (pos + 1) acc "" pos
  (helper 0 [] "" 0).reverse

def mergeOverlappingRanges (ranges : List (Nat × Nat)) : List (Nat × Nat) :=
  let sorted := ranges.toArray.qsort (fun a b => a.1 < b.1) |>.toList
  match sorted with
  | [] => []
  | first :: rest =>
    let (merged, last) := rest.foldl (fun (acc, current) next =>
      if next.1 <= current.2 + 1 then
        (acc, (current.1, max current.2 next.2))
      else
        (current :: acc, next))
      ([], first)
    (last :: merged).reverse

def parseProblemsHorizontal (input : String) : List Problem :=
  let allLines := lines input
  if allLines.length < 2 then []
  else
    let numRows := allLines.reverse.tail!.reverse
    let opRow := allLines.reverse.head!
    let numbersPerRow := numRows.map parseNumbersInRow
    let allRanges := numbersPerRow.foldl (fun acc row =>
      row.map (fun (_, start, stop) => (start, stop)) ++ acc) []
    let problemRanges := mergeOverlappingRanges allRanges
    problemRanges.filterMap (fun (rangeStart, rangeStop) =>
      let opCharsInRange := List.range (rangeStop - rangeStart + 1)
        |>.filterMap (fun offset =>
          let col := rangeStart + offset
          if col < opRow.length then
            let c := opRow.get! ⟨col⟩
            parseOp c
          else none)
      match opCharsInRange.head? with
      | none => none
      | some operation =>
        let numbers := numbersPerRow.foldl (fun acc row =>
          let numsInRange := row.filter (fun (_, start, stop) =>
            start <= rangeStop && stop >= rangeStart)
          acc ++ numsInRange.map (·.1)) []
        if numbers.isEmpty then none
        else some { numbers, op := operation })

def solvePart1 (input : String) : Nat :=
  let problems := parseProblemsHorizontal input
  problems.map evalProblem |>.foldl (· + ·) 0

-- Part 2: Parse problems where each column is a number, read right-to-left
partial def parseProblemsVertical (input : String) : List Problem :=
  let allLines := linesRaw input
  if allLines.length < 2 then []
  else
    let numRows := padRows (allLines.reverse.tail!.reverse)
    let opRow := allLines.reverse.head!
    let maxCol := numRows.foldl (fun acc r => max acc r.length) 0
    -- Read columns right-to-left
    let rec go (col : Int) (currentNums : List Nat) (currentOp : Option Op) (acc : List Problem) : List Problem :=
      if col < 0 then
        match currentOp with
        | some op => if currentNums.isEmpty then acc else { numbers := currentNums, op } :: acc
        | none => acc
      else
        let c := col.toNat
        if isSeparatorColumn numRows c then
          match currentOp with
          | some op =>
            if currentNums.isEmpty then
              go (col - 1) [] none acc
            else
              go (col - 1) [] none ({ numbers := currentNums, op } :: acc)
          | none => go (col - 1) [] none acc
        else
          let maybeNum := parseNumberFromColumn numRows c
          let maybeOp := getOpFromColumn opRow c
          let newNums := match maybeNum with
            | some n => n :: currentNums
            | none => currentNums
          let newOp := match maybeOp with
            | some op => some op
            | none => currentOp
          go (col - 1) newNums newOp acc
    go (maxCol - 1) [] none []

def solvePart2 (input : String) : Nat :=
  let problems := parseProblemsVertical input
  problems.map evalProblem |>.foldl (· + ·) 0

def run (inputPath : String) : IO Unit := do
  let input ← readFile inputPath
  IO.println s!"Day 6 Part 1: {solvePart1 input}"
  IO.println s!"Day 6 Part 2: {solvePart2 input}"

end AoC2025.Day6
