/- Day 6: Trash Compactor -/
import AoC2025.Util

namespace AoC2025.Day6

open AoC2025.Util

inductive Op where
  | add
  | mul
  deriving Repr, BEq

structure Problem where
  numbers : List Nat
  op : Op

def parseOp (c : Char) : Option Op :=
  match c with
  | '+' => some Op.add
  | '*' => some Op.mul
  | _ => none

def evalProblem (p : Problem) : Nat :=
  match p.op with
  | Op.add => p.numbers.foldl (· + ·) 0
  | Op.mul => p.numbers.foldl (· * ·) 1

def sumResults (problems : List Problem) : Nat :=
  problems.map evalProblem |>.foldl (· + ·) 0

def splitInput (input : String) (useTrim : Bool) : Option (List String × String) :=
  let allLines := if useTrim then lines input else linesRaw input
  if allLines.length < 2 then none
  else some (allLines.reverse.tail!.reverse, allLines.reverse.head!)

def padRows (rows : List String) : List String :=
  let maxLen := rows.foldl (fun acc r => max acc r.length) 0
  rows.map (fun r => r ++ String.ofList (List.replicate (maxLen - r.length) ' '))

def getColumn (rows : List String) (col : Nat) : List Char :=
  rows.map (fun r => r.toList.getD col ' ')

def isSeparator (rows : List String) (col : Nat) : Bool :=
  (getColumn rows col).all (· == ' ')

def columnToNumber (rows : List String) (col : Nat) : Option Nat :=
  let digits := (getColumn rows col).filter Char.isDigit
  if digits.isEmpty then none else (String.ofList digits).toNat?

def columnToOp (opRow : String) (col : Nat) : Option Op :=
  opRow.toList[col]? |>.bind parseOp

def parseNumbersInRow (row : String) : List (Nat × Nat × Nat) :=
  let rec go (pos : Nat) (acc : List (Nat × Nat × Nat)) (cur : String) (start : Nat) :=
    if pos >= row.length then
      match cur.toNat? with
      | some n => (n, start, pos - 1) :: acc
      | none => acc
    else
      let c := row.toList.getD pos ' '
      if c.isDigit then
        go (pos + 1) acc (if cur.isEmpty then String.singleton c else cur ++ String.singleton c)
           (if cur.isEmpty then pos else start)
      else
        match cur.toNat? with
        | some n => go (pos + 1) ((n, start, pos - 1) :: acc) "" pos
        | none => go (pos + 1) acc "" pos
  (go 0 [] "" 0).reverse

def mergeRanges (ranges : List (Nat × Nat)) : List (Nat × Nat) :=
  let sorted := ranges.toArray.qsort (fun a b => a.1 < b.1) |>.toList
  match sorted with
  | [] => []
  | first :: rest =>
    let (merged, last) := rest.foldl (fun (acc, cur) next =>
      if next.1 <= cur.2 + 1 then (acc, (cur.1, max cur.2 next.2))
      else (cur :: acc, next)) ([], first)
    (last :: merged).reverse

def findOpInRange (opRow : String) (start stop : Nat) : Option Op :=
  List.range (stop - start + 1)
    |>.filterMap (fun off => columnToOp opRow (start + off))
    |>.head?

def numbersInRange (nums : List (Nat × Nat × Nat)) (start stop : Nat) : List Nat :=
  nums.filter (fun (_, s, e) => s <= stop && e >= start) |>.map (·.1)

def parseHorizontal (input : String) : List Problem :=
  match splitInput input true with
  | none => []
  | some (numRows, opRow) =>
    let perRow := numRows.map parseNumbersInRow
    let ranges := perRow.foldl (fun acc row =>
      row.map (fun (_, s, e) => (s, e)) ++ acc) []
    let merged := mergeRanges ranges
    merged.filterMap fun (start, stop) =>
      match findOpInRange opRow start stop with
      | none => none
      | some op =>
        let nums := perRow.foldl (fun acc row => acc ++ numbersInRange row start stop) []
        if nums.isEmpty then none else some { numbers := nums, op }

def parseVertical (input : String) : List Problem :=
  match splitInput input false with
  | none => []
  | some (numRows, opRow) =>
    let padded := padRows numRows
    let maxCol := padded.foldl (fun acc r => max acc r.length) 0
    let rec go (remaining : Nat) (nums : List Nat) (op : Option Op) (acc : List Problem) :=
      match remaining with
      | 0 =>
        match op with
        | some o => if nums.isEmpty then acc else { numbers := nums, op := o } :: acc
        | none => acc
      | remaining' + 1 =>
        let c := remaining'
        if isSeparator padded c then
          match op with
          | some o => go remaining' [] none (if nums.isEmpty then acc else { numbers := nums, op := o } :: acc)
          | none => go remaining' [] none acc
        else
          let newNums := match columnToNumber padded c with
            | some n => n :: nums
            | none => nums
          let newOp := match columnToOp opRow c with
            | some o => some o
            | none => op
          go remaining' newNums newOp acc
    go maxCol [] none []

def solvePart1 (input : String) : Nat := sumResults (parseHorizontal input)

def solvePart2 (input : String) : Nat := sumResults (parseVertical input)

def run (inputPath : String) : IO Unit := do
  let input ← readFile inputPath
  IO.println s!"Day 6 Part 1: {solvePart1 input}"
  IO.println s!"Day 6 Part 2: {solvePart2 input}"

end AoC2025.Day6
