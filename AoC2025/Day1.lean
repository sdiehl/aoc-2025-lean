/- Day 1: Secret Entrance -/
import AoC2025.Util

namespace AoC2025.Day1

open AoC2025.Util

inductive Direction where
  | left
  | right
  deriving Repr, BEq

def parseDirection (c : Char) : Option Direction :=
  match c with
  | 'L' => some Direction.left
  | 'R' => some Direction.right
  | _ => none

structure Instruction where
  dir : Direction
  dist : Nat
  deriving Repr

def parseInstruction (s : String) : Option Instruction := do
  let chars := s.toList
  match chars with
  | c :: rest =>
    let dir ← parseDirection c
    let distStr := String.ofList rest
    let dist := distStr.toNat?
    match dist with
    | some d => some { dir := dir, dist := d }
    | none => none
  | [] => none

def move (pos : Nat) (instr : Instruction) : Nat :=
  match instr.dir with
  | Direction.left => (pos + 100 - (instr.dist % 100)) % 100
  | Direction.right => (pos + instr.dist) % 100

def countZerosLeft (pos : Nat) (dist : Nat) : Nat :=
  if pos > 0 && pos <= dist then
    (dist - pos) / 100 + 1
  else if pos == 0 then
    dist / 100
  else
    0

def countZerosRight (pos : Nat) (dist : Nat) : Nat :=
  let stepsToZero := if pos > 0 then 100 - pos else 100
  if dist >= stepsToZero then
    (dist - stepsToZero) / 100 + 1
  else
    0

def countZerosCrossed (pos : Nat) (instr : Instruction) : Nat :=
  match instr.dir with
  | Direction.left => countZerosLeft pos instr.dist
  | Direction.right => countZerosRight pos instr.dist

def solvePart1 (input : String) : Nat :=
  let instrs := lines input |>.filterMap parseInstruction
  let (_, count) := instrs.foldl
    (fun (pos, cnt) instr =>
      let newPos := move pos instr
      let newCnt := if newPos == 0 then cnt + 1 else cnt
      (newPos, newCnt))
    (50, 0)
  count

def solvePart2 (input : String) : Nat :=
  let instrs := lines input |>.filterMap parseInstruction
  let (_, count) := instrs.foldl
    (fun (pos, cnt) instr =>
      let zeros := countZerosCrossed pos instr
      let newPos := move pos instr
      (newPos, cnt + zeros))
    (50, 0)
  count

def run (inputPath : String) : IO Unit := do
  let input ← readFile inputPath
  IO.println s!"Day 1 Part 1: {solvePart1 input}"
  IO.println s!"Day 1 Part 2: {solvePart2 input}"

end AoC2025.Day1
