/- Day 10: Factory -/
import AoC2025.Util

namespace AoC2025.Day10

open AoC2025.Util

structure Machine where
  target : Array Bool
  buttons : Array (Array Bool)
  joltage : Array Nat
  deriving Repr

def parseTarget (s : String) : Array Bool :=
  s.toList.toArray.map (· == '#')

def parseButton (s : String) (n : Nat) : Array Bool :=
  let indices := s.splitOn "," |>.filterMap String.toNat?
  Array.range n |>.map (fun i => indices.contains i)

def parseJoltage (s : String) : Array Nat :=
  s.splitOn "," |>.filterMap String.toNat? |>.toArray

def parseMachine (line : String) : Option Machine := do
  let chars := line.toList
  let targetStart := chars.findIdx? (· == '[')
  let targetEnd := chars.findIdx? (· == ']')
  let joltStart := chars.findIdx? (· == '{')
  let joltEnd := chars.findIdx? (· == '}')
  match targetStart, targetEnd, joltStart, joltEnd with
  | some s, some e, some js, some je =>
    let targetChars := chars.drop (s + 1) |>.take (e - s - 1)
    let target := targetChars.toArray.map (· == '#')
    let joltChars := chars.drop (js + 1) |>.take (je - js - 1)
    let joltage := parseJoltage (String.ofList joltChars)
    let rest := String.ofList (chars.drop e)
    let beforeJolt := rest.takeWhile (· != '{')
    let buttonStrs := beforeJolt.splitOn "(" |>.drop 1 |>.map (·.takeWhile (· != ')'))
    let buttons := buttonStrs.map (parseButton · joltage.size) |>.toArray
    some { target, buttons, joltage }
  | _, _, _, _ => none

/-- XOR two boolean arrays -/
def xorArrays (a b : Array Bool) : Array Bool :=
  Array.range a.size |>.map (fun i => a[i]! != b[i]!)

/-- Apply a subset of buttons (represented as bitmask) to get resulting state -/
def applyButtons (buttons : Array (Array Bool)) (mask : Nat) : Array Bool :=
  let n := if buttons.size > 0 then buttons[0]!.size else 0
  let initial := (Array.range n).map (fun _ => false)
  let rec go (idx : Nat) (acc : Array Bool) : Array Bool :=
    if idx >= buttons.size then acc
    else
      let newAcc := if (mask >>> idx) &&& 1 == 1 then xorArrays acc buttons[idx]! else acc
      go (idx + 1) newAcc
  termination_by buttons.size - idx
  go 0 initial

/-- Count number of 1 bits in a natural number -/
def popCount (n : Nat) : Nat :=
  let rec go (m : Nat) (acc : Nat) (fuel : Nat) : Nat :=
    match fuel with
    | 0 => acc
    | fuel' + 1 =>
      if m == 0 then acc
      else go (m >>> 1) (acc + (m &&& 1)) fuel'
  go n 0 64

/-- Find minimum button presses to reach target by trying all combinations -/
def solveMinPresses (m : Machine) : Nat :=
  let numButtons := m.buttons.size
  let limit := 1 <<< numButtons
  let rec search (mask : Nat) (best : Nat) : Nat :=
    if mask >= limit then best
    else
      let result := applyButtons m.buttons mask
      if result == m.target then
        let presses := popCount mask
        search (mask + 1) (min best presses)
      else
        search (mask + 1) best
  termination_by limit - mask
  search 0 (numButtons + 1)

def solvePart1 (input : String) : Nat :=
  let machines := (lines input).filterMap parseMachine
  machines.foldl (fun acc m => acc + solveMinPresses m) 0

/-! ## Part 2: Integer linear combination to reach joltage target

This is an integer linear programming problem: find non-negative integers x_i
such that sum(x_i * button_i) = target, minimizing sum(x_i).

We use iterative deepening with pruning. -/

/-- Apply button presses (array of counts) to get joltage levels -/
def applyJoltage (buttons : Array (Array Bool)) (presses : Array Nat) : Array Nat := Id.run do
  let n := if buttons.size > 0 then buttons[0]!.size else 0
  let mut result := (Array.range n).map (fun _ => 0)
  for idx in [:buttons.size] do
    let count := presses[idx]!
    let btn := buttons[idx]!
    for i in [:btn.size] do
      if btn[i]! then
        result := result.set! i (result[i]! + count)
  return result

/-- Check if we've exceeded target on any counter -/
def exceedsTarget (current target : Array Nat) : Bool := Id.run do
  for i in [:current.size] do
    if current[i]! > target[i]! then return true
  return false

/-- Check if current equals target -/
def matchesTarget (current target : Array Nat) : Bool :=
  current == target

/-- Compute how many counters a button affects -/
def buttonCoverage (btn : Array Bool) : Nat :=
  btn.foldl (fun acc b => if b then acc + 1 else acc) 0

/-- Convert buttons to indexed list for sorting -/
def indexedButtons (buttons : Array (Array Bool)) : Array (Nat × Array Bool) :=
  buttons.mapIdx (fun i btn => (i, btn))

/-- Sort buttons by coverage (more coverage = more efficient) -/
def sortByEfficiency (buttons : Array (Array Bool)) : Array (Nat × Array Bool) :=
  let indexed := indexedButtons buttons
  indexed.toList.mergeSort (fun (_, a) (_, b) => buttonCoverage a > buttonCoverage b) |>.toArray

/-- Branch and bound with state passed along -/
def bbSearch (buttons : Array (Array Bool)) (target : Array Nat)
    (current : Array Nat) (idx : Nat) (currentSum : Nat) (best : Nat) : Nat :=
  -- Check if already at target
  if current == target then currentSum
  -- Check if past all buttons
  else if idx >= buttons.size then best
  else
    let n := target.size
    -- Check for exceeded counters (infeasible)
    let exceeded := Id.run do
      for j in [:n] do
        if current[j]! > target[j]! then return true
      return false
    if exceeded then best
    else
      -- Lower bound: max remaining gap
      let lowerBound := Id.run do
        let mut maxGap := 0
        for j in [:n] do
          let gap := target[j]! - current[j]!
          if gap > maxGap then maxGap := gap
        return maxGap

      if currentSum + lowerBound >= best then best
      else
        let btn := buttons[idx]!
        -- Max useful presses for this button
        let maxUseful := Id.run do
          let mut m := best - currentSum
          for j in [:n] do
            if btn[j]! then
              let gap := target[j]! - current[j]!
              m := min m gap
          return m

        -- Try each count
        Id.run do
          let mut bestNow := best
          for count in [:maxUseful + 1] do
            -- Apply this button's effect
            let newCurrent := Id.run do
              let mut c := current
              for j in [:n] do
                if btn[j]! then c := c.set! j (current[j]! + count)
              return c
            let result := bbSearch buttons target newCurrent (idx + 1) (currentSum + count) bestNow
            if result < bestNow then bestNow := result
          return bestNow
termination_by buttons.size - idx

/-- Solve joltage using branch and bound -/
def solveJoltage (buttons : Array (Array Bool)) (target : Array Nat) : Nat :=
  let n := target.size
  let initState := (Array.range n).map (fun _ => 0)
  let upperBound := target.foldl (· + ·) 0 + 1
  let result := bbSearch buttons target initState 0 0 upperBound
  if result >= upperBound then 0 else result

/-- Sort buttons by coverage (fewest affected counters first - more constrained) -/
def sortButtonsByCoverage (buttons : Array (Array Bool)) : Array (Array Bool) :=
  let indexed := buttons.mapIdx (fun i btn => (buttonCoverage btn, i, btn))
  let sorted := indexed.toList.mergeSort (fun (c1, _, _) (c2, _, _) => c1 < c2)
  sorted.map (fun (_, _, btn) => btn) |>.toArray

def solveMinJoltage (m : Machine) : Nat :=
  let sortedButtons := sortButtonsByCoverage m.buttons
  solveJoltage sortedButtons m.joltage

/-- Generate LP file content for a single machine's joltage problem -/
def generateLP (buttons : Array (Array Bool)) (target : Array Nat) : String := Id.run do
  let m := buttons.size
  let n := target.size

  -- Objective: minimize sum of x_i
  let mut obj := "Minimize\n obj:"
  for i in [:m] do
    obj := obj ++ s!" + x{i}"
  obj := obj ++ "\n"

  -- Constraints: for each counter j, sum of x_i where button i affects j = target[j]
  let mut constraints := "Subject To\n"
  for j in [:n] do
    let mut first := true
    let mut constr := s!" c{j}:"
    for i in [:m] do
      if buttons[i]![j]! then
        if first then
          constr := constr ++ s!" x{i}"
          first := false
        else
          constr := constr ++ s!" + x{i}"
    constr := constr ++ s!" = {target[j]!}\n"
    constraints := constraints ++ constr

  -- Bounds: all x_i >= 0
  let mut bounds := "Bounds\n"
  for i in [:m] do
    bounds := bounds ++ s!" x{i} >= 0\n"

  -- General (integer) variables
  let mut general := "General\n"
  for i in [:m] do
    general := general ++ s!" x{i}"
  general := general ++ "\nEnd\n"

  return obj ++ constraints ++ bounds ++ general

/-- Parse HiGHS solution file to extract objective value -/
def parseSolution (content : String) : Nat :=
  let lines := content.splitOn "\n"
  let objLine := lines.find? (·.startsWith "Objective ")
  match objLine with
  | some line =>
    let parts := line.splitOn " "
    if parts.length >= 2 then
      match parts[1]!.toNat? with
      | some n => n
      | none => 0
    else 0
  | none => 0

/-- Solve a single machine using HiGHS ILP solver.
    Uses temp files as scratch buffers, cleaned up after each solve. -/
def solveWithHiGHS (m : Machine) : IO Nat := do
  let lpContent := generateLP m.buttons m.joltage
  let lpFile := "/tmp/aoc_day10.lp"
  let solFile := "/tmp/aoc_day10.sol"

  IO.FS.writeFile lpFile lpContent

  let result ← IO.Process.output {
    cmd := "highs"
    args := #[lpFile, "--solution_file", solFile]
    stdin := .null
  }

  let objective ← if result.exitCode == 0 then
    let solContent ← IO.FS.readFile solFile
    pure (parseSolution solContent)
  else
    pure 0

  -- Clean up temp files
  IO.FS.removeFile lpFile <|> pure ()
  IO.FS.removeFile solFile <|> pure ()

  return objective

/-- Solve Part 2 using HiGHS for each machine -/
def solvePart2IO (input : String) : IO Nat := do
  let machines := (lines input).filterMap parseMachine
  let mut total := 0
  for m in machines do
    let result ← solveWithHiGHS m
    total := total + result
  return total

unsafe def solvePart2Unsafe (input : String) : Nat :=
  unsafeIO (solvePart2IO input) |>.toOption.getD 0

@[implemented_by solvePart2Unsafe]
def solvePart2 (input : String) : Nat :=
  -- Implemented by solvePart2Unsafe which shells out to HiGHS ILP solver
  0

def run (path : String) : IO Unit := do
  let input ← IO.FS.readFile path
  IO.println s!"Day 10 Part 1: {solvePart1 input}"
  let part2 ← solvePart2IO input
  IO.println s!"Day 10 Part 2: {part2}"

end AoC2025.Day10
