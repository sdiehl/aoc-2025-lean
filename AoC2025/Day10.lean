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

def xorArrays (a b : Array Bool) : Array Bool :=
  Array.range a.size |>.map (fun i => a[i]! != b[i]!)

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

def popCount (n : Nat) : Nat :=
  let rec go (m : Nat) (acc : Nat) (fuel : Nat) : Nat :=
    match fuel with
    | 0 => acc
    | fuel' + 1 =>
      if m == 0 then acc
      else go (m >>> 1) (acc + (m &&& 1)) fuel'
  go n 0 64

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

def generateLP (buttons : Array (Array Bool)) (target : Array Nat) : String := Id.run do
  let m := buttons.size
  let n := target.size

  let mut obj := "Minimize\n obj:"
  for i in [:m] do
    obj := obj ++ s!" + x{i}"
  obj := obj ++ "\n"

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

  let mut bounds := "Bounds\n"
  for i in [:m] do
    bounds := bounds ++ s!" x{i} >= 0\n"

  let mut general := "General\n"
  for i in [:m] do
    general := general ++ s!" x{i}"
  general := general ++ "\nEnd\n"

  return obj ++ constraints ++ bounds ++ general

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

-- Reuses fixed temp files as scratch buffers; safe since machines are solved sequentially
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

  IO.FS.removeFile lpFile <|> pure ()
  IO.FS.removeFile solFile <|> pure ()

  return objective

def solvePart2 (input : String) : IO Nat := do
  let machines := (lines input).filterMap parseMachine
  let mut total := 0
  for m in machines do
    total := total + (← solveWithHiGHS m)
  return total

def run (path : String) : IO Unit := do
  let input ← IO.FS.readFile path
  IO.println s!"Day 10 Part 1: {solvePart1 input}"
  IO.println s!"Day 10 Part 2: {← solvePart2 input}"

end AoC2025.Day10
