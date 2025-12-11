import AoC2025

def loadExpected (path : String) : IO (String × String) := do
  let content ← IO.FS.readFile path
  let lines := content.splitOn "\n" |>.filter (!·.isEmpty)
  match lines with
  | [p1, p2] => return (p1.trim, p2.trim)
  | _ => return ("", "")

def check (name : String) (actual expected : String) : IO Bool := do
  if actual == expected then
    IO.println s!"  {name}: {actual} [PASS]"
    return true
  else
    IO.println s!"  {name}: {actual} [FAIL] (expected {expected})"
    return false

def runDay1 (inputPath answerPath : String) : IO Bool := do
  let input ← IO.FS.readFile inputPath
  let (exp1, exp2) ← loadExpected answerPath
  let p1 := toString (AoC2025.Day1.solvePart1 input)
  let p2 := toString (AoC2025.Day1.solvePart2 input)
  let r1 ← check "Part 1" p1 exp1
  let r2 ← check "Part 2" p2 exp2
  return r1 && r2

def runDay2 (inputPath answerPath : String) : IO Bool := do
  let input ← IO.FS.readFile inputPath
  let (exp1, exp2) ← loadExpected answerPath
  let p1 := toString (AoC2025.Day2.solvePart1 input)
  let p2 := toString (AoC2025.Day2.solvePart2 input)
  let r1 ← check "Part 1" p1 exp1
  let r2 ← check "Part 2" p2 exp2
  return r1 && r2

def runDay3 (inputPath answerPath : String) : IO Bool := do
  let input ← IO.FS.readFile inputPath
  let (exp1, exp2) ← loadExpected answerPath
  let p1 := toString (AoC2025.Day3.solvePart1 input)
  let p2 := toString (AoC2025.Day3.solvePart2 input)
  let r1 ← check "Part 1" p1 exp1
  let r2 ← check "Part 2" p2 exp2
  return r1 && r2

def runDay4 (inputPath answerPath : String) : IO Bool := do
  let input ← IO.FS.readFile inputPath
  let (exp1, exp2) ← loadExpected answerPath
  let p1 := toString (AoC2025.Day4.solvePart1 input)
  let p2 := toString (AoC2025.Day4.solvePart2 input)
  let r1 ← check "Part 1" p1 exp1
  let r2 ← check "Part 2" p2 exp2
  return r1 && r2

def runDay5 (inputPath answerPath : String) : IO Bool := do
  let input ← IO.FS.readFile inputPath
  let (exp1, exp2) ← loadExpected answerPath
  let p1 := toString (AoC2025.Day5.solvePart1 input)
  let p2 := toString (AoC2025.Day5.solvePart2 input)
  let r1 ← check "Part 1" p1 exp1
  let r2 ← check "Part 2" p2 exp2
  return r1 && r2

def runDay6 (inputPath answerPath : String) : IO Bool := do
  let input ← IO.FS.readFile inputPath
  let (exp1, exp2) ← loadExpected answerPath
  let p1 := toString (AoC2025.Day6.solvePart1 input)
  let p2 := toString (AoC2025.Day6.solvePart2 input)
  let r1 ← check "Part 1" p1 exp1
  let r2 ← check "Part 2" p2 exp2
  return r1 && r2

def runDay7 (inputPath answerPath : String) : IO Bool := do
  let input ← IO.FS.readFile inputPath
  let (exp1, exp2) ← loadExpected answerPath
  let p1 := toString (AoC2025.Day7.solvePart1 input)
  let p2 := toString (AoC2025.Day7.solvePart2 input)
  let r1 ← check "Part 1" p1 exp1
  let r2 ← check "Part 2" p2 exp2
  return r1 && r2

def runDay8 (inputPath answerPath : String) : IO Bool := do
  let input ← IO.FS.readFile inputPath
  let (exp1, exp2) ← loadExpected answerPath
  let p1 := toString (AoC2025.Day8.solvePart1 input)
  let p2 := toString (AoC2025.Day8.solvePart2 input)
  let r1 ← check "Part 1" p1 exp1
  let r2 ← check "Part 2" p2 exp2
  return r1 && r2

def runDay9 (inputPath answerPath : String) : IO Bool := do
  let input ← IO.FS.readFile inputPath
  let (exp1, exp2) ← loadExpected answerPath
  let p1 := toString (AoC2025.Day9.solvePart1 input)
  let p2 := toString (AoC2025.Day9.solvePart2 input)
  let r1 ← check "Part 1" p1 exp1
  let r2 ← check "Part 2" p2 exp2
  return r1 && r2

def runDay10 (inputPath answerPath : String) : IO Bool := do
  let input ← IO.FS.readFile inputPath
  let (exp1, exp2) ← loadExpected answerPath
  let p1 := toString (AoC2025.Day10.solvePart1 input)
  let p2 := toString (← AoC2025.Day10.solvePart2 input)
  let r1 ← check "Part 1" p1 exp1
  let r2 ← check "Part 2" p2 exp2
  return r1 && r2

def runDay11 (inputPath answerPath : String) : IO Bool := do
  let input ← IO.FS.readFile inputPath
  let (exp1, exp2) ← loadExpected answerPath
  let p1 := toString (AoC2025.Day11.solvePart1 input)
  let p2 := toString (AoC2025.Day11.solvePart2 input)
  let r1 ← check "Part 1" p1 exp1
  let r2 ← check "Part 2" p2 exp2
  return r1 && r2

def main : IO UInt32 := do
  let inputDir := "inputs"
  let answerDir := "answers"
  IO.println "Running Advent of Code 2025 test suite..."
  IO.println ""

  let mut allPassed := true

  IO.println "Day 1: Secret Entrance"
  allPassed := allPassed && (← runDay1 s!"{inputDir}/day1.txt" s!"{answerDir}/day1.txt")

  IO.println "Day 2: Gift Shop"
  allPassed := allPassed && (← runDay2 s!"{inputDir}/day2.txt" s!"{answerDir}/day2.txt")

  IO.println "Day 3: Lobby"
  allPassed := allPassed && (← runDay3 s!"{inputDir}/day3.txt" s!"{answerDir}/day3.txt")

  IO.println "Day 4: Printing Department"
  allPassed := allPassed && (← runDay4 s!"{inputDir}/day4.txt" s!"{answerDir}/day4.txt")

  IO.println "Day 5: Cafeteria"
  allPassed := allPassed && (← runDay5 s!"{inputDir}/day5.txt" s!"{answerDir}/day5.txt")

  IO.println "Day 6: Trash Compactor"
  allPassed := allPassed && (← runDay6 s!"{inputDir}/day6.txt" s!"{answerDir}/day6.txt")

  IO.println "Day 7: Laboratories"
  allPassed := allPassed && (← runDay7 s!"{inputDir}/day7.txt" s!"{answerDir}/day7.txt")

  IO.println "Day 8: Playground"
  allPassed := allPassed && (← runDay8 s!"{inputDir}/day8.txt" s!"{answerDir}/day8.txt")

  IO.println "Day 9: Movie Theater"
  allPassed := allPassed && (← runDay9 s!"{inputDir}/day9.txt" s!"{answerDir}/day9.txt")

  IO.println "Day 10: Factory"
  allPassed := allPassed && (← runDay10 s!"{inputDir}/day10.txt" s!"{answerDir}/day10.txt")

  IO.println "Day 11: Reactor"
  allPassed := allPassed && (← runDay11 s!"{inputDir}/day11.txt" s!"{answerDir}/day11.txt")

  IO.println ""
  if allPassed then
    IO.println "All tests passed!"
    return 0
  else
    IO.println "Some tests failed!"
    return 1
