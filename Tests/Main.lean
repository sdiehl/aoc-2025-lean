import AoC2025

def main : IO UInt32 := do
  let inputDir := "inputs"

  IO.println "Running all Advent of Code 2025 solutions..."
  IO.println ""

  IO.println "=== Day 1 ==="
  AoC2025.Day1.run s!"{inputDir}/day1.txt"
  IO.println ""

  IO.println "=== Day 2 ==="
  AoC2025.Day2.run s!"{inputDir}/day2.txt"
  IO.println ""

  IO.println "=== Day 3 ==="
  AoC2025.Day3.run s!"{inputDir}/day3.txt"
  IO.println ""

  IO.println "=== Day 4 ==="
  AoC2025.Day4.run s!"{inputDir}/day4.txt"
  IO.println ""

  IO.println "=== Day 5 ==="
  AoC2025.Day5.run s!"{inputDir}/day5.txt"
  IO.println ""

  IO.println "=== Day 6 ==="
  AoC2025.Day6.run s!"{inputDir}/day6.txt"
  IO.println ""

  IO.println "All days completed."
  return 0
