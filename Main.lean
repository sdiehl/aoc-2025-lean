import AoC2025

def main (args : List String) : IO Unit := do
  let day := args.head?.getD "1"
  let inputDir := "inputs"

  match day with
  | "1" => AoC2025.Day1.run s!"{inputDir}/day1.txt"
  | "2" => AoC2025.Day2.run s!"{inputDir}/day2.txt"
  | "3" => AoC2025.Day3.run s!"{inputDir}/day3.txt"
  | "4" => AoC2025.Day4.run s!"{inputDir}/day4.txt"
  | "5" => AoC2025.Day5.run s!"{inputDir}/day5.txt"
  | "6" => AoC2025.Day6.run s!"{inputDir}/day6.txt"
  | "7" => AoC2025.Day7.run s!"{inputDir}/day7.txt"
  | "8" => AoC2025.Day8.run s!"{inputDir}/day8.txt"
  | "9" => AoC2025.Day9.run s!"{inputDir}/day9.txt"
  | "all" => do
    AoC2025.Day1.run s!"{inputDir}/day1.txt"
    AoC2025.Day2.run s!"{inputDir}/day2.txt"
    AoC2025.Day3.run s!"{inputDir}/day3.txt"
    AoC2025.Day4.run s!"{inputDir}/day4.txt"
    AoC2025.Day5.run s!"{inputDir}/day5.txt"
    AoC2025.Day6.run s!"{inputDir}/day6.txt"
    AoC2025.Day7.run s!"{inputDir}/day7.txt"
    AoC2025.Day8.run s!"{inputDir}/day8.txt"
    AoC2025.Day9.run s!"{inputDir}/day9.txt"
  | _ => IO.println s!"Unknown day: {day}. Use 1-9 or 'all'."
