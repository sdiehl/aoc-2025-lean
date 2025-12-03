import AoC2025

def main (args : List String) : IO Unit := do
  let day := args.head?.getD "1"
  let inputDir := "/Users/sdiehl/advent_code"

  match day with
  | "1" => AoC2025.Day1.run s!"{inputDir}/input"
  | "2" => AoC2025.Day2.run s!"{inputDir}/input2"
  | "3" => AoC2025.Day3.run s!"{inputDir}/input3"
  | "all" => do
    AoC2025.Day1.run s!"{inputDir}/input"
    AoC2025.Day2.run s!"{inputDir}/input2"
    AoC2025.Day3.run s!"{inputDir}/input3"
  | _ => IO.println s!"Unknown day: {day}. Use 1, 2, 3, or all."
