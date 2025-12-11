/- Day 11: Reactor -/
import AoC2025.Util
import Batteries.Data.HashMap

namespace AoC2025.Day11

open AoC2025.Util

abbrev Graph := Batteries.HashMap String (List String)

def parseGraph (input : String) : Graph := Id.run do
  let mut graph : Graph := Batteries.HashMap.empty
  for line in lines input do
    let parts := line.splitOn ": "
    if parts.length >= 2 then
      let node := parts[0]!
      let neighbors := parts[1]!.splitOn " " |>.filter (· != "")
      graph := graph.insert node neighbors
  return graph

abbrev Memo := Batteries.HashMap String Nat

def countPathsMemo (graph : Graph) (goal : String) : Memo := Id.run do
  let nodes := graph.toList.map (·.1)
  let mut memo : Memo := Batteries.HashMap.empty
  memo := memo.insert goal 1
  let mut changed := true
  let mut iters := 0
  while changed && iters < 1000 do
    changed := false
    iters := iters + 1
    for node in nodes do
      if !memo.contains node then
        match graph.find? node with
        | none => pure ()
        | some neighbors =>
          if neighbors.all memo.contains then
            let count := neighbors.foldl (fun acc n => acc + (memo.find? n).getD 0) 0
            memo := memo.insert node count
            changed := true
  return memo

def solvePart1 (input : String) : Nat :=
  let graph := parseGraph input
  let memo := countPathsMemo graph "out"
  (memo.find? "you").getD 0

abbrev Memo2 := Batteries.HashMap (String × Bool × Bool) Nat

def countPathsMemo2 (graph : Graph) (goal req1 req2 : String) : Memo2 := Id.run do
  let nodes := graph.toList.map (·.1)
  let mut memo : Memo2 := Batteries.HashMap.empty
  for b1 in [false, true] do
    for b2 in [false, true] do
      if b1 && b2 then
        memo := memo.insert (goal, b1, b2) 1
      else
        memo := memo.insert (goal, b1, b2) 0
  let mut changed := true
  let mut iters := 0
  while changed && iters < 1000 do
    changed := false
    iters := iters + 1
    for node in nodes do
      for b1 in [false, true] do
        for b2 in [false, true] do
          if !memo.contains (node, b1, b2) then
            match graph.find? node with
            | none => pure ()
            | some neighbors =>
              let b1' := b1 || node == req1
              let b2' := b2 || node == req2
              if neighbors.all (fun n => memo.contains (n, b1', b2')) then
                let count := neighbors.foldl (fun acc n =>
                  acc + (memo.find? (n, b1', b2')).getD 0) 0
                memo := memo.insert (node, b1, b2) count
                changed := true
  return memo

def solvePart2 (input : String) : Nat :=
  let graph := parseGraph input
  let memo := countPathsMemo2 graph "out" "dac" "fft"
  (memo.find? ("svr", false, false)).getD 0

def run (path : String) : IO Unit := do
  let input ← IO.FS.readFile path
  IO.println s!"Day 11 Part 1: {solvePart1 input}"
  IO.println s!"Day 11 Part 2: {solvePart2 input}"

end AoC2025.Day11
