import Lake
open Lake DSL

package AoC2025 where
  leanOptions := #[
    ⟨`pp.unicode.fun, true⟩,
    ⟨`autoImplicit, false⟩,
    ⟨`relaxedAutoImplicit, false⟩
  ]

@[default_target]
lean_lib AoC2025 where
  srcDir := "."

@[default_target]
lean_exe aoc2025 where
  root := `Main
  srcDir := "."
