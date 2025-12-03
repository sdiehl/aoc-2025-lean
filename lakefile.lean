import Lake
open Lake DSL

require batteries from git
  "https://github.com/leanprover-community/batteries" @ "main"

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

@[test_driver]
lean_exe tests where
  root := `Tests.Main
  srcDir := "."

@[lint_driver]
lean_exe lint where
  root := `Lint.Main
  srcDir := "."
