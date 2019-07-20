import "../prims"
import "../basic_combinators"

let v: validator = range_match "az" |> validator

-- Basic range_match test
-------------------------
-- input {"abc"} output {true}
-- input {"a"} output {true}
-- input {""} output {false}
-- input {"Abc"} output {false}
-- input {"aBc"} output {false}
-- input {"abC"} output {false}
-- input {"zzz"} output {true}
-- input {"abcdefghijklmnopqrstuvwxyz"} output {true}

entry main (s: string): bool =
  v (new_mbpd s)
