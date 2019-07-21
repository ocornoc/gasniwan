import "../../prims"
import "../../combinators"

let v: validator = range_match "az" |> validatehl

-- Basic range_match test
-- ======================
-- input { [0x61u8] } output { true }
-- input { [0x62u8] } output { true }
-- input { [0x7Au8] } output { true }
-- input { [0x61u8, 0x31u8] } output { true }
-- input { [0x31u8] } output { false }
-- input { [0x31u8, 0x61u8] } output { false }

entry main (s: string): bool =
  v (new_mbpd s)
