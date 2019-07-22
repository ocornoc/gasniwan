import "../../prims"
import "../../combinators"
import "../../combinator_ops"

let v: validator = validatehl <| end_match

-- Testing set_match
-- ==================
-- input { empty(u8) } output { true }
-- input { [0x31u8] } output { false }

entry main (s: string): bool =
  v (new_mbpd s)
