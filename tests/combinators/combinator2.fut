import "../../prims"
import "../../combinators"
import "../../combinator_ops"

let v: validator = validatehl <| set_match "10"

-- Testing set_match
-- ==================
-- input { [0x30u8] } output { true }
-- input { [0x31u8] } output { true }
-- input { [0x32u8] } output { false }
-- input { [0x2Fu8] } output { false }

entry main (s: string): bool =
  v (new_mbpd s)
