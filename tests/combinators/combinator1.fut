import "../../prims"
import "../../combinators"

let v: validator = ranges_match ["az", "AZ"] |> validatehl

-- Basic ranges_match test
-- =======================
-- input { [0x48u8] } output { true }
-- input { [0x68u8] } output { true }

entry main (s: string): bool =
	v (new_mbpd s)
