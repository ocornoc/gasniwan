import "../../prims"
import "../../combinators"
import "../../combinator_ops"

let v1: validator = validatehl <|
  (literal_match "Hello, world!" | literal_match "hello, world!")
  * end_match

let v2: validator = validatehl <| set_match "Hh" *
  literal_match "ello, world!" * end_match

-- Testing literal, set, and empty matching
-- =========================================
-- input { [0x48u8, 0x65, 0x6c, 0x6c, 0x6f,
--  0x2c, 0x20, 0x77, 0x6f, 0x72, 0x6c, 0x64, 0x21] } output { [true, true] }
-- input { [0x68u8, 0x65, 0x6c, 0x6c, 0x6f,
--  0x2c, 0x20, 0x77, 0x6f, 0x72, 0x6c, 0x64, 0x21] } output { [true, true] }
-- input { [0x42u8, 0x79, 0x65, 0x2c, 0x20,
--  0x77, 0x6f, 0x72, 0x6c, 0x64, 0x21] } output { [false, false] }
-- input { [0x62u8, 0x79, 0x65, 0x2c, 0x20,
--  0x77, 0x6f, 0x72, 0x6c, 0x64, 0x21] } output { [false, false] }
-- input { [0x48u8, 0x65, 0x6c, 0x6c, 0x6f,
--  0x2c, 0x20, 0x77, 0x6f, 0x72, 0x6c, 0x64, 0x3f] } output { [false, false] }
-- input { [0x68u8, 0x65, 0x6c, 0x6c, 0x6f,
--  0x2c, 0x20, 0x77, 0x6f, 0x72, 0x6c, 0x64, 0x3f] } output { [false, false] }
-- input { [0x48u8, 0x65, 0x6c, 0x6c, 0x6f, 0x2c,
--  0x20, 0x77, 0x6f, 0x72, 0x6c, 0x64, 0x21, 0x3f] } output { [false, false] }
-- input { [0x68u8, 0x65, 0x6c, 0x6c, 0x6f, 0x2c,
--  0x20, 0x77, 0x6f, 0x72, 0x6c, 0x64, 0x21, 0x3f] } output { [false, false] }

entry main (s: string): [2]bool =
  let s: Maybe parse_data = new_mbpd s in
  [v1 s, v2 s]
