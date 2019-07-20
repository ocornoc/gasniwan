type char = u8
type string = []char
type Maybe 't = #Nothing | #Just t
type parse_data = {data: string, len: i32, pos: i32}

let new_pd (s: string): parse_data = {data = s, len = length s, pos = 0}
let new_mbpd (s: string): Maybe parse_data = #Just (new_pd s)
