import "prims"

local type mbpd = Maybe parse_data
type matcher = mbpd -> mbpd
type validator = mbpd -> bool

let end_match (pd: mbpd): mbpd =
  match pd
  case #Nothing -> #Nothing
  case #Just x  -> if x.pos == x.len - 1 then
                     #Just x
                   else
                     #Nothing

let validate (pd: mbpd): bool =
  match pd
  case #Nothing -> false
  case #Just _  -> true

let validatehl (m: matcher): validator = m >-> validate

let or_e (f: matcher) (g: matcher) (pd: mbpd): mbpd =
  match f pd
  case #Nothing -> g pd
  case x        -> x

let seq_e (f: matcher) (g: matcher) (pd: mbpd): mbpd =
  match f pd
  case #Nothing -> #Nothing
  case x        -> g x

let char_match (c: char) (pd: mbpd): mbpd =
  match pd
  case #Nothing -> #Nothing
  case #Just x -> if or [x.pos >= x.len, x.data[x.pos] != c] then
                    #Nothing
                  else
                    #Just (x with pos = x.pos + 1)

let set_match [n] (set: [n]char) (pd: mbpd): mbpd =
  if n == 0 then pd else match pd
  case #Nothing -> #Nothing
  case _        -> let pds: [n]mbpd = map (\c -> char_match c pd) set in
                   let matches: []mbpd = filter validate pds in
                   if null matches then #Nothing else head matches

local let mkrange (r: [2]char): []char =
  let (r0, r1): (char, char) = (u8.min r[0] r[1], u8.max r[0] r[1]) in
  r0...r1

let range_match (r: [2]char): matcher =
  set_match (mkrange r)

let ranges_match (r: [][2]char): matcher =
  let ranges: []char = flatten (map mkrange r) in
  set_match ranges
