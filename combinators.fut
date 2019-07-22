import "prims"

local type mbpd = Maybe parse_data
type matcher = mbpd -> mbpd
type validator = mbpd -> bool

let end_match (pd: mbpd): mbpd =
  match pd
  case #Nothing -> #Nothing
  case #Just x  -> if x.pos == x.len then
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

let star_e (m: matcher) (pd: mbpd): mbpd =
  loop pd
  while validate (m pd) do m pd

let optional_e (m: matcher) (pd: mbpd): mbpd =
  match m pd
  case #Nothing -> pd
  case _        -> m pd

let plus_e (m: matcher): matcher =
  seq_e m (star_e m)

let and_e (m: matcher) (pd: mbpd): mbpd =
  match m pd
  case #Nothing -> #Nothing
  case _        -> pd

let atleast_e (m: matcher) (n: i32) (pd: mbpd): mbpd =
  let n: i32 = assert (n >= 0) n in
  if or [n == 0, pd == #Nothing] then star_e m pd else
  let pd: mbpd = loop pd
  for _ in [1...n] do match pd
    case #Nothing -> #Nothing
    case _        -> m pd in
  star_e m pd

let upto_e (m: matcher) (n: i32) (pd: mbpd): mbpd =
  let n: i32 = assert (n >= 0) n in
  if or [n == 0, pd == #Nothing] then pd else
  loop pd
  for _ in [1...n] do match (m pd)
    case #Nothing -> pd
    case _        -> optional_e m pd

let between_e (m: matcher) ((a, b): (i32, i32)): matcher =
  let b: i32 = assert (b >= a) b in
  let f: matcher = (\x -> (and_e (atleast_e m b)) x) in
  let g: matcher = (\x -> upto_e m a x) in
  seq_e f g

let not_e (m: matcher) (pd: mbpd): mbpd =
  match m pd
  case #Nothing -> pd
  case _        -> #Nothing

let literal_match (s: string) (pd: mbpd): mbpd =
  let n: i32 = length s in
  loop pd
  for i < n do match char_match s[i] pd
    case #Nothing -> #Nothing
    case x        -> x
