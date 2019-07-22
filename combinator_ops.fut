import "prims"
import "combinators"

let (m1: matcher) * (m2: matcher): matcher = seq_e m1 m2
let (m1: matcher) / (m2: matcher): matcher = or_e m1 m2
let (m1: matcher) | (m2: matcher): matcher = or_e m1 m2
let (m: matcher) ^< (n: i32): matcher = upto_e m n
let (m: matcher) ^> (n: i32): matcher = atleast_e m n
let (m: matcher) ^>< (n: (i32, i32)): matcher = between_e m n
