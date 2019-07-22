import "prims"
import "combinators"

let (m1: matcher) * (m2: matcher): matcher = seq_e m1 m2
let (m1: matcher) / (m2: matcher): matcher = or_e m1 m2
let (m1: matcher) | (m2: matcher): matcher = or_e m1 m2
