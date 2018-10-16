  type t = int
  let zero = 0
  let add a b = let s = a + b in if s > 100 then 100 else if s < 0 then 0 else s
  let sub a b = add a (-b)