let int_sqrt (i : int) : int =
  let rec sqrt_aux s =
    if s * s = i then s
    else sqrt_aux (s + 1) in
    sqrt_aux 0
  
let rec int_power (i: int) (p : int) : int =
  if p = 0 then 1
  else i * int_power i (p - 1)

let filteri (f : int -> 'a -> bool) (lst : 'a list) = 
  let rec filter_aux index lst acc = match lst with 
    | [] -> acc
    | x :: xs when f index x -> filter_aux (index + 1) xs (acc @ [x])
    | x :: xs -> filter_aux (index + 1) xs acc
  in filter_aux 0 lst []

let range start stop =
  List.init (stop - start) (fun i -> i + start) 

let rec any f lst = match lst with
| [] -> false
| head :: tail -> f head || any f tail