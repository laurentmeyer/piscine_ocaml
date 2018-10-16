module type WATCHTOWER =
sig
  type hour = int
  val zero : hour
  val add : hour -> hour -> hour
  val sub : hour -> hour -> hour
end

module Watchtower : WATCHTOWER =
struct
  type hour = int
  let zero = 12
  let add a b = let m = (a + b) mod zero in if m > 0 then m else m + zero
  let sub a b = add a (-b)
end

let () =
  let h = -13 in
  print_endline (string_of_int (Watchtower.add Watchtower.zero h)) ;
  print_endline (string_of_int (Watchtower.add h Watchtower.zero)) ;
  let a = 1942 in
  print_endline (string_of_int (Watchtower.add h a)) ;
  print_endline (string_of_int (Watchtower.sub a h)) ;
  print_endline (string_of_int (Watchtower.sub a a)) ;