module type MONOID =
sig
  type element
  val zero1 : element
  val zero2 : element
  val mul : element -> element -> element
  val add : element -> element -> element
  val div : element -> element -> element
  val sub : element -> element -> element
end

module INT : (MONOID with type element = int) =
struct
  type element = int
  let zero1 = 0
  let zero2 = 1
  let add = Pervasives.(+)
  let sub = Pervasives.(-)
  let mul = Pervasives.( * )
  let div = Pervasives.(/)
end

module FLOAT : (MONOID with type element = float) =
struct
  type element = float
  let zero1 = 0.
  let zero2 = 1.
  let add = Pervasives.(+.)
  let sub = Pervasives.(-.)
  let mul = Pervasives.( *. )
  let div = Pervasives.(/.)
end

module type CALC =
  functor (M : MONOID) ->
  sig
    val add : M.element -> M.element -> M.element
    val sub : M.element -> M.element -> M.element
    val mul : M.element -> M.element -> M.element
    val div : M.element -> M.element -> M.element
    val power : M.element -> int -> M.element
    val fact : M.element -> M.element
  end

module Calc : CALC =
  functor (M : MONOID) ->
  struct
    let add = M.add
    let sub = M.sub
    let mul = M.mul
    let div = M.div
    let rec power a i =
      if i = 0 then M.zero2
      else if i < 0 then M.div M.zero2 (power a (-i))
      else M.mul a (power a (i - 1))
    let rec fact a = if a <= M.zero1 then M.zero2 else M.mul a (fact (M.sub a M.zero2))
  end


module Calc_int = Calc(INT)
module Calc_float = Calc(FLOAT)
let () =
  print_endline (string_of_int (Calc_int.div 3 2));
  print_endline (string_of_float (Calc_float.div 3. 2.));
  print_endline (string_of_int (Calc_int.sub 3 3));
  print_endline (string_of_float (Calc_float.sub 3. 3.));
  print_endline (string_of_int (Calc_int.power 3 3));
  print_endline (string_of_float (Calc_float.power 3.0 3));
  print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
  print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0)) ;
  print_endline (string_of_int (Calc_int.power 42 (-1)));
  print_endline (string_of_float (Calc_float.power 42.0 (-1)));
  print_endline (string_of_int (Calc_int.power 42 0));
  print_endline (string_of_float (Calc_float.power 42.0 0));
  print_endline (string_of_int (Calc_int.power 2 8));
  print_endline (string_of_float (Calc_float.power 2.0 8));
  print_endline (string_of_int (Calc_int.fact 5));
  print_endline (string_of_float (Calc_float.fact 5.0));