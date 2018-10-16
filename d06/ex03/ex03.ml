module type FIXED =
sig
  type t
  val zero : t
  val one : t
  val of_int : int -> t
  val of_float : float -> t
  val to_float : t -> float
  val to_int : t -> int
  val to_string : t -> string
  val succ : t -> t
  val pred : t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val add : t -> t -> t
  val gth : t -> t -> bool
  val lth : t -> t -> bool
  val gte : t -> t -> bool
  val lte : t -> t -> bool
  val eqp : t -> t -> bool (** physical equality *)
  val eqs : t -> t -> bool (** structural equality *)
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val foreach : t -> t -> (t -> unit) -> unit
end


module type FIXABLE =
sig
  val bits : int
end

module type MAKEFIXED =
  functor (Fixable : FIXABLE) -> FIXED

module Make : MAKEFIXED =
  functor (F : FIXABLE) ->
  struct
    type t = int
    let zero = 0
    let one = 1 lsl F.bits
    let of_int i = i lsl F.bits
    let of_float f = int_of_float (floor (0.5 +. f *. (float_of_int one)))
    let to_float t = (float_of_int t) /. (float_of_int one)
    let to_int t = t lsr F.bits
    let to_string t = string_of_float (to_float t)
    let succ = Pervasives.(+) 1
    let pred = (fun a -> Pervasives.(-) a 1)
    let min = Pervasives.min
    let max = Pervasives.max
    let add = Pervasives.(+)
    let gth = Pervasives.(>)
    let lth = Pervasives.(<)
    let gth = Pervasives.(>)
    let gte = Pervasives.(>=)
    let lte = Pervasives.(<=)
    let eqp = Pervasives.(=)
    let eqs = Pervasives.(==)
    let sub = Pervasives.(-)
    let mul a b = (a * b) / one
    let div a b = of_int a / b
    let rec foreach a b f = if a > b then () else (f a ; foreach (a + 1) b f)
  end




module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)
let () =
  let x8 = Fixed8.of_float 21.10 in
  let y8 = Fixed8.of_float 21.32 in
  print_string "of_int: " ;
  print_endline (Fixed8.to_string (Fixed8.of_int (int_of_float 21.10)));
  print_string "of_float: " ;
  print_endline (Fixed8.to_string x8);
  print_string "to_int: " ;
  print_endline (string_of_int (Fixed8.to_int x8));
  print_string "to_float: " ;
  print_endline (string_of_float (Fixed8.to_float x8));
  print_string "min: " ;
  print_endline (Fixed8.to_string (Fixed8.min x8 y8));
  print_string "max: " ;
  print_endline (Fixed8.to_string (Fixed8.max x8 y8));
  print_string "pred: " ;
  print_endline (Fixed8.to_string (Fixed8.pred x8));
  print_string "succ: " ;
  print_endline (Fixed8.to_string (Fixed8.succ x8));
  print_string "gth: " ;
  Printf.printf "%B\n" (Fixed8.gth x8 y8);
  print_string "gte: " ;
  Printf.printf "%B\n" (Fixed8.gte x8 x8);
  print_string "lth: " ;
  Printf.printf "%B\n" (Fixed8.lth x8 x8);
  print_string "lte: " ;
  Printf.printf "%B\n" (Fixed8.lte x8 x8);
  print_string "add: " ;
  print_endline (Fixed8.to_string (Fixed8.add x8 y8));
  print_string "sub: " ;
  print_endline (Fixed8.to_string (Fixed8.sub (Fixed8.add x8 y8) y8));
  print_string "mul: " ;
  print_endline (Fixed8.to_string (Fixed8.mul x8 y8));
  print_string "div: " ;
  print_endline (Fixed8.to_string (Fixed8.div (Fixed8.mul x8 y8) y8));
  let x8bis = x8 in
  print_string "eqs: " ;
  Printf.printf "%B %B %B\n" (Fixed8.eqs x8 y8) (Fixed8.eqs x8 x8bis) (Fixed8.eqs x8 x8);
  print_string "eqp: " ;
  Printf.printf "%B %B %B\n" (Fixed8.eqp x8 y8) (Fixed8.eqp x8 x8bis) (Fixed8.eqp x8 x8);
  Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f))