module type SET =
sig
  type 'a t = 'a list
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val union : 'a t -> 'a t -> 'a t
  val inter : 'a t -> 'a t -> 'a t
  val diff : 'a t -> 'a t -> 'a t
  val filter : 'a t -> ('a -> bool) -> 'a t
  val foreach : 'a t -> ('a -> unit) -> unit
  val for_all : 'a t -> ('a -> bool) -> bool
  val exists : 'a t -> ('a -> bool) -> bool
end

module Set : SET =
struct
  type 'a t = 'a list
  let remove_duplicates lst =
    let rec aux l acc = match l with
      | [] -> acc
      | head :: tail when not (List.mem head acc) -> aux tail (acc @ [head])
      | head :: tail -> aux tail acc
    in aux lst []
  let return a = [a]
  let bind a f = remove_duplicates (List.flatten (List.map f a))
  let union a b = remove_duplicates (a @ b)
  let inter a b = List.filter (fun e -> List.mem e b) a
  let diff a b = List.filter (fun e -> not (List.mem e b)) a
  let filter a f = List.filter f a
  let foreach a f = List.iter f a
  let for_all a f = List.for_all f a
  let exists a f = List.exists f a
end

let () =
  let print_int_set s = Set.foreach s (fun i -> print_int i ; print_char ' ') ; print_newline () in
  let a = Set.return 1 in
  let b = Set.return 2 in
  print_int_set (Set.union a b) ;
  let bind_fun = fun e -> [e ; e * 2] in
  let binded_a = Set.bind a bind_fun in
  let binded_b = Set.bind (Set.bind binded_a bind_fun) bind_fun in
  print_int_set binded_a ;
  print_int_set binded_b ;
  print_int_set (Set.diff binded_b binded_a) ;
  print_int_set (Set.inter binded_b binded_a) ;
  print_int_set (Set.filter binded_a (fun i -> i mod 2 = 1)) ;
  print_endline (string_of_bool (Set.for_all (Set.diff binded_b binded_a) (fun i -> i mod 2 = 0))) ;
  print_endline (string_of_bool (Set.for_all binded_a (fun i -> i mod 3 = 0))) ;
