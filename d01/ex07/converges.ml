let rec converges f x n =
  if n < 0 then false else
    let next = f x in
    if next = x then true
    else converges f next (n - 1)

let () =
  print_string("converges(( * ) 2) 2 5): ");
  print_endline(string_of_bool (converges(( * ) 2) 2 5));
  print_string("converges(fun x -> x / 2) 2 3): ");
  print_endline(string_of_bool (converges(fun x -> x / 2) 2 3));
  print_string("converges(fun x -> x / 2) 2 2): ");
  print_endline(string_of_bool (converges(fun x -> x / 2) 2 2))