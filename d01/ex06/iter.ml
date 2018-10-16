let rec iter f x n =
  if n < 0 then (-1)
  else if n = 0 then x
  else f (iter f x (n - 1))

let () =
  print_string "iter (fun x -> x * x) 2 4: ";
  print_int (iter (fun x -> x * x) 2 4);
  print_newline ();
  print_string "iter (fun x -> x * x) 2 4: ";
  print_int (iter (fun x -> x * 2) 2 4);
  print_newline ();