(* let rec repeat_string ?str:(s="x") n =
    if n < 0 then "Error"
    else if n = 0 then ""
    else (repeat_string ~str:s (n - 1)) ^ s *)
let rec repeat_string ?(str="x") n =
    if n < 0 then "Error"
    else if n = 0 then ""
    else (repeat_string ~str (n - 1)) ^ str

let () =
  print_string ("repeat_string (-1): ");
  print_endline (repeat_string (-1));
  print_string ("repeat_string ~str:\"Toto\" 1: ");
  print_endline (repeat_string ~str:"Toto" 1);
  print_string ("repeat_string 2: ");
  print_endline (repeat_string 2);
  print_string ("repeat_string ~str:\"a\" 5: ");
  print_endline (repeat_string ~str:"a" 5);
  print_string ("repeat_string ~str:\"what\" 3: ");
  print_endline (repeat_string ~str:"what" 3)