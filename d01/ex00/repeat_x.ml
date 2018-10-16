let rec repeat_x n =
    if n < 0 then "Error"
    else if n = 0 then ""
    else (repeat_x (n - 1)) ^ "x"

let () =
  print_string ("repeat_x (-1): ");
  print_endline (repeat_x (-1));
  print_string ("repeat_x (0): ");
  print_endline (repeat_x 0);
  print_string ("repeat_x 42: ");
  print_endline (repeat_x 42)