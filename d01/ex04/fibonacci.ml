let fibonacci n =
  let rec fib_acc n fn_1 fn_2 =
    if n < 0 then (-1)
    else if n = 0 then fn_2
    else if n = 1 then fn_1
    else fib_acc (n - 1) (fn_1 + fn_2) fn_1 in
  fib_acc n 1 0

let () =
  print_string "fibonacci (-1): ";
  print_int (fibonacci (-1));
  print_newline ();
  print_string "fibonacci 0: ";
  print_int (fibonacci 0);
  print_newline ();
  print_string "fibonacci 1: ";
  print_int (fibonacci 1);
  print_newline ();
  print_string "fibonacci 100: ";
  print_int (fibonacci 100);
  print_newline ();