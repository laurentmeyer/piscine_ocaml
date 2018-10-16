let rot_lower n c =
  let ascii_for_a = int_of_char 'a' in
  let ascii_for_current = int_of_char c in
  let offset = (n + ascii_for_current - ascii_for_a) mod 26 in
  char_of_int (ascii_for_a + offset)

let rot_upper n c =
  let ascii_for_a = int_of_char 'A' in
  let ascii_for_current = int_of_char c in
  let offset = (n + ascii_for_current - ascii_for_a) mod 26 in
  char_of_int (ascii_for_a + offset)

let ft_rot_n n str = 
  let ft_rot c =
    if c >= 'a' && c <= 'z' then rot_lower n c
    else if c >= 'A' && c <= 'Z' then rot_upper n c
    else c
  in
  String.map ft_rot str

let main () =
  print_string "ft_rot_n 1 \"abcdefghijklmnopqrstuvwxyz\": ";
  print_string (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz");
  print_char '\n';
  print_string "ft_rot_n 13 \"abcdefghijklmnopqrstuvwxyz\": ";
  print_string (ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz");
  print_char '\n';
  print_string "ft_rot_n 42 \"0123456789\": ";
  print_string (ft_rot_n 42 "0123456789");
  print_char '\n';
  print_string "ft_rot_n 2 \"OI2EAS67B9\": ";
  print_string (ft_rot_n 2 "OI2EAS67B9");
  print_char '\n';
  print_string "ft_rot_n 0 \"Damned !\": ";
  print_string (ft_rot_n 0 "Damned !");
  print_char '\n';
  print_string "ft_rot_n 42 \"\": ";
  print_string (ft_rot_n 42 "");
  print_char '\n';
  print_string "ft_rot_n 1 \"NBzlk qnbjr !\": ";
  print_string (ft_rot_n 1 "NBzlk qnbjr !");
  print_char '\n'

let () = main ()