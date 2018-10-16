let ft_is_palindrome str =
  let rec loop start len =
    if len <= 1 then true
    else if (String.get str start) <> (String.get str (start + len)) then false
    else loop (start + 1) (len - 2)
  in
  loop 0 (String.length str - 1)


let main () =
  print_string "ft_is_palindrome \"radar\": ";
  print_string (string_of_bool (ft_is_palindrome "radar"));
  print_char '\n';
  print_string "ft_is_palindrome \"madam\": ";
  print_string (string_of_bool (ft_is_palindrome "madam"));
  print_char '\n';
  print_string "ft_is_palindrome \"car\": ";
  print_string (string_of_bool (ft_is_palindrome "car"));
  print_char '\n';
  print_string "ft_is_palindrome \"\": ";
  print_string (string_of_bool (ft_is_palindrome ""));
  print_char '\n'

  let () = main ()