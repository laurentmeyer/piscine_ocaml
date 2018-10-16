let ft_string_all predicate str =
  let rec loop pos =
    if pos < 0 then true
    else if predicate (String.get str pos) = false then false
    else loop (pos - 1)
  in
  loop (String.length str - 1)


let main () =
  let is_digit c = c >= '0' && c <= '9' in begin
    print_string "ft_string_all is_digit \"\": ";
    print_string (string_of_bool (ft_string_all is_digit ""));
    print_char '\n';
    print_string "ft_string_all is_digit \"0123456789\": ";
    print_string (string_of_bool (ft_string_all is_digit "0123456789"));
    print_char '\n';
    print_string "ft_string_all is_digit \"O12EAS67B9\": ";
    print_string (string_of_bool (ft_string_all is_digit "O12EAS67B9"));
    print_char '\n'
  end

let () = main ()
