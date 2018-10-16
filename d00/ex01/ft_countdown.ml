let rec ft_countdown a =
  if a > 0
  then begin print_int a ; print_char '\n' ; ft_countdown (a - 1) end
  else begin print_int 0 ; print_char '\n' end


let () =
  print_endline "===ft_countdown 10 ===";
  ft_countdown 10;
  print_endline "===ft_countdown 0  ===";
  ft_countdown 0;
  print_endline "===ft_countdown -3 ===";
  ft_countdown (-3)