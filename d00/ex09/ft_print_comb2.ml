let print_int_2 i =
  if i < 10
  then
    begin 
      print_char '0';
      print_int i
    end
  else
    print_int i

let ft_print_comb2 () =
  let rec loop a b =
    print_int_2 a;
    print_char ' ';
    print_int_2 b;
    if a <= 97
    then 
      begin
        print_char ',';
        print_char ' ';
        if b > 98 then
          loop (a + 1) (a + 2)
        else
          loop a (b + 1)
      end
  in
  loop 0 1;
  print_char '\n'

let () =
  ft_print_comb2 ()