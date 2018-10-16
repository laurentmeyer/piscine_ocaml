type 'a ft_ref = {mutable contents : 'a}

let return a =
  {contents =  a}

let get r =
  r.contents

let set r a =
  r.contents <- a

let bind r f =
  return !(f (get r))

let () =
  let my_ref = return 42 in
  print_endline (string_of_int (get my_ref)) ;
  set my_ref 12 ;
  print_endline (string_of_int (get my_ref)) ;
  let my_bind = bind my_ref (fun x -> ref (130 + x)) in
  print_endline (string_of_int (get my_bind))