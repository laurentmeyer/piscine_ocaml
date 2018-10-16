let encode l =
  let rec encode_aux origin count acc =
    match origin with
    | head :: next :: tail when head = next -> encode_aux (next :: tail) (count + 1) acc
    | head :: next :: tail -> encode_aux (next :: tail) 1 (acc @ [count; head])
    | head :: [] -> acc @ [count; head]
    | [] -> []
  in
  encode_aux l 1 []

let rec make_sequence n =
  match n with
  | _ when n <= 0 -> []
  | 1 -> [1]
  | _ -> encode (make_sequence (n - 1))

let sequence n =
  if n <= 0 then ""
  else
    let rec concat_sequence s =
      match s with
      | [] -> ""
      | head :: tail -> (string_of_int head) ^ (concat_sequence tail)
    in concat_sequence (make_sequence n)


let () =
  let rec print_aux cur max =
    if cur <= max then begin print_endline(sequence cur) ; print_aux (cur + 1) max end
  in
  print_aux (-1) 15