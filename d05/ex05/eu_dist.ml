let eu_dist a b =
  let len = Array.length a in
  let acc = ref 0.0 in
  for i = 0 to len - 1 do
    let ai = a.(i) in
    let bi = b.(i) in
    acc := !acc +. (ai -. bi) *. (ai -. bi)
  done;
  sqrt !acc

let () =
  let a = [| 1.0 ; 2.0 ; 1.0 |] in
  let b = [| 1.0 ; 1.0 ; 1.0 |] in
  (* let a = [| 1.0 ; 0.0 ; 3.0 |] in
  let b = [| 1.0 ; 0.0 ; 3.0 |] in *)
  print_endline (string_of_float (eu_dist a b))