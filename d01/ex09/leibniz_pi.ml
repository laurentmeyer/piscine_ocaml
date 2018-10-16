let leibniz_pi min_delta =
  if min_delta < 0. then -1
  else
    let rec leibniz_aux i sign acc =
      let d = 4. *. (atan 1.) -. acc in
      if d >= 0. && d < min_delta then i
      else if d <= 0. && (-. d) < min_delta then i
      else
        let sigma = 4. *. float_of_int(sign) /. float_of_int (2 * i + 1) in
        leibniz_aux (i + 1) (-sign) (acc +. sigma)
    in
    leibniz_aux 0 1 0.

let () =
  print_endline (string_of_int (leibniz_pi 0.0000000123))