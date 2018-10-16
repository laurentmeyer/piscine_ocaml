type radar = float array * string

let eu_dist a b =
  let len = Array.length a in
  let acc = ref 0.0 in
  for i = 0 to len - 1 do
    let ai = a.(i) in
    let bi = b.(i) in
    acc := !acc +. (ai -. bi) *. (ai -. bi)
  done;
  sqrt !acc

let format_example str =
  let split = String.split_on_char ',' str in
  let split_len = List.length split in
  let float_array = Array.make (split_len - 1) 0. in
    try begin
      for i = 0 to split_len - 2
      do
        float_array.(i) <- float_of_string (List.nth split i)
      done ;
      (float_array, List.nth split (split_len - 1))
    end
    with
    | Failure message -> print_endline message ; ([||], "")

let examples_of_file filepath =
  let ic = open_in filepath in
  let rec construct_list lst =
    try begin
      let file_line = input_line ic in
      let example = format_example file_line in
      construct_list (lst @ [example])
    end
    with
    | End_of_file -> close_in ic ; lst
    | Failure str -> close_in ic ; print_endline "Error" ; []
    in construct_list []

let closest_radar (lst : radar list) ((pos, status) : radar)  =
  let distances = List.map (fun (p, s) -> (eu_dist pos p, s)) lst in
  let min_radar (d1, s1) (d2, s2) = if d1 <= d2 then (d1, s1) else (d2, s2) in
  List.fold_left min_radar (List.hd distances) distances

let one_nn (lst : radar list) (r : radar)  =
  let (distance, status) = closest_radar lst r in status

let () = 
  let examples = examples_of_file "ionosphere.train.csv" in
  let radar = format_example "1,0,0.99539,-0.05889,0.85243,0.02306,0.83398,-0.37708,1,0.03760,0.85243,-0.17755,0.59755,-0.44945,0.60536,-0.38223,0.84356,-0.38542,0.58212,-0.32192,0.56971,-0.29674,0.36946,-0.47357,0.56811,-0.51171,0.41078,-0.46168,0.21266,-0.34090,0.42267,-0.54487,0.18641,-0.45300,g" in
  (* let radar = format_example "1,0,0.85271,0.05426,1,0.08069,1,1,0.91473,-0.00775,0.83721,0.03876,1,0.27153,1,1,0.81395,0.04651,0.90698,0.11628,1,0.50670,1,-1,0.80620,0.03876,1,0.71613,0.84496,0.06977,1,0.87317,1,1,b" in *)
  let status = one_nn examples radar in
  print_endline status