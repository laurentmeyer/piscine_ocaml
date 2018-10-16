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

let () =
  let examples = examples_of_file "ionosphere.test.csv" in
  let print_example (arr, str) =
    Array.iter (fun f -> Printf.printf "%.2f | " f) arr ;
    print_endline str
  in
  List.iter print_example examples