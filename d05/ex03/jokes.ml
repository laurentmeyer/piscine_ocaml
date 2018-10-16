let fill_jokes arr filename =
  let ic = open_in filename in
  for i = 0 to (Array.length arr) - 1 do
    begin
      try arr.(i) <- (input_line ic)
      with
      | Sys_error err -> print_endline ("Could not read " ^ filename)
    end
  done

let count_lines filename =
  let i = ref 0 in
  let continue = ref true in
  let ic = open_in filename in
  while !continue = true do
    begin
      try ignore (input_line ic) ; incr i 
      with
      | End_of_file -> continue := false ; close_in ic
      | Sys_error err -> print_endline ("Could not read " ^ filename)
    end
  done ;
  !i

let () =
  let argv = Sys.argv in
  if Array.length argv < 2 then () else
  let filename = argv.(1) in
  let length = count_lines filename in
  let jokes = Array.make length "" in
  fill_jokes jokes filename ;
  Random.self_init () ;
  let i = Random.int length in
  print_endline jokes.(i)