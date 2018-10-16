let my_sleep () = Unix.sleep 1

let sleep time =
for i = 0 to time - 1 do my_sleep () done

let () =
  let argv = Sys.argv in
  if Array.length argv < 2 then () else
    try sleep (int_of_string argv.(1)) with
    | Failure message -> ()
    | _ -> ()