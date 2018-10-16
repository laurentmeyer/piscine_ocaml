let sdbm_hash str =
  let length = String.length str in
  let rec hash_aux h i =
    if i = length then h
    else
      let c = int_of_char (String.get str i) in
      hash_aux (c + (h lsl 6) + (h lsl 16) - h) (i + 1)
  in hash_aux 0 0

module StringHashtbl = Hashtbl.Make (
  struct
    type t = String.t
    let equal = String.equal
    let hash = sdbm_hash
  end)

let () =
  let ht = StringHashtbl.create 5 in
  let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
  let pairs = List.map (fun s -> (s, String.length s)) values in
  List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
  StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht ;
  Printf.printf "%s: %d\n" "Hello" (sdbm_hash "Hello") ;
  Printf.printf "%s: %d\n" "hello" (sdbm_hash "hello")