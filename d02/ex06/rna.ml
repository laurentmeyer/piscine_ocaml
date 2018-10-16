type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | U | None
type nucleotide = phosphate * deoxyribose * nucleobase
type helix = nucleotide list
type rna = nucleobase list

let generate_nucleotide c =
  let n = match c with
    | 'A' -> A
    | 'T' -> T
    | 'C' -> C
    | 'G' -> G
    | 'U' -> U
    | _ -> None
  in
  let nuc:nucleotide = ("phosphate", "deoxyribose", n)
  in nuc

let rec generate_helix n =
  let random_nucleotide () =
    let rand = 
      begin
        Random.self_init ();
        Random.int 4
      end
    in
    let nucleobase = match rand with
      | 0 -> 'A'
      | 1 -> 'T'
      | 2 -> 'C'
      | _ -> 'G'
    in generate_nucleotide nucleobase
  in
  let empty_helix:helix = [] in
  match n with
  | _ when n <= 0 -> empty_helix
  | _ -> random_nucleotide () :: generate_helix (n - 1)

let rec helix_to_string h =
  let string_of_nucleotide (p, d, n) =
    match n with
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | U -> "U"
    | None -> " " in
  match h with
  | [] -> ""
  | head :: tail -> string_of_nucleotide head ^ helix_to_string tail

let complementary_helix h =
  let complemetary_nucleotide (p, d, n) =
    match n with
    | A -> generate_nucleotide 'T'
    | T -> generate_nucleotide 'A'
    | C -> generate_nucleotide 'G'
    | G -> generate_nucleotide 'C'
    | _ -> generate_nucleotide ' '
  in
  let rec loop src comp =
    match src with
    | [] -> comp
    | head :: tail -> loop tail (comp @ [complemetary_nucleotide head])
  in
  let input_helix:helix = h in
  let empty_helix:helix = [] in
  loop input_helix empty_helix

let generate_rna h =
  let complemetary_nucleotide (p, d, n) =
    match n with
    | A -> U
    | T -> A
    | C -> G
    | G -> C
    | _ -> None
  in
  let rec loop src comp =
    match src with
    | [] -> comp
    | head :: tail -> loop tail (comp @ [complemetary_nucleotide head])
  in
  let input_helix:helix = h in
  let empty_rna:rna = [] in
  loop input_helix empty_rna