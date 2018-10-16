type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | U | None
type nucleotide = phosphate * deoxyribose * nucleobase
type helix = nucleotide list
type rna = nucleobase list
type aminoacid =  Stop | Ala | Arg | Asn | Asp | Cys | Gln | Glu
                  | Gly | His | Ile | Leu | Lys | Met | Phe | Pro
                  | Ser | Thr | Trp | Tyr | Val
type protein = aminoacid list

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

let generate_bases_triplets r =
  let r:rna = r in
  let rec generate_aux initial triplets =
    match initial with
    | one :: two :: three :: tail -> generate_aux tail (triplets @ [(one, two, three)])
    | _ -> triplets
  in
  generate_aux r []

let string_of_aminoacid a =
  match a with
  | Ala -> "Alanine"
  | Arg -> "Arginine"
  | Asn -> "Asparagine"
  | Asp -> "Aspartique"
  | Cys -> "Cysteine"
  | Gln -> "Glutamine"
  | Glu -> "Glutamique"
  | Gly -> "Glycine"
  | His -> "Histidine"
  | Ile -> "Isoleucine"
  | Leu -> "Leucine"
  | Lys -> "Lysine"
  | Met -> "Methionine"
  | Phe -> "Phenylalanine"
  | Pro -> "Proline"
  | Ser -> "Serine"
  | Thr -> "Threonine"
  | Trp -> "Tryptophane"
  | Tyr -> "Tyrosine"
  | Val -> "Valine"
  | Stop -> ""

let decode_arn r =
  let rec decode_arn_aux triplets acc =
    match triplets with
    | (U, A, A) :: tail | (U, A, G) :: tail | (U, G, A) :: tail ->  acc
    | (G, C, A) :: tail | (G, C, C) :: tail | (G, C, G) :: tail | (G, C, U) :: tail ->  decode_arn_aux tail (Ala :: acc)
    | (A, G, A) :: tail | (A, G, G) :: tail | (C, G, A) :: tail | (C, G, C) :: tail | (C, G, G) :: tail | (C, G, U) :: tail ->  decode_arn_aux tail (Arg :: acc)
    | (A, A, C) :: tail | (A, A, U) :: tail ->  decode_arn_aux tail (Asn :: acc)
    | (G, A, C) :: tail | (G, A, U) :: tail ->  decode_arn_aux tail (Asp :: acc)
    | (U, G, C) :: tail | (U, G, U) :: tail ->  decode_arn_aux tail (Cys :: acc)
    | (C, A, A) :: tail | (C, A, G) :: tail ->  decode_arn_aux tail (Gln :: acc)
    | (G, A, A) :: tail | (G, A, G) :: tail ->  decode_arn_aux tail (Glu :: acc)
    | (G, G, A) :: tail | (G, G, C) :: tail |  (G, G, G) :: tail | (G, G, U) :: tail ->  decode_arn_aux tail (Gly :: acc)
    | (C, A, C) :: tail | (C, A, U) :: tail ->  decode_arn_aux tail (His :: acc)
    | (A, U, A) :: tail | (A, U, C) :: tail | (A, U, U) :: tail ->  decode_arn_aux tail (Ile :: acc)
    | (C, U, A) :: tail | (C, U, C) :: tail | (C, U, G) :: tail | (C, U, U) :: tail | (U, U, A) :: tail | (U, U, G) :: tail ->  decode_arn_aux tail (Leu :: acc)
    | (A, A, A) :: tail | (A, A, G) :: tail ->  decode_arn_aux tail (Lys :: acc)
    | (A, U, G) :: tail -> decode_arn_aux tail (Met :: acc)
    | (U, U, C) :: tail | (U, U, U) :: tail ->  decode_arn_aux tail (Phe :: acc)
    | (C, C, C) :: tail | (C, C, A):: tail |  (C, C, G) :: tail | (C, C, U) :: tail ->  decode_arn_aux tail (Pro :: acc)
    | (U, C, A) :: tail | (U, C, C) :: tail | (U, C, G) :: tail | (U, C, U) :: tail | (A, G, U) :: tail | (A, G, C) :: tail ->  decode_arn_aux tail (Ser :: acc)
    | (A, C, A) :: tail | (A, C, C) :: tail | (A, C, G) :: tail | (A, C, U) :: tail ->  decode_arn_aux tail (Thr :: acc)
    | (U, G, G) :: tail ->  decode_arn_aux tail (Trp :: acc)
    | (U, A, C) :: tail | (U, A, U) :: tail ->  decode_arn_aux tail (Tyr :: acc)
    | (G, U, A) :: tail | (G, U, C) :: tail |  (G, U, G) :: tail |  (G, U, U) :: tail ->  decode_arn_aux tail (Val :: acc)
    | head :: tail -> decode_arn_aux tail acc
    | [] -> acc
  in
  let empty_protein:protein = [] in
  let r:rna = r in
  let t = generate_bases_triplets r in
  decode_arn_aux t empty_protein

let rec string_of_protein p =
  let p:protein = p in
  match p with
  | head :: tail when head <> Stop -> string_of_aminoacid head ^ " " ^ string_of_protein tail
  | head :: tail -> string_of_aminoacid head
  | _ -> ""


(*
let helix = generate_helix 2000 ;;
let rn = generate_rna helix ;;
decode_arn rn;;
*)