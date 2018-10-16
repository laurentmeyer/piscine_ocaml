let modulo a b =
  let m = a mod b in
  if m >= 0 then m else m + b

let caesar n str =
  let addone c =
    match c with
    | 'a' .. 'z' -> char_of_int (modulo (n + int_of_char c - int_of_char 'a') 26 + int_of_char 'a')
    | 'A' .. 'Z' -> char_of_int (modulo (n + int_of_char c - int_of_char 'A') 26 + int_of_char 'A')
    | _ -> c in
  String.map addone str

let rot42 str = caesar 42 str

let xor key str = String.map (fun c -> char_of_int (key lxor (int_of_char c))) str

let ft_crypt functions str =
  let rec crypt_aux f acc =
  match f with
  | [] -> acc
  | head :: tail -> crypt_aux tail (head acc) in
  let str:string = str in
  crypt_aux functions str