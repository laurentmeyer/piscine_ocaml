let uncaesar n str = Cipher.caesar (-n) str

let unrot42 str = (uncaesar 42) str

let ft_uncrypt functions str =
  let rec uncrypt_aux f acc =
  match f with
  | [] -> acc
  | head :: tail -> head (uncrypt_aux tail acc)
  in
  let str:string = str in
  uncrypt_aux functions str

let () =
  let str = "abyzABYZ" in
  print_endline (Cipher.rot42 str) ;
  print_endline (Cipher.caesar 42 str) ;
  print_endline (uncaesar 42 (Cipher.caesar 42 str)) ;
  print_endline (Cipher.rot42 (unrot42 str)) ;
  print_endline (unrot42 (Cipher.rot42 str)) ;
  print_endline (Cipher.xor 42 str) ;
  print_endline (Cipher.xor 42 (Cipher.xor 42 str)) ;
  let cipher_list = [Cipher.xor 42 ; Cipher.caesar 12 ; Cipher.xor 12; Cipher.rot42] in
  let decipher_list = [Cipher.xor 42 ; uncaesar 12 ; Cipher.xor 12 ; unrot42] in
  let enciphered = Cipher.ft_crypt cipher_list str in
  let deciphered = ft_uncrypt decipher_list enciphered in
  print_endline enciphered ;
  print_endline deciphered ;