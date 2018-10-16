type sign = O | X | Undefined
type tree = Leaf of sign | Node of tree list
type position = int * int

let newTree size depth =
  let rec builder d =
    match d with
    | _ when d <= 0 -> invalid_arg "depth must be positive"
    | 1 -> Node (List.init (size * size) (fun _ -> (Leaf Undefined)))
    | _ -> Node (List.init (size * size) (fun i -> builder (d - 1)))
  in
  builder depth

let rec depth (t : tree) : int =
  match t with
  | Leaf s -> 0
  | Node lst -> 1 + List.fold_left max 0 (List.map depth lst)

let printSign (s : sign) =
  match s with
  | X -> print_string "X"
  | O -> print_string "O"
  | Undefined -> print_string "-"

let rec getSign t row col size depth : sign =
  match t with
  | Leaf s -> s
  | Node lst ->
    let sub_x = row / Utils.int_power size (depth - 1) in
    let sub_y = col / Utils.int_power size (depth - 1) in
    let n = (sub_x * size + sub_y) in
    let subtree = List.nth lst n in
    let offset = Utils.int_power size (depth - 1) in
    getSign subtree (row mod offset) (col mod offset) size (depth - 1)

let printRowSeparator row max size depth =
  if row mod size = size - 1 && row <> max - 1 then begin
    let nGrids = Utils.int_power size (depth - 1) in
    let nSigns = nGrids * size in
    let nSeparators = nGrids - 1 in
    let n = nSigns + nSeparators in
    print_endline (String.make (2 * n - 1) '-')
  end

let rec printVertSep col size depth =
  if depth = 0 then ()
  else begin 
    if col mod (Utils.int_power size depth) = 0 && col <> 0 then print_string "|";
    printVertSep col size (depth - 1) 
  end

let rec buildHorSep size depth =
  let line_length = Utils.int_power size depth in
  let char_signs = 2 * (line_length - 1) in
  let rec separators chars acc =
    if chars = 0 then acc
    else if chars = line_length then separators (chars / size) (acc + 2 * (chars / size))
    else separators (chars / size) (acc + chars / size - 1) in
  let total_length = separators line_length 0 + char_signs in
  String.make total_length '-' ^ "\n"


let rec printHorSep col row size depth =
  let line_length = Utils.int_power size depth in
  if col mod line_length = line_length - 1 then begin 
    print_string "\n";
    let rec aux d =
      if d = 0 then ()
      else begin 
        if (row + 1) mod (Utils.int_power size d) = 0 && row <> line_length - 1
        then print_string (buildHorSep size depth);
        aux (d - 1);
      end
    in aux depth 
  end

let printGrid (t : tree) size depth : unit =
  print_newline ();
  let total_chars = Utils.range 0 (Utils.int_power size (depth * 2)) in
  let line_length = Utils.int_power size depth in
  let my_print_fun i =
    let row = i / line_length in
    let col = i mod line_length in
    printVertSep col size depth;
    if col <> 0 && col mod size = 0 then print_string " ";
    printSign (getSign t row col size depth) ;
    if col <> line_length - 1 then print_string " ";
    printHorSep col row size depth ;
    ()
  in
  List.iter my_print_fun total_chars;
  print_newline ()

let getSubtreeIndex t row col size depth =
  let sub_x = row / Utils.int_power size (depth - 1) in
  let sub_y = col / Utils.int_power size (depth - 1) in
  (sub_x * size + sub_y)

let getPositionInSubtree row col size depth =
  let offset = Utils.int_power size (depth - 1) in
  let r = row mod offset in
  let c = col mod offset in
  (r, c)

let rec setSign t row col size depth sign =
  match t with
  | Node lst ->
    let n = getSubtreeIndex t row col size depth in
    let (x, y) = getPositionInSubtree row col size depth in
    Node (List.mapi
            (fun i subtree -> if i <> n then subtree else setSign subtree x y size (depth - 1) sign)
            lst)
  | Leaf _ -> Leaf sign

let getColums lst size =
  let cols = Utils.range 0 size in
  let getColn n =
    Utils.filteri (fun i x -> i mod size = n) lst
  in
  List.map getColn cols

let getRows lst size =
  let rows = Utils.range 0 size in
  let getRown n =
    Utils.filteri (fun i x -> i / size = n) lst
  in
  List.map getRown rows

let getDiagonalUp lst size =
  Utils.filteri (fun i x -> i / size = i mod size) lst

let getDiagonalDown lst size =
  Utils.filteri (fun i x -> (i / size) = (size - 1 - i mod size)) lst

let isFull lst =
  let won = List.filter (fun x -> x = Leaf X || x = Leaf O) lst in
  List.length lst = List.length won

let rec getWon lastSign size t =
  match t with
  | Leaf s -> s
  | Node lst ->
    let cols = getColums lst size in
    let rows = getRows lst size in
    let diagonalUp = getDiagonalUp lst size in
    let diagonalDown = getDiagonalDown lst size in
    let all = [diagonalDown; diagonalUp] @ cols @ rows in
    if Utils.any (isAllOfSign lastSign O size) all then O
    else if Utils.any (isAllOfSign lastSign X size) all then X
    else if isFull lst then lastSign
    else Undefined

and isAllOfSign lastSign sign size lst =
  List.for_all (fun elt -> getWon lastSign size elt = sign) lst

let rec updateWon lastSign size t = 
  let won = getWon lastSign size t in
  if won <> Undefined then Leaf won
  else setWon lastSign size t

and setWon lastSign size t = match t with
  | Node lst ->
    Node (List.map (updateWon lastSign size) lst)
  | Leaf s -> Leaf s

