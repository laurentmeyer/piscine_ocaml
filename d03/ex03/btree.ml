type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let is_bst t =
  let rec bst_aux tr min max =
    match tr with 
    | Nil -> true
    | Node (n, _, _) when n < min || n > max -> false
    | Node (n, l, r) -> bst_aux l min (n - 1) && bst_aux r (n + 1) max
  in bst_aux t min_int max_int

let is_perfect t =
  let rec perfect tr =
    match tr with
    | Nil -> true
    | Node (n, l, r) when (l = Nil) <> (r == Nil) -> false
    | Node (n, l, r) -> perfect l && perfect r
  in perfect t && is_bst t

let rec is_balanced t =
  let rec height t =
    match t with
    | Nil -> 0
    | Node (n, l, r) -> 1 + max (height l) (height r)
  in
  match t with
  | Nil -> true
  | Node (n, l, r) when abs (height l - height r) > 1 -> false
  | Node (n, l, r) -> is_balanced l && is_balanced r && abs (height l - height r) <= 1

(* =============================================================================== *)

let rec size t =
  match t with
  | Nil -> 0
  | Node (n, l, r) -> 1 + size l + size r

let rec height t =
  match t with
  | Nil -> 0
  | Node (n, l, r) -> 1 + max (height r) (height l)

let rec main_loop ()=
  main_loop ()

let draw_square x y size = 
  let up = x + size / 2 in
  let down = x - size / 2 in
  let left = y - size / 2 in
  let right = y + size / 2 in
  Graphics.moveto down left ;
  Graphics.lineto down right ;
  Graphics.lineto up right ;
  Graphics.lineto up left ;
  Graphics.lineto down left

let connect_squares center_left center_right size =
  let (x, y) = center_left in Graphics.moveto (x + size / 2) y ;
  let (x, y) = center_right in Graphics.lineto (x - size / 2) y

let draw_node n x y size =
  draw_square x y size ; 
  Graphics.moveto x y ;
  match n with
  | Nil -> Graphics.draw_string "Nil"
  | Node (s, _, _) -> Graphics.draw_string (string_of_int s)


let rec required_vertical t sq_size margin =
  match t with
  | Nil -> sq_size + margin
  | Node (n, l, r) -> margin + required_vertical l sq_size margin + required_vertical r sq_size margin

let rec draw_tree_node_from n x y size y_margin =
  let x_offset = 100 in
  draw_node n x y size ;
  match n with
  | Nil -> ()
  | Node (i, l, r) -> begin
      let parent_center = (x, y) in
      let left_center = (x + x_offset, y - (required_vertical r size y_margin) / 2) in 
      let right_center = (x + x_offset, y + (required_vertical l size y_margin) / 2) in 
      begin
        let (x, y) = left_center in draw_tree_node_from l x y size y_margin ;
        let (x, y) = right_center in draw_tree_node_from r x y size y_margin;
        connect_squares parent_center left_center size ;
        connect_squares parent_center right_center size
      end
    end

let draw_tree n =
  let origin_x = 100 in
  let origin_y = 600 in
  draw_tree_node_from n origin_x origin_y 40 10

let () =
  Graphics.open_graph (" 800x1200") ;
  (* let t = Node (8, Node (3, Node (1, Nil, Nil), Node (6, Node (4, Nil, Nil), Node (7, Nil, Nil))), Node (10, Nil, Node (14, Node (13, Nil, Nil), Nil))) *)
  (* let t = Node (8, Node (9, Node (1, Nil, Nil), Node (6, Node (4, Nil, Nil), Node (7, Nil, Nil))), Node (10, Nil, Node (14, Node (13, Nil, Nil), Nil))) *)
  (* let t = Node (1, Node (1, Node (1, Node (1, Nil, Nil), Node (1, Nil, Nil)), Node (1, Node (1, Nil, Nil), Node (1, Nil, Nil))), Node (1, Node (1, Node (1, Nil, Nil), Node (1, Nil, Nil)), Node (1, Node (1, Nil, Nil), Node (1, Nil, Nil)))) *)
  (* let t = Node (1, Node (1, Node (1, Nil, Node (1, Nil, Nil)), Node (1, Node (1, Nil, Nil), Node (1, Nil, Nil))), Node (1, Node (1, Node (1, Nil, Nil), Node (1, Nil, Nil)), Node (1, Node (1, Nil, Nil), Node (1, Nil, Nil)))) *)
  let t = Node (1, Nil, Nil)
  in
  draw_tree t ;
  (* print_endline (string_of_bool (is_bst t)) ; *)
  print_endline (string_of_bool (is_perfect t)) ;
  main_loop ()