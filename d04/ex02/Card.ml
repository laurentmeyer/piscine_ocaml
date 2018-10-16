module Color =
struct
  type t = Spade | Heart | Diamond | Club

  let all = [Spade ; Heart ; Diamond ; Club]

  let toString t =
    match t with
    | Spade     -> "S"
    | Heart     -> "H"
    | Diamond   -> "D"
    | Club      -> "C"

  let toStringVerbose t =
    match t with
    | Spade     -> "Spade"
    | Heart     -> "Heart"
    | Diamond   -> "Diamond"
    | Club      -> "Club"
end

module Value =
struct
  type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

  let all = [T2 ; T3 ; T4 ; T5 ; T6 ; T7 ; T8 ; T9 ; T10 ; Jack ; Queen ; King ; As]

  let toInt t =
    match t with
    | T2 -> 1
    | T3 -> 2
    | T4 -> 3
    | T5 -> 4
    | T6 -> 5
    | T7 -> 6
    | T8 -> 7
    | T9 -> 8
    | T10 -> 9
    | Jack -> 10
    | Queen -> 11
    | King -> 12
    | As -> 13

  let toString t =
    match t with
    | T2 -> "2"
    | T3 -> "3"
    | T4 -> "4"
    | T5 -> "5"
    | T6 -> "6"
    | T7 -> "7"
    | T8 -> "8"
    | T9 -> "9"
    | T10 -> "10"
    | Jack -> "J"
    | Queen -> "Q"
    | King -> "K"
    | As -> "A"

  let toStringVerbose t =
    match t with
    | Jack -> "Jack"
    | Queen -> "Queen"
    | King -> "King"
    | As -> "As"
    | _ -> toString t

  let next t =
    let rec next_aux l =
      match l with
      | _ when t = As -> invalid_arg "As does not have next"
      | head :: next :: tail when head = t -> next
      | head :: tail -> next_aux tail
      | [] -> invalid_arg "Value does not have next" in
    next_aux all

  let previous t =
    let rec prev_aux l =
      match l with
      | _ when t = T2 -> invalid_arg "2 does not have previous"
      | head :: next :: tail when next = t -> head
      | head :: tail -> prev_aux tail
      | [] -> invalid_arg "Value does not have previous" in
    prev_aux all
end

type t = Value.t * Color.t
let newCard v c = (v, c)
let allSpades = List.map (fun v -> newCard v Color.Spade) Value.all
let allHearts = List.map (fun v -> newCard v Color.Heart) Value.all
let allDiamonds = List.map (fun v -> newCard v Color.Diamond) Value.all
let allClubs = List.map (fun v -> newCard v Color.Club) Value.all
let all = allClubs @ allDiamonds @ allHearts @ allSpades
let getValue t = let (v, c) = t in v
let getColor t = let (v, c) = t in c
let toString t = Value.toString (getValue t) ^ Color.toString (getColor t)
let toStringVerbose t = "Card (" ^ Value.toStringVerbose (getValue t) ^ ", " ^ Color.toStringVerbose (getColor t) ^ ")"
let compare a b = Value.toInt (getValue a) - Value.toInt (getValue b)
let max a b = if compare a b >= 0 then a else b
let min a b = if compare a b < 0 then a else b
let best l =
  match l with
  | [] -> invalid_arg "Invalid list"
  | head :: tail -> List.fold_left max head tail
let isOf t c = getColor t = c
let isSpade = fun t -> isOf t Color.Spade
let isHeart = fun t -> isOf t Color.Heart
let isDiamond = fun t -> isOf t Color.Diamond
let isClub = fun t -> isOf t Color.Club