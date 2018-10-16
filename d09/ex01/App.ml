module type APP =
sig
  type project = string * string * int
  val zero : project
  val combine : project -> project -> project
  val fail : project -> project
  val success : project -> project
end

module App : APP =
struct
  type status = string
  type grade = int
  type project = string * status * grade
  let zero = ("", "", 0)
  let combine (f1, s1, g1) (f2, s2, g2) =
    let grade = (g1 + g2) / 2 in
    let status = if grade >= 80 then "succes" else "fail" in
    (f1 ^ f2, status, grade)
  let fail (s, _, _) = (s, "fail", 0)
  let success (s, _, _) = (s, "success", 80)
end

let () =
  let print_proj ((name, status, grade) : App.project) =
    print_endline ("Project {" ^ name ^
                   "} has status {" ^ status ^
                   "} with grade {" ^ string_of_int grade ^ "}") in
  let a = ("Coucou", "success", 95) in
  let b = ("Bouzin", "fail", 42) in
  print_proj a ;
  print_proj (App.combine App.zero a) ;
  print_proj (App.combine a App.zero) ;
  print_proj (App.fail a) ;
  print_proj (App.success b) ;
  print_proj (App.combine a b) ;