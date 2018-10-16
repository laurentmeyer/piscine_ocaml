let string_of_tardis =
"        ___
        | |
        | |
-------------------
-------------------
 |  ___  |  ___  |
 | | | | | | | | |
 | |-+-| | |-+-| |
 | |_|_| | |_|_| |
 |  ___  |  ___  |
 | |   | | |   | |
 | |   | | |   | |
 | |___| | |___| |
 |  ___  |  ___  |
 | |   | | |   | |
 | |   | | |   | |
 | |___| | |___| |
 |       |       |
==================="


class doctor name age sidekick =
  object (self)
    initializer print_endline ("Doctor " ^ name ^ " was created with age " ^ (string_of_int age) ^ " and sidekick " ^ sidekick#get_name )
    val _name : string = name
    val _age : int = age
    val _sidekick : People.people = sidekick
    val _hp : int = 100
    method to_string = "Doctor " ^ _name
                      ^ " with hp of " ^ string_of_int _hp
                      ^ ", age of " ^ string_of_int _age
                      ^ " and sidekick " ^ _sidekick#get_name
    method talk = print_endline "Hi! I’m the Doctor!"
    method travel_in_time (start : int) (arrival : int) = print_endline string_of_tardis
    method use_sonic_screwdriver = print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"
    method private regenerate = {< _hp = 100 >}
    method regenerate_exposed = self#regenerate
  end