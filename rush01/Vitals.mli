type t = { health : Points.t ;
           energy : Points.t ;
           hygiene : Points.t ;
           happiness : Points.t ; }
val zero : t
val all : t
val add : t -> t -> t
val sub : t -> t -> t
val to_string : t -> string