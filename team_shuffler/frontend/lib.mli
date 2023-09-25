val read : string -> (string * (string * (string * string) array) array) array

val generate_buckets :
  int ->
  (string * (string * (string * string) array) array) array ->
  ((string * string) * string * string) list array

val pp_buckets :
  Format.formatter -> ((string * string) * string * string) list array -> unit
