type t

val from_json : Yojson.Basic.t -> t

val first_user: t -> string