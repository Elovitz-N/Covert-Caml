(**  *)

type user_data
(***)

val inst_data : user_data
(**[inst_data] is the personal client data like username and password*)

val socket : Unix.file_descr
(**[socket] is the local socket used to connect to *)

val connect : string -> string -> unit
(**[connect s] connects the socket to the server with name s*)

val send : string -> unit
(**[send s] writes the [s] onto the the socket.*)

val recieve : unit -> string
(**[recieve ()] reads whatever has been written onto the socket.*)