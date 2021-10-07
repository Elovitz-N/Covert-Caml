(**  *)

val socket : Unix.file_descr
(**[socket] is the local socket used to connect to *)

(*val local_addr : Unix.inet_addr*)

val connect : string -> string -> unit
(**[connect s1 s2] connects the socket to the server with host name s1
   and service s2*)

val send : string -> unit
(**[send s] writes the [s] onto the the socket.*)

val recieve : unit -> string
(**[recieve ()] reads whatever has been written onto the socket.*)

val close_connection : unit -> unit
(**[close_connection ()] closes the connection for sending*)
