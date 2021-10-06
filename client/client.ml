open Unix

type user_data = {
  username : string;
  password : string;
}

let inst_data = { username = ""; password = "" }

let socket = socket PF_INET SOCK_STREAM 0

let connect host serv =
  let server_addr =
    try (gethostbyname host).h_addr_list.(0)
    with Not_found ->
      prerr_endline (host ^ ": Host not found");
      exit 2
  in
  let port_number =
    try (getservbyname serv "tcp").s_port
    with Not_found ->
      prerr_endline (serv ^ ": Port not found");
      exit 2
  in
  connect socket (ADDR_INET (server_addr, port_number))

let send s =
  let bytes_s = Bytes.of_string s in
  ignore (write socket bytes_s 0 (Bytes.length bytes_s))

let buffer_size = 4096

let recieve () =
  let rec recv_bytes acc =
    let bytes_s = Bytes.create buffer_size in
    match read socket bytes_s 0 buffer_size with
    | 0 -> ""
    | n -> recv_bytes (acc ^ Bytes.to_string bytes_s)
  in
  recv_bytes ""

let close_connection () = shutdown socket SHUTDOWN_SEND

(**[query_input ()] is the string of a user input.*)
let query_input () =
  match read_line () with exception End_of_file -> exit 0 | c -> c

let main () =
  print_endline "Connect to host:";
  let host = query_input () in
  print_endline "Connect to service:";
  let serv = query_input () in
  connect host serv;
  print_endline "Send or Recieve?";
  match query_input () with
  | "Send" ->
      send (query_input ());
      close_connection ()
  | "Recieve" ->
      print_string (recieve ());
      close_connection ()
  | _ -> exit 0
;;

main ()
