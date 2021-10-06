open Unix

type user_data = {
  username : string;
  password : string;
}

let inst_data = { username = ""; password = "" }

let socket = socket PF_INET SOCK_STREAM 0

let connect name =
  let server_addr =
    try (gethostbyname name).h_addr_list.(0)
    with Not_found ->
      prerr_endline (name ^ ": Host not found");
      exit 2
  in
  let port_number =
    try (getservbyname name "tcp").s_port
    with Not_found ->
      prerr_endline (name ^ ": Port not found");
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
