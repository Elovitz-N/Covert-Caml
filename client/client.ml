open Sys
open Unix

let send_and_rec fdin fdout =
  let buffer_size = 4096 in
  let buffer = Bytes.create buffer_size in
  let rec loop () =
    match read fdin buffer 0 buffer_size with
    | 0 -> ()
    | x ->
        ignore (write fdout buffer 0 x);
        loop ()
  in
  loop ()

let main () =
  (if Array.length Sys.argv < 3 then
   let _ =
     print_string
       "Missing Host or Port. To run, use ./client.exe <host> <port>"
   in
   exit 2);
  let ip = Sys.argv.(1) in
  let port = int_of_string Sys.argv.(2) in
  let socket = socket PF_INET SOCK_STREAM 0 in
  connect socket (ADDR_INET (inet_addr_of_string ip, port));
  match fork () with
  | 0 ->
      send_and_rec stdin socket;
      shutdown socket SHUTDOWN_SEND;
      exit 0
  | _ ->
      send_and_rec socket stdout;
      close stdout;
      wait ()
;;

handle_unix_error main ()
