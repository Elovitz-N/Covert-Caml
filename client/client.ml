open Sys
open Unix

(* [send_and_rec fdin fdout] sends and recieves data using a socket. If
   [fdin] is stdin and [fdout] is a socket, data is read. If [fdout] is
   stdout and [fdin] is a socket, it data is written. *)
let send_and_rec fdin fdout =
  let buffer_size = 4096 in
  let buffer = Bytes.create buffer_size in
  let rec loop () =
    match read fdin buffer 0 buffer_size with
    | 0 -> ()
    | bytes_read ->
        let read = Bytes.sub buffer 0 bytes_read in
        let str = Bytes.to_string read in
        if str = "quit" then exit 0
        else ignore (write fdout buffer 0 bytes_read);
        loop ()
  in
  loop ()

let recieve s =
  let buffer_size = 4096 in
  let buffer = Bytes.create buffer_size in
  let rec loop () =
    match read s buffer 0 buffer_size with
    | 0 -> print_string "here2"
    | bytes_read ->
        let read = Bytes.sub buffer 0 bytes_read in
        let str = Bytes.to_string read in
        if str = "quit" then exit 0
        else ignore (write stdout buffer 0 bytes_read);
        loop ()
  in
  loop ()

let send s =
  let _ = print_string "starting" in
  let buffer_size = 4096 in
  let buffer = Bytes.create buffer_size in
  let rec loop () =
    match read stdin buffer 0 buffer_size with
    | 0 -> print_string "here2"
    | bytes_read ->
        let read = Bytes.sub buffer 0 bytes_read in
        let str = Bytes.to_string read in
        if str = "quit" then exit 0
        else ignore (write s buffer 0 bytes_read);
        loop ()
  in
  loop ()

let main () =
  print_string "Connecting to Server...";

  (if Array.length Sys.argv < 3 then
   let _ =
     print_string
       "Missing Host or Port. To run, use ./client.exe <host> <port>"
   in
   exit 2);
  print_string "what2";

  let ip = Sys.argv.(1) in
  let port = int_of_string Sys.argv.(2) in
  let socket = socket PF_INET SOCK_STREAM 0 in
  connect socket (ADDR_INET (inet_addr_of_string ip, port));
  print_string "what";
  match fork () with
  | 0 ->
      (* Write to socket (send a msg) *)
      print_string "0";
      send socket;
      shutdown socket SHUTDOWN_SEND;
      exit 0
  | _ ->
      print_string "1";
      (* Read from socket (read a msg)*)
      recieve socket;
      close stdout;
      wait ()
;;

main ()
