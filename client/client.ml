open Sys
open Unix
open Printf
open Keys

(* [send_and_rec fdin fdout] sends and recieves data using a socket. If
   [fdin] is stdin and [fdout] is a socket, data is read. If [fdout] is
   stdout and [fdin] is a socket, it data is written. *)

let id = id_gen

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
    | 0 -> ()
    | bytes_read ->
        let read = Bytes.sub buffer 0 bytes_read in
        let str = Bytes.to_string read in
        let _ =
          fprintf Stdlib.stdout "message from serverr: %s\n %!" str
        in
        (* TODO: this quit is not working *)
        ignore (write stdout buffer 0 bytes_read);
        loop ()
  in
  loop ()

let handle_str str s =
  match str with
  | "ok" ->
      let _ = fprintf Stdlib.stdout "Diffie time bb %!" in
      ""
  | _ -> ""

let recieve_init s =
  let buffer_size = 4096 in
  let buffer = Bytes.create buffer_size in
  let rec loop () =
    match read s buffer 0 buffer_size with
    | 0 -> loop ()
    | bytes_read -> (
        let read = Bytes.sub buffer 0 bytes_read in
        let str = Bytes.to_string read in
        let _ =
          fprintf Stdlib.stdout "message from server: %s\n %!" str
        in
        match handle_str str s with "complete" -> () | _ -> loop ())
  in
  loop ()

let send s =
  let buffer_size = 4096 in
  let buffer = Bytes.create buffer_size in
  let rec loop () =
    match read stdin buffer 0 buffer_size with
    | 0 -> ()
    | bytes_read ->
        let read = Bytes.sub buffer 0 bytes_read in
        let str = Bytes.to_string read in
        if str = "quit" then exit 0
        else ignore (write s buffer 0 bytes_read);
        loop ()
  in
  loop ()

let send_str (str : string) socket =
  ignore (write_substring socket str 0 (String.length str))

let handshake s = send_str ("op=init " ^ id) s

let main () =
  (* NOTE: the issue with these statements not printing was the buffer
     was never being flushed. To fix, I can use [fprintf "%s%!" str]
     where the %! arg ensures that the buffer will flush. For debugging
     within the threads, I should create and clear a log file at the
     beginning of main and then have the threads write the debug
     statements to the log file. *)
  fprintf Stdlib.stdout "%s %!" "Connecting to Server...\n";
  (* TODO: check more aggressively to make sure host is a valid IP
     addr *)
  (if Array.length Sys.argv < 2 then
   let _ =
     fprintf Stdlib.stdout "%s %!"
       "\n\
        Missing Host IP address. To run, use ./client/client.exe <host>\n"
   in
   exit 2);
  (if Array.length Sys.argv > 2 then
   let _ =
     fprintf Stdlib.stdout "%s %!"
       "\nToo many arguments. To run, use ./client/client.exe <host> \n"
   in
   exit 2);

  let ip = Sys.argv.(1) in
  let port = 8886 in
  let socket = socket PF_INET SOCK_STREAM 0 in
  (* TODO: figure out why all these print statements never print! *)
  connect socket (ADDR_INET (inet_addr_of_string ip, port));
  handshake socket;
  send_str id socket;
  recieve_init socket;
  match fork () with
  | 0 ->
      (* Write to socket (send a msg) *)
      send socket;
      shutdown socket SHUTDOWN_SEND;
      exit 0
  | _ ->
      (* Read from socket (read a msg)*)
      recieve socket;
      close stdout;
      wait ()
;;

main ()
