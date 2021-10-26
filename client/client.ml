open Sys
open Unix
open Printf
open Keys

(* [id] is the id that the client will use to identify itself to the
   server. This id will always be sent in plaintext.*)
let id = id_gen 2 ""

(* [rand_challenge] is the integer that the client will send to the
   server during the server authentication process. This is used to
   defend against replay attacks.*)
let rand_challenge = rand_int

(* [send_str str socket] writes string [str] to socket [socket]. Note:
   all strings sent to the server must start with "op=[op] [id]". If the
   string contains a random challenge (which most do), then the string
   must start with "op=[op] [id] r=[r]"*)
let send_str (str : string) socket =
  ignore (write_substring socket str 0 (String.length str))

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

(* [prompt_uname_pass ()] reads a username from stdin and then prompts
   the user for a password. It returns the list [uname;pass]*)
let prompt_uname_pass () =
  let uname = read_line () in
  let _ =
    fprintf Stdlib.stdout "%s %!"
      "\nPlease enter your password, then hit enter.\n\n"
  in
  let pass = read_line () in
  [ uname; pass ]

(* match Core.In_channel.input_line Core.In_channel.stdin with | None ->
   failwith "Aborting due to timeout." | Some uname -> ( fprintf
   Stdlib.stdout "%s %!" "\nPlease enter your password, then hit
   enter.\n"; match Core.In_channel.input_line Core.In_channel.stdin
   with | None -> failwith "Aborting due to timeout." | Some passwd -> [
   uname; passwd ]) *)

(* [handle_login socket] prompts the user for their username and
   password during login and then sends the login request to the server
   using socket [socket].*)
let handle_login socket =
  fprintf Stdlib.stdout "%s %!"
    "\nPlease enter your username, then hit enter.\n\n";
  match prompt_uname_pass () with
  | uname :: passwd :: e ->
      fprintf Stdlib.stdout "%s %!" "\nLogging in...\n";
      send_str
        ("op=login " ^ id ^ " " ^ "username=" ^ uname ^ " "
       ^ "password=" ^ passwd)
        socket;
      ()
  | _ -> failwith "Login Unsuccessful"

(* [handle_register socket] prompts the user for their username and
   password during registration and then sends the registration request
   to the server using socket [socket].*)
let handle_register socket =
  fprintf Stdlib.stdout "%s %!"
    "\nPlease enter your new username, then hit enter.\n\n";
  match prompt_uname_pass () with
  | uname :: passwd :: e ->
      fprintf Stdlib.stdout "%s %!" "\nRegistering...\n";
      send_str
        ("op=register " ^ id ^ " " ^ "username=" ^ uname ^ " "
       ^ "password=" ^ passwd)
        socket;
      fprintf Stdlib.stdout "%s %!"
        "\nRegistration Succesful! You can now login.\n";
      handle_login socket
  | _ -> failwith "Registration Unsuccessful"

(* [handle_success socket] prompts the user to either login or register
   after the server authentication process has completed, and calls
   [handle_login socket] or [handle_register socket].*)
let handle_success socket : string =
  fprintf Stdlib.stdout "%s %!" "Random Challenge Succesfull\n";
  fprintf Stdlib.stdout "%s %!"
    "\n\
     Handshake Complete! Type \"login\" and hit enter to login, or \
     type \"register\" and hit enter to register.\n\n";
  let cmd = read_line () in
  match cmd with
  | "login" ->
      handle_login socket;
      ""
  | "register" ->
      handle_register socket;
      ""
  | _ ->
      fprintf Stdlib.stdout "%s %!" "Invalid value. Please try again.\n";
      ""

(* [rand_response str socket] extracts the random number challenge
   response from the message [str] sent by the server. Calls
   [handle_success socket] if the challenge was completed. Raises
   "Random Challenge Failed" if the challenge was failed.*)
let rand_response str socket =
  if extract_r str |> int_of_string = int_of_string rand_challenge + 1
  then handle_success socket
  else failwith "Random Challenge Failed"

(* [handle_str str s] handles the server response [str] according to its
   operation, and replies using socket [s]. *)
let handle_str str s =
  let op = extract_op str in
  let _ = fprintf Stdlib.stdout "op is %s %!" op in
  match op with
  | "ok" ->
      send_str ("op=diffie " ^ id) s;
      fprintf Stdlib.stdout "%s %!" "Diffie Hellman initiating\n";
      ""
  | "diffie_complete" ->
      send_str ("op=random_challenge " ^ id ^ " r=" ^ rand_int) s;
      fprintf Stdlib.stdout "%s %!" "Diffie Hellman complete\n";
      ""
  | "random_response" -> rand_response str s
  | _ -> ""

(* [recieve_init s] listens for messages sent from the server on socket
   [s] and calls [handle_str] to process the messages.*)
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

(* [handshake s] sends the message to the server using socket [s] that
   initializes the handshake*)
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
  recieve_init socket
(* match fork () with | 0 -> (* Write to socket (send a msg) *) send
   socket; shutdown socket SHUTDOWN_SEND; exit 0 | _ -> (* Read from
   socket (read a msg)*) recieve socket; close stdout; wait () *)
;;

main ()
