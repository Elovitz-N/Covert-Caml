open Sys
open Unix
open Printf
open Util.Msg
open Util.Keys
open Util.Db

(* [uid] is the id that the client will use to identify itself to the
   server. This id will always be sent in plaintext.*)
let uid = id_gen 2 ""

(* [rand_challenge] is the integer that the client will send to the
   server during the server authentication process. This is used to
   defend against replay attacks.*)
let rand_challenge = ref rand_int

let dh_pub_info = dh_pub_info ()

let dh_keys = create_dh_keys dh_pub_info

let shared_key = ref ""

let rsa_pub_key = ref ("", "")

(* [send_str str socket] writes string [str] to socket [socket]. Note:
   all strings sent to the server must start with "op=[op] [id]". If the
   string contains a random challenge (which most do), then the string
   must start with "op=[op] [id] r=[r]"*)
let send_str (str : string) socket =
  ignore (write_substring socket str 0 (String.length str))

let prompt_uname_message () =
  let uname = read_line () in
  let _ =
    fprintf Stdlib.stdout "%s %!"
      "\nPlease enter the message, then hit enter.\n\n"
  in
  let msg = read_line () in
  [ uname; msg ]

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

(* [create_enc_msg parent_msg enc_msg] encryptes [enc_msg] and returns a
   string message with the encrypted message stored in
   [parent_msg].dh_encrypted*)
let create_enc_msg parent_msg enc_msg =
  let enc = enc_msg |> msg_to_str |> encrypt_dh !shared_key in
  msg_to_str { parent_msg with dh_encrypted = enc }

(* [handle_login socket] prompts the user for their username and
   password during login and then sends the login request to the server
   using socket [socket].*)
let handle_login msg socket =
  fprintf Stdlib.stdout "%s %!"
    "\nPlease enter your username, then hit enter.\n\n";
  match prompt_uname_pass () with
  | uname :: password :: e ->
      fprintf Stdlib.stdout "%s %!" "\nLogging in...\n";
      send_str
        (create_enc_msg
           { msg with op = "post_auth" }
           { empty_msg with op = "login"; uname; password })
        socket
  | _ -> failwith "Login Unsuccessful"

(* [handle_register msg socket] prompts the user for their username and
   password during registration and then sends the registration request
   to the server using socket [socket].*)
let handle_register msg socket =
  fprintf Stdlib.stdout "%s %!"
    "\n\
     Please enter your new username, then hit enter. Usernames must \
     not contain any spaces.\n\n";
  match prompt_uname_pass () with
  | uname :: password :: e ->
      fprintf Stdlib.stdout "%s %!" "\nRegistering...\n";
      send_str
        (create_enc_msg
           { msg with op = "post_auth" }
           { empty_msg with op = "register"; uname; password })
        socket
  | _ -> failwith "Registration Unsuccessful"

(* [handle_success socket] prompts the user to either login or register
   after the server authentication process has completed, and calls
   [handle_login socket] or [handle_register socket].*)

let rec handle_success msg socket : string =
  fprintf Stdlib.stdout "%s %!"
    "\n\
     Type \"login\" and hit enter to login, or type \"register\" and \
     hit enter to register.\n\n";
  let cmd = read_line () in
  match cmd with
  | "login" ->
      handle_login msg socket;
      ""
  | "register" ->
      handle_register msg socket;
      ""
  | _ ->
      fprintf Stdlib.stdout "%s %!"
        "\nInvalid value. Please try again.\n";
      ignore (handle_success msg socket);
      ""

let handle_msg msg socket =
  fprintf Stdlib.stdout "%s %!"
    "\nPlease enter the username of the recipient, then hit enter.\n\n";
  match prompt_uname_message () with
  | reciever :: message :: e ->
      fprintf Stdlib.stdout "%s %!" "\nRegistering...\n";
      send_str
        (create_enc_msg
           { msg with op = "post_auth" }
           { empty_msg with op = "send_msg"; reciever; message })
        socket
  | _ -> failwith "Message Sending Unsuccessful"

let rec prompt_cmd msg socket =
  fprintf Stdlib.stdout "%s %!"
    "\n\
     Available commands: \"send message\", \"list new messages\", \
     \"list users\", \"quit\". Type a command and press enter. Note \
     that after new messages are listed they will be deleted from the \
     database for security purposes.\n\n";
  let cmd = read_line () in
  match cmd with
  | "send message" -> handle_msg msg socket
  | "list new messages" ->
      send_str
        (create_enc_msg
           { msg with op = "post_auth" }
           { empty_msg with op = "list_new_msgs" })
        socket
  | "list users" ->
      send_str
        (create_enc_msg
           { msg with op = "post_auth" }
           { empty_msg with op = "list_users" })
        socket
  | "quit" ->
      fprintf Stdlib.stdout "%s %!" "\nExiting...\n";
      exit 0
  | _ ->
      fprintf Stdlib.stdout "%s %!"
        "\nInvalid value. Please try again.\n";
      prompt_cmd msg socket

(* [rand_response str socket] extracts the random number challenge
   response from the message [str] sent by the server. Calls
   [handle_success socket] if the challenge was completed. Raises
   "Random Challenge Failed" if the challenge was failed.*)
let rand_response msg socket =
  let decrypted_msg =
    msg.dh_encrypted |> decrypt_dh !shared_key |> int_of_string
  in
  if decrypted_msg = !rand_challenge + 1 then
    let _ =
      fprintf Stdlib.stdout "%s %!"
        "\nRandom Challenge Succesfull\n\nHandshake Complete!\n"
    in
    handle_success msg socket
  else failwith "Random Challenge Failed"

let handle_enc_msg s parent_msg msg =
  match msg.op with
  | "reg_success" ->
      fprintf Stdlib.stdout "%s %!"
        "\nRegistration Succesful! You can now login.\n";
      handle_login msg s
  | "reg_failure" ->
      fprintf Stdlib.stdout "%s %!"
        "\nUsername already exists, please try again.\n";
      handle_register msg s
  | "login_success" ->
      fprintf Stdlib.stdout "%s %!" "\nLogin Successful! \n";
      prompt_cmd parent_msg s
      (* Leaving off here, now implement what happens after a successful
         login *)
  | "login_failure" ->
      fprintf Stdlib.stdout "%s %!"
        "\nInvalid username or password, please try again. \n";
      let _ = handle_success parent_msg s in
      ()
  | "message_success" ->
      fprintf Stdlib.stdout "%s %!"
        "\nEncrypted message successfully sent!. \n";
      prompt_cmd parent_msg s
  | "message_failure" ->
      fprintf Stdlib.stdout "%s %!"
        "\nRecipient username does not exist, please try again. \n";
      prompt_cmd parent_msg s
  | "list_message" ->
      fprintf Stdlib.stdout "%s %!" msg.message;
      prompt_cmd parent_msg s
  | "list_unames" ->
      fprintf Stdlib.stdout "%s %!" msg.message;
      prompt_cmd parent_msg s
  | _ -> ()

(* [handle_msg str s] handles the server response [msg] according to its
   operation, and replies using socket [s]. *)
let handle_msg msg s =
  match msg.op with
  | "ok" ->
      send_str
        (msg_to_str
           {
             msg with
             op = "diffie_1";
             id = uid;
             r = 0;
             mod_p = dh_pub_info.mod_p;
             prim_root_p = dh_pub_info.prim_root_p;
             pub_key_client = dh_keys |> dh_get_public_key;
           })
        s;
      fprintf Stdlib.stdout "%s %!" "Diffie Hellman initiating\n";
      ""
  | "diffie_2" ->
      let new_keys =
        create_dh_shared_key dh_keys msg.pub_key_server
          (extract_pub_info msg)
      in
      let shared_k = match new_keys.private_key with x, y -> y in
      shared_key := Z.to_string shared_k;
      let _ =
        fprintf Stdlib.stdout "%s %!" ("shared key is " ^ !shared_key)
      in
      let decrypted_msg = msg.dh_encrypted |> decrypt_dh !shared_key in
      fprintf Stdlib.stdout "%s %!"
        ("\n\ndecrypted message is: " ^ decrypted_msg ^ "\n\n");
      send_str
        (msg_to_str
           {
             msg with
             op = "random_challenge";
             id = uid;
             dh_encrypted =
               encrypt_dh !shared_key (string_of_int !rand_challenge)
               |> encrypt_rsa !rsa_pub_key
               |> dh_lst_to_str;
           })
        s;
      fprintf Stdlib.stdout "%s %!" "Diffie Hellman complete\n";
      ""
  | "random_response" -> rand_response msg s
  | "post_auth" ->
      msg.dh_encrypted |> decrypt_dh !shared_key |> str_to_msg
      |> handle_enc_msg s msg;
      ""
  | _ -> ""

(* [recieve_init s] listens for messages sent from the server on socket
   [s] and calls [handle_msg] to process the messages.*)
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
        match handle_msg (str_to_msg str) s with
        | "complete" -> ()
        | _ -> loop ())
  in
  loop ()

(* [handshake s] sends the message to the server using socket [s] that
   initializes the handshake*)
let handshake s =
  send_str (msg_to_str { empty_msg with op = "init"; id = uid }) s

let load_pub_key () =
  match Core.In_channel.read_lines "public_key.txt" with
  | [ x; y ] -> rsa_pub_key := (x, y)
  | _ -> failwith "Invalid key found at /client/public_key.txt"

let main () =
  fprintf Stdlib.stdout "%s %!" "Loading Server Public Key...\n";
  load_pub_key ();

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
  connect socket (ADDR_INET (inet_addr_of_string ip, port));
  handshake socket;
  recieve_init socket
;;

main ()
