open Core
open Async
open Sys
open Util.Msg
open Util.Keys
open Util.Db

(* [info_lst] is an info list ref that stores the id, dh_key pairs *)
let (info_lst : info list ref) = ref []

(* [send_str s w] sends string [s] using writer [w]. *)
let send_str s w = Writer.write w s ~len:(String.length s)

let msg_file = "../data/msgs.json"

let rsa_keys =
  ref { private_key = Z.zero; public_key = (Z.zero, Z.zero) }

(* [send_lst l w] sends the list of string l using writer w. A new line
   character will be appended to each string in the list as it is sent.
   Requires: all elements in l are strings. *)
let rec send_lst l w =
  match l with
  | [] -> Writer.write w "" ~len:0
  | h :: t ->
      send_str (h ^ "\n") w;
      send_lst t w

(* [send_msgs w f] sends the messages in file f line by line using
   writer w. *)
let send_msgs w f =
  let lst = try In_channel.read_lines f with _ -> [] in
  if List.is_empty lst then
    (* If the file does not exist, the following lines will create the
       file*)
    let _ = send_str "No previous messages.\n" w in
    Out_channel.write_all f ~data:""
  else send_str "\nPrevious messages: \n" w;
  send_lst lst w;
  send_str "\n" w

(* [rand_challenge msg w] completes the random challenge in message
   [msg] and writes the response using writer [w]. *)
let rand_challenge msg w =
  let decrypted_msg =
    msg.dh_encrypted |> dh_str_to_lst |> decrypt_rsa !rsa_keys
    |> decrypt_dh (get_key msg.id !info_lst)
  in
  let dh_encrypted_msg =
    decrypted_msg |> int_of_string |> ( + ) 1 |> Z.of_int |> Z.to_string
    |> encrypt_dh (get_key msg.id !info_lst)
  in
  send_str
    (msg_to_str
       {
         msg with
         dh_encrypted = dh_encrypted_msg;
         op = "random_response";
       })
    w

(* [create_enc_msg parent_msg enc_msg] encryptes [enc_msg] and returns a
   string message with the encrypted message stored in
   [parent_msg].dh_encrypted*)
let create_enc_msg parent_msg enc_msg =
  let enc =
    enc_msg |> msg_to_str
    |> encrypt_dh (get_key parent_msg.id !info_lst)
  in
  msg_to_str { parent_msg with dh_encrypted = enc }

(* [handle__enc_msg msg w] handles the message msg sent from the client
   in the encrypted portion of the message based on its operation, using
   writer [w] as arguments in functions that it calls.*)
let handle_enc_msg msg w =
  match msg.op with
  | "register" ->
      if check_user "db.ml" msg.uname then
        send_str
          (create_enc_msg msg { empty_msg with op = "reg_failure" })
          w
      else add_user msg.id msg.uname msg.password;
      send_str
        (create_enc_msg msg { empty_msg with op = "reg_success" })
        w
  | "login" -> ()
  | _ -> ()

(* [handle_msg msg w f] handles the string str sent from the client
   based on its operation, using writer [w] and file [f] as arguments in
   functions that it calls.*)
let handle_msg msg w f =
  match msg.op with
  | "init" -> send_str (msg_to_str { msg with op = "ok" }) w
  | "diffie_1" ->
      let _ = print_string "diffie_1 beginning" in
      let pub_info = extract_pub_info msg in
      let dh_keys = pub_info |> create_dh_keys in
      let new_keys =
        create_dh_shared_key dh_keys msg.pub_key_client pub_info
      in
      let shared_key = match new_keys.private_key with x, y -> y in
      info_lst := { id = msg.id; dh_key = shared_key } :: !info_lst;
      print_string ("\nshared key is " ^ Z.to_string shared_key);

      (* Protocol for storing dh keys: 1) server derives dh key, stores
         <id, dh_key> 2) client logs in with id <uname, pass> 3) server
         stores id and dh_key with that uname, pass so when the server
         recieves a message, it firsts: checks the list to find the dh
         key associated with that id. Then, it looks at the action
         trying to be performed. If the action is login, it logs the
         user in. If it is register, it registers the user. In both of
         those cases it stores the id and dh_key with those users. If
         the action is list users, it checks to make sure the user is
         logged in by looking at the dh key used and seeing if that dh
         key exists in the db. This works bc in order to decrypt the
         message, the dh key must have been a valid key. If the op is
         send a message, the server authenticates the same way, and then
         sends the message. *)
      send_str
        (msg_to_str
           {
             msg with
             op = "diffie_2";
             pub_key_server = dh_keys |> dh_get_public_key;
             mod_p = pub_info.mod_p;
             prim_root_p = pub_info.prim_root_p;
             pub_key_client = msg.pub_key_client;
             dh_encrypted =
               encrypt_dh
                 (Z.to_string shared_key)
                 "encryption test is working";
           })
        w
  | "diffie_3" -> ()
  | "random_challenge" -> rand_challenge msg w
  | "login" ->
      ()
      (* TODO - one task I can do soon is set up the session ID
         generator and send it back to the client*)
  | "register" -> () (* TODO *)
  | "post_auth" ->
      msg.dh_encrypted
      |> decrypt_dh (get_key msg.id !info_lst)
      |> str_to_msg |> handle_enc_msg w
      (* post auth it will decrypt diffie, which will be a msg str, and
         then call handle_enc_msg to parse the msg*)
  | _ ->
      let _ = print_string ("Msg Recieved: " ^ msg_to_str msg) in
      (* This is where it writes str to the file *)
      let oc = Out_channel.create ~append:true f in
      Out_channel.output_string oc (msg_to_str msg);
      Out_channel.close oc;
      Out_channel.flush oc;
      (* TODO: add message here about the possibility of quitting once
         it gets up and running *)
      send_str
        "Message Recieved! Type another message in the format \
         \"[name]: [msg]\" and hit enter to send: \n\n"
        w

(* [store_msg buffer r w f] uses reader [r] to read data from [buffer]
   and appends it to file [f]. If file [f] does not exist, [f] will be
   created and the data in [buffer] will be written to the empty
   file. *)
let rec recieve buffer r w f =
  Reader.read r buffer >>= function
  | `Eof -> return ()
  | `Ok bytes_read ->
      let read = Bytes.sub buffer 0 bytes_read in
      (* str now contains the data that was in the buffer, converted to
         a string *)
      let str = Bytes.to_string read in
      let _ = print_string str in
      let msg = str_to_msg str in
      handle_msg msg w f;
      recieve buffer r w f

(* [perform_tasks w r] performs all of the tasks that should be
   performed by the server after the server is started.*)
let perform_tasks w r =
  let buffer = Bytes.create (16 * 1024) in
  recieve buffer r w msg_file
(* old code: send_msgs w msg_file; send_str "Type your message in the
   format \"[name]: [msg]\" and hit enter \ to send: \n\n" w; *)

(** Starts a TCP server, which listens on the specified port, calling
    all of the function in [perform_tasks] *)
let run () =
  print_string "Server Running";

  let host_and_port =
    Tcp.Server.create ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port 8886) (fun _addr r w ->
        perform_tasks w r)
  in
  ignore
    (host_and_port
      : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t)

let gen_rsa_keys () =
  let { private_key = p; public_key = k1, k2 } = create_rsa_keys () in
  let pub = Z.to_string k1 ^ "\n" ^ Z.to_string k2 in
  Out_channel.write_all "public_key.txt" ~data:pub;
  let priv = Z.to_string p in
  Out_channel.write_all "private_key.txt" ~data:priv;
  Core.fprintf Stdlib.stdout "%s %!"
    "\n\
     RSA private key, public key created. Private key is stored in \
     private_key.txt. Public key is stored in public_key.txt. In order \
     for the server to be able to authenticate to the client, make \
     sure that public_key.txt is stored inside of the \"/client/\" \
     folder in the repository on every machine that wants to run the \
     client application.\n\
    \ "

let load_rsa_keys () =
  match Core.In_channel.read_lines "../server/public_key.txt" with
  | [ x; y ] -> (
      rsa_keys :=
        { !rsa_keys with public_key = (Z.of_string x, Z.of_string y) };
      match Core.In_channel.read_lines "../server/private_key.txt" with
      | [ x ] ->
          rsa_keys := { !rsa_keys with private_key = Z.of_string x }
      | _ -> failwith "Invalid key found at /server/private_key.txt")
  | _ -> failwith "Invalid key found at /server/private_key.txt"

(* Call [run], and then start the scheduler *)
let () =
  let args = Sys.get_argv () in
  match Array.length args with
  | 2 ->
      if String.equal args.(1) "keygen" then
        let _ =
          Core.fprintf Stdlib.stdout "%s %!"
            "\nGenerating RSA public key, private key... \n"
        in
        gen_rsa_keys ()
  | _ ->
      (match Core.In_channel.read_lines "private_key.txt" with
      | [] ->
          failwith
            "No private key found. Try running dune exec ./server.exe \
             keygen"
      | key :: _ ->
          load_rsa_keys ();
          print_string "here");
      let _ = run () in
      never_returns (Scheduler.go ())

(* let word = test_write () *)
