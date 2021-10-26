open Core
open Async
open Sys
open Keys

(* [send_str s w] sends string [s] using writer [w]. *)
let send_str s w = Writer.write w s ~len:(String.length s)

let msg_file = "../data/msgs.json"

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

(* [rand_challenge str w] completes the random challenge in string [str]
   and writes the response using writer [w]. *)
let rand_challenge str w =
  let r = extract_r str in
  send_str
    ("op=random_response r="
    ^ (r |> int_of_string |> ( + ) 1 |> string_of_int))
    w

(* [handle_str str n w f] handles the string str sent from the client
   based on its operation, using length [n], writer [w], and file [f] as
   arguments in functions that it calls.*)
let handle_str str n w f =
  let op = extract_op str in
  match op with
  | "init" -> send_str "op=ok " w
  | "diffie" -> send_str "op=diffie_complete " w
  | "random_challenge" -> rand_challenge str w
  | "login" ->
      ()
      (* TODO - one task I can do soon is set up the session ID
         generator and send it back to the client*)
  | "register" -> () (* TODO *)
  | _ ->
      let _ =
        print_string
          ("Msg Recieved: " ^ str ^ " with length "
          ^ string_of_int (String.length str))
      in
      (* This is where it writes str to the file *)
      let oc = Out_channel.create ~append:true f in
      Out_channel.output_string oc str;
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
      handle_str str bytes_read w f;
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

(* Call [run], and then start the scheduler *)
let () =
  run ();
  never_returns (Scheduler.go ())
