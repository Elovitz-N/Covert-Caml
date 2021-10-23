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

let handle_str str n w f =
  let i = String.index_exn str ' ' in
  let op = String.sub str 3 (i - 3) in
  match op with
  | "init" -> send_str "ok" w
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
