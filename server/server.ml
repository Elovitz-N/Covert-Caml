open Core
open Async
open Sys

(* [send_str s w] sends string [s] using writer [w]. *)
let send_str s w = Writer.write w s ~len:(String.length s)

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
  let _ = print_string "here" in
  let lst = try In_channel.read_lines f with _ -> [] in
  if List.is_empty lst then send_str "No previous messages.\n" w
  else send_str "Previous messages: \n" w;
  send_lst lst w

let rec store_msg buffer r =
  let _ = print_string "Store msg running" in
  Reader.read r buffer >>= function
  | `Eof -> return ()
  | `Ok bytes_read ->
      let read = Bytes.sub buffer 0 bytes_read in
      let str = Bytes.to_string read in
      let _ =
        print_string
          ("Msg Recieved: " ^ str ^ " with length "
          ^ string_of_int (String.length str))
      in
      let oc = Out_channel.create ~append:true "../data/msgs.json" in
      Out_channel.output_string oc str;
      Out_channel.close oc;
      Out_channel.flush oc;
      store_msg buffer r

(* [perform_tasks w r] performs all of the tasks that should be
   performed by the server after the server is started.*)
let perform_tasks w r =
  print_string "here";
  send_msgs w "../data/msgs.json";
  send_str "Type your message and hit enter to send: \n" w;
  let buffer = Bytes.create (16 * 1024) in
  store_msg buffer r

(** Starts a TCP server, which listens on the specified port, calling
    all of the function in [perform_tasks] *)
let run () =
  print_string "Server Running";
  let host_and_port =
    Tcp.Server.create ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port 8765) (fun _addr r w ->
        perform_tasks w r)
  in
  ignore
    (host_and_port
      : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t)

(* Call [run], and then start the scheduler *)
let () =
  run ();
  never_returns (Scheduler.go ())
