open Yojson.Basic.Util

type username = string

type session_id = string

type msg = {
  sender : string;
  msg : string;
}

let file = "db.json"

(* [get_uname id] returns the username associated with the session id
   [id]. Raises "DNE" if that session id does not exist. *)
let get_uname (id : string) : string = failwith "unimplemented"

(* [get_password id] returns the password associated with the session id
   [id]. Raises "DNE" if that session id does not exist.*)
let get_password (id : string) : string = failwith "unimplemented"

(* [get_new_msgs id] returns a list of the new messages associated with
   the session id [id]. Raises "DNE" if that session id does not
   exist.*)
let get_new_msgs (id : string) : msg list = failwith "unimplemented"

(* [put_uname id] replaces the username in the database file with the
   specified username [new_uname] associated with the session id [id].
   Raises "DNE" if that session id does not exist. *)
let put_uname (id : string) (new_uname : string) =
  failwith "unimplemented"

(* [put_passwd id] replaces the password in the database file with the
   specified password [new_passwd] associated with the session id [id].
   Raises "DNE" if that session id does not exist. *)
let put_passwd (id : string) (new_passwd : string) =
  failwith "unimplemented"

(* [put_msg reciever new_msg] adds the msg [new_msg] to the new messages
   array associated with username [reciever] in the database file.
   Raises "DNE" if that username does not exist. *)
let put_msg (reciever : username) (new_msg : msg) =
  failwith "unimplemented"

(* [put_id uname] replaces the session id in the database file with the
   specified session id [id] associated with the username [uname].
   Raises "DNE" if that username does not exist. *)
let put_id (id : string) (uname : string) = failwith "unimplemented"

(* [add_user id uname passwd] adds a user to the database file with
   session id [id], username [uname], password [passwd], and an empty
   new messages array. This function will be called when a client
   creates an account with the service. *)
let add_user (id : string) (uname : string) (passwd : string) =
  failwith "unimplemented"

(* [delete_msg uname msg] deletes the message [msg] associated with
   username [uname] from the database file. Raises "DNE" if that
   username does not exist. *)
let delete_msg (uname : string) (msg : msg) = failwith "unimplemented"

(* [list_unames] returns a list of all the usernames in the database
   file. *)
let list_unames : string list = failwith "unimplemented"