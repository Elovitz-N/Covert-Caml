(* This file is some function defns that Ria made, I'm not sure if it
   will be helpful to use those or if it would be easier to just start
   over. Regaurdless, here are the functions we need: for every field in
   the db.json file (i.e. username, password, new messages, etc.) we
   need: 1) a function to read the value of that field based on a
   session ID 2) a function to write the value of that field based on a
   user ID.

   Additionally we need a function to add a user, which means add a user
   with every field to the json file, using a given username and
   password. We need a function to delete new messages based on a
   session id. We need a function to write a session ID value based on a
   username*)
open Yojson.Basic.Util

let file = "db.json"

type username = string

type session_id = string

type password = string

exception DNE of session_id

(*let person = `Assoc [ ("name", `String "Anil") ] *)

type msg = {
  sender : username;
  msg : string;
}

type user = {
  session_id : session_id;
  mutable username : username;
  mutable password : password;
  mutable messages : msg list;
}

type t = { mutable users : user list }

(*let update key f json = let rec update_json_obj = function | [] ->
  begin match f None with None -> [] | Some v -> [ (key, v) ] end | ((k,
  v) as m) :: tl -> if k = key then match f (Some v) with | None ->
  update_json_obj tl | Some v' -> if v' == v then m :: tl else (k, v')
  :: tl else m :: update_json_obj tl in

  match json with | `Assoc obj -> `Assoc (update_json_obj obj) | _ ->
  json *)
let msgs_of_json json =
  {
    sender = json |> member "username" |> to_string;
    msg = json |> member "message" |> to_string;
  }

let users_of_json json =
  {
    session_id = json |> member "session id" |> to_string;
    username = json |> member "username" |> to_string;
    password = json |> member "password" |> to_string;
    messages =
      json |> member "new messages" |> to_list |> List.map msgs_of_json;
  }

let get_users json =
  json |> member "users" |> to_list |> List.map users_of_json

let from_json json = { users = get_users json }

let rec get_username_helper id user_list =
  match user_list with
  | [] -> raise (DNE id)
  | h :: t ->
      if h.session_id = id then h.username else get_username_helper id t

(* [get_uname id] returns the username associated with the session id
   [id]. Raises "DNE" if that session id does not exist. *)
let get_uname (id : session_id) (chat : t) : string =
  get_username_helper id chat.users

let rec get_password_helper id user_list =
  match user_list with
  | [] -> raise (DNE id)
  | h :: t ->
      if h.session_id = id then h.password else get_password_helper id t

(* [get_password id] returns the password associated with the session id
   [id]. Raises "DNE" if that session id does not exist.*)
let get_password (id : session_id) (chat : t) =
  get_password_helper id chat.users

let rec get_new_msgs_helper id user_list =
  match user_list with
  | [] -> raise (DNE id)
  | h :: t ->
      if h.session_id = id then h.messages else get_new_msgs_helper id t

(* [get_new_msgs id] returns a list of the new messages associated with
   the session id [id]. Raises "DNE" if that session id does not
   exist.*)
let get_new_msgs (id : session_id) (chat : t) =
  get_new_msgs_helper id chat.users

(* [get_dh_key id] returns the dh key associated with session id
   [id]. *)
let get_dh_key (id : session_id) = failwith "unimplemented"

(* [put_uname id] replaces the username in the database file with the
   specified username [new_uname] associated with the session id [id].
   Raises "DNE" if that session id does not exist. *)
let rec change_username id new_uname user_list =
  match user_list with
  | [] -> raise (DNE id)
  | h :: t ->
      if h.session_id = id then h.username = new_uname
      else change_username id new_uname t

let put_uname (id : string) (new_uname : string) (chat : t) =
  change_username id new_uname chat.users

let rec change_password id new_password user_list =
  match user_list with
  | [] -> raise (DNE id)
  | h :: t ->
      if h.session_id = id then h.password = new_password
      else change_password id new_password t

(* [put_passwd id] replaces the password in the database file with the
   specified password [new_passwd] associated with the session id [id].
   Raises "DNE" if that session id does not exist. *)
let put_passwd (id : string) (new_passwd : string) (chat : t) =
  change_password id new_passwd chat.users

(* [put_msg reciever new_msg] adds the msg [new_msg] to the new messages
   array associated with username [reciever] in the database file.
   Raises "DNE" if that username does not exist. *)
let put_msg (sender : username) (reciever : username) (new_msg : msg) =
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
