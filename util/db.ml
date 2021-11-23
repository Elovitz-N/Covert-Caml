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
open Yojson.Basic

let file = "../data/db.json"

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

let trim_quotes str = String.sub str 1 (String.length str - 2)

let msgs_of_json json =
  {
    sender = json |> member "username" |> to_string |> trim_quotes;
    msg = json |> member "message" |> to_string |> trim_quotes;
  }

let users_of_json json =
  {
    session_id = json |> member "session id" |> to_string |> trim_quotes;
    username = json |> member "username" |> to_string |> trim_quotes;
    password = json |> member "password" |> to_string |> trim_quotes;
    messages =
      json |> member "new messages" |> to_list |> List.map msgs_of_json;
  }

let get_users (db : string) =
  let json = Yojson.Basic.from_file db in
  json |> member "users" |> to_list |> List.map users_of_json

(* [check_user db uname] returns true if there is a user with username
   [uname] in the database file [db].*)
let check_user (db : string) uname =
  List.filter (fun x -> x.username = uname) (get_users db) <> []

let from_json db = { users = get_users db }

let rec get_username_helper id lst =
  match lst with
  | [] -> raise (DNE id)
  | h :: t ->
      if h.session_id = id then h.username else get_username_helper id t

(* [get_uname id] returns the username associated with the session id
   [id]. Raises "DNE" if that session id does not exist. *)
let get_uname (id : session_id) (db : string) : string =
  let user_list = get_users db in
  get_username_helper id user_list

let rec get_password_helper id user_list =
  match user_list with
  | [] -> raise (DNE id)
  | h :: t ->
      if h.session_id = id then h.password else get_password_helper id t

(* [get_password id] returns the password associated with the session id
   [id]. Raises "DNE" if that session id does not exist.*)
let get_password (id : session_id) (db : string) =
  let user_list = get_users db in
  get_password_helper id user_list

let rec get_new_msgs_helper id user_list =
  match user_list with
  | [] -> raise (DNE id)
  | h :: t ->
      if h.session_id = id then h.messages else get_new_msgs_helper id t

(* [get_new_msgs id] returns a list of the new messages associated with
   the session id [id]. Raises "DNE" if that session id does not
   exist.*)
let get_new_msgs (id : session_id) (db : string) =
  let user_list = get_users db in
  let lst = get_new_msgs_helper id user_list in
  List.fold_left
    (fun acc msg ->
      acc ^ "\nSender: " ^ msg.sender ^ "\nMessage: " ^ msg.msg)
    "New messages: \n" lst

(* [update_k key v json] returns the json object [json] updated by
   setting key [key] to value [v]. Raises "Empty json" if the
   corresponding json association list is empty.*)
let update_k key (v : Yojson.Basic.t) (json : Yojson.Basic.t) =
  let rec update_key = function
    | [] -> failwith "Empty json"
    | ((k, v') as m) :: tl ->
        if k = key then (k, v) :: tl else m :: update_key tl
  in
  match json with `Assoc obj -> `Assoc (update_key obj) | _ -> json

(* [update_list key v check_k check_v json_lst delete_msgs] returns the
   updated json users list. The function finds the user to update by
   checking field [check_k] against value [check_v]. It then updates the
   ([key],[v]) field for that user by calling [update_k]. If [key] =
   "new messages" and [delete_msgs] = true, then all new messages
   corresponding to the user corresponding to ([check_k],[check_v]) will
   be deleted. Raises "unknown json type" if the json_lst field is not a
   json String or json List*)
let rec update_list key v check_k check_v json_lst delete_msgs =
  match json_lst with
  | h :: t ->
      let cv = h |> member check_k in
      if cv = check_v then
        match h |> member key with
        | `String v' ->
            let _ = print_string "match found" in
            update_k key v h :: t
        | `List messages ->
            print_string "\n deep deep list found \n";
            if delete_msgs then update_k key (`List []) h :: t
            else
              let j = v :: messages in
              update_k key (`List j) h :: t
        | _ -> failwith "Unkown json type"
      else h :: update_list key v check_k check_v t delete_msgs
  | [] -> json_lst

(* [update_json obj key v check_k check_v delete_msgs add] returns the
   updated json association list by calling [update_list key v check_k
   check_v x delete_msgs]. If [add] = true, then the json obj [obj] will
   be added to the user list. Raises "invalid user list" if [obj] is not
   a json association list. *)
let update_json obj key v check_k check_v delete_msgs add =
  match obj with
  | [] -> failwith "empty obj"
  | (k, v') :: tl -> (
      match v' with
      | `List users ->
          if add then
            let j = v :: users in
            ("users", `List j) :: tl
          else
            let updated =
              update_list key v check_k check_v users delete_msgs
            in
            ("users", `List updated) :: tl
      | _ -> failwith "invalid user list")

(* [update key v check_k check_v json delete_msgs add] parses the
   Yojson.Basic.t object [json] and returns the updated json object by
   calling [update_json] if the json is an Association object. Returns
   the original json object if it was not a valid association object. *)
let update key v check_k check_v (json : Yojson.Basic.t) delete_msgs add
    =
  match json with
  | `Assoc obj ->
      print_string "assoc found";
      `Assoc (update_json obj key v check_k check_v delete_msgs add)
  | _ -> json

(* [put_uname id new_uname] replaces the username in the database file
   with the specified username [new_uname] associated with the session
   id [id]. *)
let rec put_uname id new_uname =
  let json = from_file file in
  let new_json =
    update "username" (`String new_uname) "session id" (`String id) json
      false false
  in
  to_file file new_json

(* [put_passwd id new_passwd] replaces the password in the database file
   with the specified password [new_passwd] associated with the session
   id [id]. *)
let put_passwd (id : string) (new_passwd : string) =
  let json = from_file file in
  let new_json =
    update "password" (`String new_passwd) "session id" (`String id)
      json false false
  in
  to_file file new_json

(* [put_msg reciever new_msg] adds the msg [new_msg] to the new messages
   array associated with username [reciever] in the database file. *)
let put_msg (reciever : username) (new_msg : msg) =
  let j = from_file file in
  let msg =
    from_string
      ("{\"username\":\"" ^ new_msg.sender ^ "\",\"message\":\""
     ^ new_msg.msg ^ "\"}")
  in
  let new_j =
    update "new messages" msg "username" (`String reciever) j false
      false
  in
  to_file file new_j

(* [put_id uname] replaces the session id in the database file with the
   specified session id [id] associated with the username [uname].
   Raises "DNE" if that username does not exist. *)
let put_id (id : string) (uname : string) =
  let json = from_file file in
  let new_json =
    update "session id" (`String id) "username" (`String uname) json
      false false
  in
  to_file file new_json

(* [add_user id uname passwd] adds a user to the database file with
   session id [id], username [uname], password [passwd], and an empty
   new messages array. This function will be called when a client
   creates an account with the service. *)
let add_user (id : string) (uname : string) (passwd : string) =
  let j = from_file file in
  let user =
    from_string
      ("{\n         \"session id\": \"" ^ id
     ^ "\",\n         \"username\": \"" ^ uname
     ^ "\",\n         \"password\": \"" ^ passwd
     ^ "\",\n         \"new messages\": []\n       }")
  in
  let new_j = update "" user "username" (`String "") j false true in
  to_file file new_j

(* [delete_msgs id] deletes the new messages associated with the user
   that has session id [id] from the database file. *)
let delete_msgs (id : string) =
  let j = from_file file in
  let msg = from_string "{}" in
  let new_j =
    update "new messages" msg "session id" (`String id) j true false
  in
  to_file file new_j

(* [list_unames] returns a list of all the usernames in the database
   file. *)
let list_unames () = failwith "unimplemented"

(* [create_db ()] creates the json database file and initializes an
   empty users array. *)
let create_db () =
  let json = from_string "{\"users\":[]}" in
  to_file file json

let test_write () =
  print_string "testing";
  print_newline ();
  let j = from_file file in
  let new_j =
    update "username" (`String "test6") "session id"
      (`String "sdafj434jnl34g33il4h3") j false false
  in
  to_file "../data/test.json" new_j;
  let j = from_file file in
  let msg = from_string "{\"username\":\"bob\",\"message\":\"woah\"}" in
  let new_j =
    update "new messages" msg "session id"
      (`String "sdafj434jnl34g33il4h3") j true false
  in
  to_file "../data/test2.json" new_j;
  let j = from_file "../data/test2.json" in
  let msg = from_string "{\"username\":\"bob\",\"message\":\"woah\"}" in
  let new_j =
    update "new messages" msg "session id"
      (`String "sdafj434jnl34g33il4h3") j false false
  in
  to_file "../data/test3.json" new_j;
  add_user "sess" "dummyu" "dummyp";
  print_newline ()
