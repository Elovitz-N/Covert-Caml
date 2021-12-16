open Yojson.Basic.Util
open Yojson.Basic

let file = "data/db.json"

type username = string

type session_id = string

type password = string

exception DNE of session_id

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

let check_user (db : string) uname =
  List.filter (fun x -> x.username = uname) (get_users db) <> []

let from_json db = { users = get_users db }

let rec get_username_helper id lst =
  match lst with
  | [] -> raise (DNE id)
  | h :: t ->
      if h.session_id = id then h.username else get_username_helper id t

let get_uname (id : session_id) (db : string) : string =
  let user_list = get_users db in
  get_username_helper id user_list

let rec get_password_helper id user_list =
  match user_list with
  | [] -> raise (DNE id)
  | h :: t ->
      if h.session_id = id then h.password else get_password_helper id t

let get_password (id : session_id) (db : string) =
  let user_list = get_users db in
  get_password_helper id user_list

let rec get_new_msgs_helper id user_list =
  match user_list with
  | [] -> raise (DNE id)
  | h :: t ->
      if h.session_id = id then h.messages else get_new_msgs_helper id t

let get_new_msgs (id : session_id) (db : string) =
  let user_list = get_users db in
  let lst = get_new_msgs_helper id user_list in
  List.fold_left
    (fun acc msg ->
      acc ^ "\nSender: " ^ msg.sender ^ "\nMessage: " ^ msg.msg)
    "\nNew messages: \n" lst

let update_k key (v : Yojson.Basic.t) (json : Yojson.Basic.t) =
  let rec update_key = function
    | [] -> failwith "Empty json"
    | ((k, v') as m) :: tl ->
        if k = key then (k, v) :: tl else m :: update_key tl
  in
  match json with `Assoc obj -> `Assoc (update_key obj) | _ -> json

let rec update_list key v check_k check_v json_lst delete_msgs =
  match json_lst with
  | h :: t ->
      let cv = h |> member check_k in
      if cv = check_v then
        match h |> member key with
        | `String v' -> update_k key v h :: t
        | `List messages ->
            if delete_msgs then update_k key (`List []) h :: t
            else
              let j = v :: messages in
              update_k key (`List j) h :: t
        | _ -> failwith "Unkown json type"
      else h :: update_list key v check_k check_v t delete_msgs
  | [] -> json_lst

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

let update key v check_k check_v (json : Yojson.Basic.t) delete_msgs add
    =
  match json with
  | `Assoc obj ->
      `Assoc (update_json obj key v check_k check_v delete_msgs add)
  | _ -> json

let rec put_uname id new_uname =
  let json = from_file file in
  let new_json =
    update "username" (`String new_uname) "session id" (`String id) json
      false false
  in
  to_file file new_json

let put_passwd (id : string) (new_passwd : string) =
  let json = from_file file in
  let new_json =
    update "password" (`String new_passwd) "session id" (`String id)
      json false false
  in
  to_file file new_json

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

let put_id (id : string) (uname : string) =
  let json = from_file file in
  let new_json =
    update "session id" (`String id) "username" (`String uname) json
      false false
  in
  to_file file new_json

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

let delete_msgs (id : string) =
  let j = from_file file in
  let msg = from_string "{}" in
  let new_j =
    update "new messages" msg "session id" (`String id) j true false
  in
  to_file file new_j

let list_unames db =
  let users = get_users db in
  let res =
    List.fold_left
      (fun acc user -> acc ^ user.username ^ ", ")
      "List of usernames: " users
  in
  String.sub res 0 (String.length res - 2) ^ "\n"

let create_db () =
  if Sys.file_exists file then ()
  else
    let json = from_string "{\"users\":[]}" in
    to_file file json
