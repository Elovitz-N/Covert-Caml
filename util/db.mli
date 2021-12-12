module Db : sig
  val file : string
  (**[file] is the name of a json file.*)

  type username = string
  type session_id = username
  type password = session_id

  exception DNE of password

  type msg = {
    sender : username;
    msg : string;
  }
  (** type [msg] is the type that stores the information of a message*)

  type user = {
    session_id : password;
    mutable username : password;
    mutable password : password;
    mutable messages : msg list;
  }
  (** type [user] is the type that stores the information of a message*)

  type t = { mutable users : user list }
  (** type [t] is the type that stores the information of a message*)

  val trim_quotes : string -> string
  (**[trim_quotes str] removes the quotations from a given input*)
  val msgs_of_json : Yojson.Basic.t -> msg
  (**[msgs_of_json json] extracts message information from a json*)
  val users_of_json : Yojson.Basic.t -> user
  (**[users_of_json json] extracts user information from a json*)
  val get_users : password -> user list
  (**[get_users db] retrieves user information from a database*)
  val check_user : password -> password -> bool
  (**[check_user db] checks to see if a given user exists in the database*)
  val from_json : password -> t
  (**[from_json db] retrieves information from a database*)

  val get_uname : session_id -> string -> string
  (**[get_uname id db] returns the username associated with the session id
     [id]. Raises "DNE" if that session id does not exist. *)

  val get_password : session_id -> string -> password
    (* [get_password id db] returns the password associated with the session
     id [id]. Raises "DNE" if that session id does not exist.*)

  val get_new_msgs : session_id -> string -> string
  (* [get_new_msgs id db] returns a list of the new messages associated
     with the session id [id]. Raises "DNE" if that session id does not
     exist.*)

  val update_k :
    string -> Yojson.Basic.t -> Yojson.Basic.t -> Yojson.Basic.t
     (* [update_k key v json] returns the json object [json] updated by
     setting key [key] to value [v]. Raises "Empty json" if the
     corresponding json association list is empty.*)

  val update_list :
    string ->
    Yojson.Basic.t ->
    string ->
    Yojson.Basic.t ->
    Yojson.Basic.t list ->
    bool ->
    Yojson.Basic.t list
  (* [update_list key v check_k check_v json_lst delete_msgs] returns
     the updated json users list. The function finds the user to update
     by checking field [check_k] against value [check_v]. It then
     updates the ([key],[v]) field for that user by calling [update_k].
     If [key] = "new messages" and [delete_msgs] = true, then all new
     messages corresponding to the user corresponding to
     ([check_k],[check_v]) will be deleted. Raises "unknown json type"
     if the json_lst field is not a json String or json List*)

  val update_json :
    (string * ([> `List of Yojson.Basic.t list ] as 'a)) list ->
    string ->
    Yojson.Basic.t ->
    string ->
    Yojson.Basic.t ->
    bool ->
    bool ->
    (string * 'a) list
      (* [update_json obj key v check_k check_v delete_msgs add] returns the
     updated json association list by calling [update_list key v check_k
     check_v x delete_msgs]. If [add] = true, then the json obj [obj]
     will be added to the user list. Raises "invalid user list" if [obj]
     is not a json association list. *)

  val update :
    string ->
    Yojson.Basic.t ->
    string->
    Yojson.Basic.t ->
    Yojson.Basic.t ->
    bool ->
    bool ->
    Yojson.Basic.t
  (* [update key v check_k check_v json delete_msgs add] parses the
     Yojson.Basic.t object [json] and returns the updated json object by
     calling [update_json] if the json is an Association object. Returns
     the original json object if it was not a valid association
     object. *)

  val put_uname : string -> string -> unit
  (* [put_uname id new_uname] replaces the username in the database file
     with the specified username [new_uname] associated with the session
     id [id]. *)


  val put_passwd : string -> string -> unit
  (* [put_passwd id new_passwd] replaces the password in the database
     file with the specified password [new_passwd] associated with the
     session id [id]. *)

  val put_msg : username -> msg -> unit
  (* [put_msg reciever new_msg] adds the msg [new_msg] to the new
     messages array associated with username [reciever] in the database
     file. *)
  val put_id : string -> string -> unit
  (* [put_id uname] replaces the session id in the database file with
     the specified session id [id] associated with the username [uname].
     Raises "DNE" if that username does not exist. *)
  val add_user : string -> string-> string -> unit
     (* [add_user id uname passwd] adds a user to the database file with
     session id [id], username [uname], password [passwd], and an empty
     new messages array. This function will be called when a client
     creates an account with the service. *)
  val delete_msgs : string -> unit
  (* [delete_msgs id] deletes the new messages associated with the user
     that has session id [id] from the database file. *)
  val list_unames : string -> password
  (* [list_unames string] creates a list of the usernames in a database*)
  val create_db : unit -> unit
  (* [create_db ()] creates the json database file and initializes an
     empty users array. *)
end