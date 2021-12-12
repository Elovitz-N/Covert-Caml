open Keys

type info = {
  id : string;
  dh_key : Z.t;
}
(* type [info] is the type that maps a session id value to the
   corresponding dh_key. **)

type msg = {
  op : string;
  id : string;
  r : int;
  mod_p : Z.t;
  prim_root_p : Z.t;
  pub_key_client : string;
  pub_key_server : string;
  dh_encrypted : string;
  uname : string;
  password : string;
  reciever : string;
  message : string;
}
(** type [msg] is the type that stores the all of the fields required in
    a message sent in between the client and server.*)

val empty_msg : msg
(** [empty_msg] is an message initialized with default values. *)

val process : string list -> msg -> msg
(** [process lst msg] processes the list of strings lst and outputs a
    msg with the contents of [msg] updated according to the arguments in
    the strings. Required: no element of [lst] contains any spaces.*)

val encrypted_str : string -> string -> string
(** [encrypted_str str keyword] returns the substring of str [str] that
    occurs after the string "[keyword]=". For example, encrypted_str
    "blah DIFFIE=testing test" "DIFFIE" returns "testing test". Returns
    the empty string if [keyword] is not found in [str]. *)

val str_to_msg : string -> msg
(** [str_to_msg str] returns the string [str] converted to a message
    based on message field found in [str], where [str] is a string sent
    between the client and server. *)

val msg_to_str : msg -> string
(** [msg_to_str msg] returns the msg [msg] converted into a string ready
    to be sent between the client and server. *)

val extract_pub_info : msg -> pub_info
(** [extract_pub_info msg] returns a value of type info that contains
    the public diffie hellman info associated with [msg] *)

val dh_str_to_lst : string -> string list
(** [dh_str_to_lst str] returns a list of strings that is [str]
    seperated into a list based on occurences of a keyword. *)

val dh_lst_to_str : string list -> string
(* [dh_lst_to_str lst] returns the elements of [lst[ combined into a
   string based on occurences of a keyword. *)

val get_key : string -> info list -> string
(** [get_key id] returns the private diffie hellman key associated with
    the session that is using [id]*)
