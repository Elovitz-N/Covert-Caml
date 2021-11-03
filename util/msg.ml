open Keys

type msg = {
  op : string;
  id : string;
  r : int;
  mod_p : Z.t;
  prim_root_p : Z.t;
  pub_key_client : string;
  pub_key_server : string;
  dh_encrypted : string;
}

(* [empty_msg] is an message initialized with unimportant values. *)
let empty_msg =
  {
    op = "";
    id = "";
    r = 0;
    mod_p = Z.zero;
    prim_root_p = Z.zero;
    pub_key_client = "";
    pub_key_server = "";
    dh_encrypted = "";
  }

(* [process lst msg] processes the list of strings lst and outputs a
   message corresponding to the arguments in the strings.*)
let rec process lst msg =
  match lst with
  | h :: t -> (
      match String.split_on_char '=' h with
      | x :: y :: e -> (
          match x with
          (* NOTE: update here when new fields are added to msg *)
          | "op" -> process t { msg with op = y }
          | "r" -> process t { msg with r = int_of_string y }
          | "id" -> process t { msg with id = y }
          | "mod_p" -> process t { msg with mod_p = Z.of_string y }
          | "prim_root_p" ->
              process t { msg with prim_root_p = Z.of_string y }
          | "pub_key_client" ->
              process t { msg with pub_key_client = y }
          | "pub_key_server" ->
              process t { msg with pub_key_server = y }
          | _ -> process t msg)
      | _ -> msg)
  | [] -> msg

(* [encrypted_str str keyword] returns the substring of str [str] that
   occurs after the string "[keyword]=". For example, encrypted_str
   "blah DIFFIE=testing test" "DIFFIE" returns "testing test". Returns
   the empty string if [keyword] is not found in [str]. *)
let encrypted_str str keyword =
  let keyword_len = String.length keyword in
  let str_len = String.length str in
  let res = ref "" in
  for i = 0 to str_len - keyword_len do
    if String.sub str i keyword_len = keyword then
      res :=
        String.sub str
          (i + keyword_len + 1)
          (str_len - i - keyword_len - 1)
  done;
  !res

(* [str_to_msg str] converts the string [str] to a message, where [str]
   is a string sent between the client and server. *)
let str_to_msg str =
  (* NOTE: every time the msg type is updated, make sure to update the
     msg variable*)
  let dh_str = encrypted_str str "DIFFIE" in
  let lst = String.split_on_char ' ' str in
  process lst { empty_msg with dh_encrypted = dh_str }

(* [msg_to_str msg] converts msg [msg] into a string ready to be sent
   between the client and server *)
let msg_to_str msg =
  (* NOTE: update this as fields are added to msg *)
  "op=" ^ msg.op ^ " id=" ^ msg.id ^ " r=" ^ string_of_int msg.r
  ^ " mod_p=" ^ Z.to_string msg.mod_p ^ " prim_root_p="
  ^ Z.to_string msg.prim_root_p
  ^ " pub_key_client=" ^ msg.pub_key_client ^ " pub_key_server="
  ^ msg.pub_key_server ^ " DIFFIE=" ^ msg.dh_encrypted

let extract_pub_info msg =
  { mod_p = msg.mod_p; prim_root_p = msg.prim_root_p }

let dh_str_to_lst str = Str.split (Str.regexp "[BREAK_HERE]+") str

let dh_lst_to_str lst =
  List.fold_left (fun x y -> x ^ "BREAK_HERE" ^ y) "" lst
