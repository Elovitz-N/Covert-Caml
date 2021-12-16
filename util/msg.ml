open Keys

let found = ref false

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

type info = {
  id : string;
  dh_key : Z.t;
}

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
    uname = "";
    password = "";
    reciever = "";
    message = "";
  }

let rec p_m t msg y =
  process t
    {
      msg with
      message =
        List.fold_left
          (fun acc x ->
            if String.contains x '=' || !found then
              let _ = found := true in
              acc
            else acc ^ " " ^ x)
          y t;
    }

(* Note: this function is longer than 20 lines, but that is because of
   the pattern matching possibilities. Helper functions in this case
   would not simplify the code. *)
and process lst msg =
  found := false;
  match lst with
  | h :: t -> (
      match String.split_on_char '=' h with
      | x :: y :: e -> (
          match x with
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
          | "uname" -> process t { msg with uname = y }
          | "password" -> process t { msg with password = y }
          | "reciever" -> process t { msg with reciever = y }
          | "message" -> p_m t msg y
          | _ -> process t msg)
      | _ -> msg)
  | [] -> msg

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

let str_to_msg str =
  let dh_str = encrypted_str str "DIFFIE" in
  let lst = String.split_on_char ' ' str in
  process lst { empty_msg with dh_encrypted = dh_str }

let msg_to_str msg =
  "op=" ^ msg.op ^ " id=" ^ msg.id ^ " r=" ^ string_of_int msg.r
  ^ " mod_p=" ^ Z.to_string msg.mod_p ^ " prim_root_p="
  ^ Z.to_string msg.prim_root_p
  ^ " pub_key_client=" ^ msg.pub_key_client ^ " pub_key_server="
  ^ msg.pub_key_server ^ " uname=" ^ msg.uname ^ " password="
  ^ msg.password ^ " reciever=" ^ msg.reciever ^ " message="
  ^ msg.message ^ " DIFFIE=" ^ msg.dh_encrypted

let extract_pub_info msg =
  { mod_p = msg.mod_p; prim_root_p = msg.prim_root_p }

let dh_str_to_lst str = Str.split (Str.regexp "[BREAK_HERE]+") str

let dh_lst_to_str lst =
  List.fold_left (fun x y -> x ^ "BREAK_HERE" ^ y) "" lst

let get_key id (lst : info list) =
  let res = List.filter (fun (x : info) -> x.id = id) lst |> List.hd in
  Z.to_string res.dh_key
