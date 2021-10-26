type msg = {
  op : string;
  id : string;
  r : int;
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
          | _ -> process t msg)
      | _ -> msg)
  | [] -> msg

(* [str_to_msg str] converts the string [str] to a message, where [str]
   is a string sent between the client and server. *)
let str_to_msg str =
  (* NOTE: every time the msg type is updated, make sure to update the
     msg variable*)
  let msg = { op = ""; id = ""; r = 0 } in
  let lst = String.split_on_char ' ' str in
  process lst msg

(* [msg_to_str msg] converts msg [msg] into a string ready to be sent
   between the client and server *)
let msg_to_str msg =
  "op=" ^ msg.op ^ " id=" ^ msg.id ^ " r=" ^ string_of_int msg.r

(* [extract_op str] returns the operation extracted from string [str].
   Raises "Invalid op string" if the string cannot be parsed. Requires:
   [str] is in the form "op=[val]..." *)
let extract_op str =
  match String.split_on_char ' ' str with
  | h :: t -> (
      match String.split_on_char '=' h with
      | x :: y :: z -> y
      | _ -> failwith "Invalid op string")
  | _ -> failwith "Invalid op string"

(* [extract_r str] returns the random value "r" extracted from string
   [str]. Raises "Invalid r string" if the string cannot be parsed.
   Requires: [str] is in the form "op=[val] [id] r=[val] ..." *)
let extract_r str =
  match String.split_on_char '=' str with
  | x :: y :: z :: e -> (
      match String.split_on_char ' ' z with
      | h :: t -> h
      | _ -> failwith "Invalid r string")
  | _ -> failwith "Invalid r string"