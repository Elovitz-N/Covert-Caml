type user = {
  id: string; 
  pass: string; 
  token: string; 
}

type chat = {
  id: string;
  user_list: user list; 
  messages: (user * string) list; 
}

type application = {
  chats: chat list
}

(** function that retrives the user's name *)
let get_user_id user = user.id

(** function that retrives the user's password *)
let get_user_pass user = user.pass

(** function that retrives the user's token *)
let get_user_token user = user.token

(** function that gets all chats *)
let get_chats app = app.chats

(** function that gets a specific chat by id *)
let rec get_chat_by_id chats id = match chats with 
| h :: t -> if h.id = id then Some h else get_chat_by_id t id
| _ -> None

(** function that gets the user list for a chat*)
let get_chat_user_list chat = chat.user_list

(** function that checks if a user is in the chat*)
let rec check_chat_user user_list user = match user_list with 
| h :: t -> if h.id = user.id then true else check_chat_user t user
| _ -> false 




