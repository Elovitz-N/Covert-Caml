(*(**[logged_in ()] prompts the user for their username then password
  and returns whether they match the stored data*) let logged_in () =
  print_endline "Username:"; if query_input () = inst_data.username then
  begin print_endline "Password:"; query_input () = inst_data.username
  end else false

  let query_data () = { username = ""; password = "" }

  (* module InstClient (M: Client) = struct include M let inst_data =
  inst_data () end*) let greeting = "Welcome to CovertCaml."

  let client () = print_endline greeting; if logged_in () then () else
  print_endline "Set username/password Y/N?"; match query_input () with
  "Y" -> () | "N" -> exit 0 | _ -> () ;; client ()*)

open Client

(**[query_input ()] is the string of a user input.*)
let query_input () =
  match read_line () with exception End_of_file -> exit 0 | c -> c

let main () =
  print_endline "Connect to host:";
  connect (query_input ());
  print_endline "Send or Recieve?";
  match query_input () with
  | "Send" -> send (query_input ())
  | "Recieve" -> print_string (recieve ())
  | _ -> exit 0
;;

main ()