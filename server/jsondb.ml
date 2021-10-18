open Yojson.Basic.Util

type user = {
  name : string;
  password : string;
}

type t = {
  users : user list;
  test_data: string;
}

let user_of_json j =
  {
    name = j |> member "id" |> to_string;
    password = j |> member "password" |> to_string;
  }

let t_of_json j =
  {
    users = j |> member "users" |> to_list |> List.map user_of_json;
    test_data = j |> member "test_data" |> to_string;
  }

let from_json json = try t_of_json json with
  | Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let first_user j = j.test_data

