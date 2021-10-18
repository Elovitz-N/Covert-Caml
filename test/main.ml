open OUnit2
open Yojson.Basic.Util
open Database
open Jsondb

let users = Yojson.Basic.from_file "data/users.json"
let users_t = from_json users

let users_test
    (name : string)
    (adv : Jsondb.t)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (first_user adv)

  let database_tests =
    [
      users_test "Get the first user" users_t
        "test_data";
    ]

    let suite =
      "Testing Database"
      >::: List.flatten [ database_tests;]
    
    let _ = run_test_tt_main suite