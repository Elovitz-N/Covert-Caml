open OUnit2
open Yojson.Basic.Util
open Keys

let test (name : string) input expected_output : test =
  name >:: fun _ -> assert_equal expected_output input

let keys_tests = []

let suite = "Testing" >::: List.flatten [ keys_tests ]

let _ = run_test_tt_main suite
