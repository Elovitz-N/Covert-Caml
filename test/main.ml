open OUnit2
open Yojson.Basic.Util
open Keys

let test (name : string) input expected_output : test =
  name >:: fun _ -> assert_equal expected_output input

let rec test_many f n = if n = 0 then [] else f :: test_many f (n - 1)

let keys_tests =
  test_many
    (let p = dh_pub_info () in
     let k1 = create_dh_keys p in
     let k2 = create_dh_keys p in
     let pub1 = dh_get_public_key k1 in
     let pub2 = dh_get_public_key k2 in
     let k1 = create_dh_shared_key k1 pub2 p in
     let k2 = create_dh_shared_key k2 pub1 p in
     test "DH Shared key is the same"
       (Z.to_string (snd k1.private_key))
       (Z.to_string (snd k2.private_key)))
    50
  @ test_many
      (let p = dh_pub_info () in
       let k1 = create_dh_keys p in
       let k2 = create_dh_keys p in
       let pub1 = dh_get_public_key k1 in
       let pub2 = dh_get_public_key k2 in
       let k1 = create_dh_shared_key k1 pub2 p in
       let k2 = create_dh_shared_key k2 pub1 p in
       let s = string_of_int (Random.bits ()) in
       test "DH encrypt then decrypt keeps a string the same"
         (s |> encrypt_dh k1 |> decrypt_dh k2)
         s)
      50
  @ test_many
      (let k = create_rsa_keys () in
       let pub = rsa_get_public_key k in
       let s = string_of_int (Random.bits ()) in
       test "RSA encrypt then decrypt keeps a string the same"
         (s |> encrypt_rsa pub |> decrypt_rsa k)
         s)
      50

let suite = "Testing" >::: List.flatten [ keys_tests ]

let _ = run_test_tt_main suite
