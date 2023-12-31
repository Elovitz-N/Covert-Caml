open OUnit2
open Yojson.Basic.Util
open Util.Msg
open Util.Db
open Util.Keys

let test (name : string) input expected_output : test =
  name >:: fun _ -> assert_equal expected_output input

(**[test_many f n] is a list of [n] copies of the test [f].*)
let rec test_many f n = if n = 0 then [] else f :: test_many f (n - 1)

(**[rand_string ()] is a random string of fewer than 10000 characters
   made up of characters with ASCII 1 to 127.*)
let rand_string () =
  let length = Random.int 10000 in
  String.init length (fun _ -> Random.int 127 + 1 |> Char.chr)

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
    100
  @ test_many
      (let p = dh_pub_info () in
       let k1 = create_dh_keys p in
       let k2 = create_dh_keys p in
       let pub1 = dh_get_public_key k1 in
       let pub2 = dh_get_public_key k2 in
       let k1 = create_dh_shared_key k1 pub2 p in
       let k2 = create_dh_shared_key k2 pub1 p in
       let s = rand_string () in
       test "DH encrypt then decrypt keeps a string the same"
         (s
         |> encrypt_dh (Z.to_string (snd k1.private_key))
         |> decrypt_dh (Z.to_string (snd k2.private_key)))
         s)
      100
  @ test_many
      (let p = dh_pub_info () in
       let k1 = create_dh_keys p in
       let k2 = create_dh_keys p in
       let pub1 = dh_get_public_key k1 in
       let pub2 = dh_get_public_key k2 in
       let k1 = create_dh_shared_key k1 pub2 p in
       let k2 = create_dh_shared_key k2 pub1 p in
       let s = rand_string () in
       test
         "DH encrypt then decrypt using str breaking and concatenating \
          fctns keeps a string the same"
         (s
         |> encrypt_dh (Z.to_string (snd k1.private_key))
         |> dh_lst_to_str |> dh_str_to_lst
         |> decrypt_dh (Z.to_string (snd k2.private_key)))
         s)
      100
  @ test_many
      (let k = create_rsa_keys () in
       let pub = rsa_get_public_key k in
       let s = rand_string () in
       test "RSA encrypt then decrypt keeps a string the same"
         (s |> encrypt_rsa pub |> decrypt_rsa k)
         s)
      100

let suite = "Testing" >::: List.flatten [ keys_tests ]

let _ = run_test_tt_main suite

(* - encrypt str with encrypt_dh k s, which outputs a str list -
   concatenate strings in the str list the seperator "BREAK_HERE" using
   dh_list_to_str - send that string over - break that str back into a
   list with dh_str_to_lst - decrypt that string with decrypt_dh k s,
   which takes in a str list and outputs a str *)
