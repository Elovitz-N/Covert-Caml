open OUnit2
open Yojson.Basic.Util
open Util.Msg
open Util.Db
open Util.Keys

let test (name : string) input expected_output : test =
  name >:: fun _ -> assert_equal expected_output input

(**[test_many t n] is a list of [n] copies of the test [t].*)
let rec test_many t n = if n = 0 then [] else t :: test_many t (n - 1)

let test_s_box =
  let rec test_s_box_aux n =
    if n = -1 then []
    else
      test
        ("Failed s-box and inverse stays the same for: "
       ^ string_of_int n)
        n
        GaloisField.(
          Char.chr n |> of_char |> s_box |> inv_s_box |> to_char
          |> Char.code)
      :: test_s_box_aux (n - 1)
  in
  test_s_box_aux 255

(**[rand_string ()] is a random string of fewer than 10000 characters
   made up of characters with ASCII 1 to 127.*)
let rand_string ?printable:(p = true) ?rand_length:(r = true) n =
  let length = if p then Random.int n else n in
  String.init length (fun _ ->
      (if r then Random.int 95 + 32 else Random.int 255) |> Char.chr)

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
       let s = rand_string 10000 in
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
       let s = rand_string 10000 in
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
       let s = rand_string 10000 in
       test "RSA encrypt then decrypt keeps a string the same"
         (s |> encrypt_rsa pub |> decrypt_rsa k)
         s)
      100
  @ test_s_box
  @ test_many
      (let s = rand_string ~printable:false ~rand_length:false 16 in
       test "ByteMatrix of_string then to_string keeps string the same."
         ByteMatrix.(s |> of_string |> to_string)
         s)
      100
  @ test_many
      (let s = rand_string ~printable:false ~rand_length:false 16 in
       test "ByteMatrix s_box then inv_s_box keeps string the same."
         ByteMatrix.(s |> of_string |> s_box |> inv_s_box |> to_string)
         s)
      100
  @ test_many
      (let s = rand_string ~printable:false ~rand_length:false 16 in
       test
         "ByteMatrix shift_rows then inv_shift_rows keeps string the \
          same."
         ByteMatrix.(
           s |> of_string |> shift_rows |> inv_shift_rows |> to_string)
         s)
      100
  @ test_many
      (let s = rand_string ~printable:false ~rand_length:false 16 in
       test
         "ByteMatrix mix_colum then inv_mix_column keeps string the\n\
         \  same."
         ByteMatrix.(
           s |> of_string |> mix_column |> inv_mix_column |> to_string)
         s)
      100

let suite = "Testing" >::: List.flatten [ keys_tests ]

let _ = run_test_tt_main suite

(* - encrypt str with encrypt_dh k s, which outputs a str list -
   concatenate strings in the str list the seperator "BREAK_HERE" using
   dh_list_to_str - send that string over - break that str back into a
   list with dh_str_to_lst - decrypt that string with decrypt_dh k s,
   which takes in a str list and outputs a str *)
