open OUnit2
open Yojson.Basic.Util
open Util.Msg
open Util.Db
open Util.Keys

let test (name : string) input expected_output : test =
  name >:: fun _ -> assert_equal expected_output input

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

let q_test (name : string) (count : int) f arbitrary =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~name ~count arbitrary (fun s -> s = f s))

let keys_tests' =
  [
    QCheck_runner.to_ounit2_test
      (QCheck.Test.make ~name:"DH Shared key is the same" ~count:10
         QCheck.unit (fun _ ->
           let p = dh_pub_info () in
           let k1 = create_dh_keys p in
           let k2 = create_dh_keys p in
           let pub1 = dh_get_public_key k1 in
           let pub2 = dh_get_public_key k2 in
           let k1 = create_dh_shared_key k1 pub2 p in
           let k2 = create_dh_shared_key k2 pub1 p in
           Z.to_string (snd k1.private_key)
           = Z.to_string (snd k2.private_key)));
    QCheck_runner.to_ounit2_test
      (QCheck.Test.make
         ~name:
           "ByteMatrix of_string then to_string keeps string the same."
         ~count:100
         QCheck.(string_of_size Gen.(16 -- 16))
         (fun s -> ByteMatrix.(s |> of_string |> to_string) = s));
    QCheck_runner.to_ounit2_test
      (QCheck.Test.make
         ~name:"ByteMatrix s_box then inv_s_box keeps string the same."
         ~count:100
         QCheck.(string_of_size Gen.(16 -- 16))
         (fun s ->
           ByteMatrix.(
             s |> of_string |> s_box |> inv_s_box |> to_string)
           = s));
    QCheck_runner.to_ounit2_test
      (QCheck.Test.make
         ~name:
           "ByteMatrix shift_rows then inv_shift_rows keeps string the \
            same."
         ~count:100
         QCheck.(string_of_size Gen.(16 -- 16))
         (fun s ->
           ByteMatrix.(
             s |> of_string |> shift_rows |> inv_shift_rows |> to_string)
           = s));
    QCheck_runner.to_ounit2_test
      (QCheck.Test.make
         ~name:
           "ByteMatrix mix_colum then inv_mix_column keeps string the \
            same."
         ~count:100
         QCheck.(string_of_size Gen.(16 -- 16))
         (fun s ->
           ByteMatrix.(
             s |> of_string |> mix_column |> inv_mix_column |> to_string)
           = s));
    QCheck_runner.to_ounit2_test
      (let p = dh_pub_info () in
       let k1 = create_dh_keys p in
       let k2 = create_dh_keys p in
       let pub1 = dh_get_public_key k1 in
       let pub2 = dh_get_public_key k2 in
       let k1 = create_dh_shared_key k1 pub2 p in
       let k2 = create_dh_shared_key k2 pub1 p in

       QCheck.Test.make
         ~name:"DH encrypt then decrypt keeps a string the same"
         ~count:10
         QCheck.(printable_string_of_size (Gen.int_bound 10000))
         (fun s ->
           s
           |> encrypt_dh (Z.to_string (snd k1.private_key))
           |> decrypt_dh (Z.to_string (snd k2.private_key))
           = s));
    QCheck_runner.to_ounit2_test
      (let p = dh_pub_info () in
       let k1 = create_dh_keys p in
       let k2 = create_dh_keys p in
       let pub1 = dh_get_public_key k1 in
       let pub2 = dh_get_public_key k2 in
       let k1 = create_dh_shared_key k1 pub2 p in
       let k2 = create_dh_shared_key k2 pub1 p in

       QCheck.Test.make
         ~name:
           "DH encrypt then decrypt using str breaking and \
            concatenating fctns keeps a string the same"
         ~count:0
         QCheck.(printable_string_of_size (Gen.int_bound 10000))
         (fun s ->
           s
           |> encrypt_dh (Z.to_string (snd k1.private_key))
           |> dh_lst_to_str |> dh_str_to_lst
           |> decrypt_dh (Z.to_string (snd k2.private_key))
           = s));
    QCheck_runner.to_ounit2_test
      (QCheck.Test.make
         ~name:"RSA encrypt then decrypt keeps a string the same"
         ~count:10
         QCheck.(printable_string_of_size (Gen.int_bound 10000))
         (fun s ->
           let k = create_rsa_keys () in
           let pub = rsa_get_public_key k in
           s |> encrypt_rsa pub |> decrypt_rsa k = s));
  ]
  @ test_s_box

let suite = "Testing" >::: List.flatten [ keys_tests' ]

let _ = run_test_tt_main suite

(* - encrypt str with encrypt_dh k s, which outputs a str list -
   concatenate strings in the str list the seperator "BREAK_HERE" using
   dh_list_to_str - send that string over - break that str back into a
   list with dh_str_to_lst - decrypt that string with decrypt_dh k s,
   which takes in a str list and outputs a str *)
