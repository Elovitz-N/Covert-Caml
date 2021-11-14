(**[pow a b] is [a] to the power of [b]. Requires: [b >= 0].*)
let rec pow a b = match b with 0 -> 1 | n -> a * pow a (b - 1)

module GaloisField = struct
  type t = int
  (**AF: an element of the Galois field GF(2^8) is represented by the
     decimal int of the binary number created from its coefficients.
     Example: [x^6+x^3+x+1] is [01001011] as the binary representation
     of its coefficients which is [75] in decimal. RI: it is a positive
     int and except for [p], it is between 0 and 255.*)

  let add a b = Int.logxor a b

  let p = 283

  (**[max_pow x] is the highest number n such that 2^n is less than [x].*)
  let rec max_pow x = if x <= 1 then 0 else 1 + max_pow (x / 2)

  let mul a b =
    let rec mul_aux a b =
      if b = 0 then 0
      else
        let x = pow 2 (max_pow b) in
        Int.logxor (x * a) (mul_aux a (b - x))
    in
    let a_x_b = mul_aux a b in
    let rec mod_p x =
      if x < 256 then x
      else mod_p (Int.logxor (Int.shift_left p (max_pow x - 8)) x)
    in
    mod_p a_x_b

  let of_char c = Char.code c

  let to_char t = Char.chr t

  let s_box_tble =
    [| [|
    99; 202; 183; 4; 9; 83; 208; 81; 205; 96; 224; 231; 186; 112; 225; 140;
    |]; [|
    124; 130; 253; 199; 131; 209; 239; 163; 12; 129; 50; 200; 120; 62; 248; 161;
    |]; [|
    119; 201; 147; 35; 44; 0; 170; 64; 19; 79; 58; 55; 37; 181; 152; 137;
    |]; [|
    123; 125; 38; 195; 26; 237; 251; 143; 236; 220; 10; 109; 46; 102; 17; 13;
    |]; [|
    242; 250; 54; 24; 27; 32; 67; 146; 95; 34; 73; 141; 28; 72; 105; 191
    |]; [|
    107; 89; 63; 150; 110; 252; 77; 157; 151; 42; 6; 213; 166; 3; 217; 230
    |]; [|
    111; 71; 247; 5; 90; 177; 51; 56; 68; 144; 36; 78; 180; 246; 142; 66
    |]; [|
    197; 240; 204; 154; 160; 91; 133; 245; 23; 136; 92; 169; 198; 14; 148; 104
    |]; [|
    48; 173; 52; 7; 82; 106; 69; 188; 196; 70; 194; 108; 232; 97; 155; 65
    |]; [|
    1; 212; 165; 18; 59; 203; 249; 182; 167; 238; 211; 86; 221; 53; 30; 153
    |]; [|
    103; 162; 229; 128; 214; 190; 2; 218; 126; 184; 172; 244; 116; 87; 135; 45 
    |]; [|
    43; 175; 241; 226; 179; 57; 127; 33; 61; 20; 98; 234; 31; 185; 233; 15
    |]; [|
    254; 156; 113; 235; 41; 74; 80; 16; 100; 222; 145; 101; 75; 134; 206; 176
    |]; [|
    215; 164; 216; 39; 227; 76; 60; 255; 93; 94; 149; 122; 189; 193; 85; 84
    |]; [|
    171; 114; 49; 178; 47; 88; 159; 243; 25; 11; 228; 174; 139; 29; 40; 187
    |]; [|
    118; 192; 21; 117; 132; 207; 168; 210; 115; 219; 121; 8; 138; 158; 223; 22
    |] |] [@@ocamlformat "disable"]

  let s_box t = s_box_tble.(t mod 16).(t / 16)

  let inv_s_box_tble = 
    [| [|
    82; 124; 84; 8; 114; 108; 144; 208; 58; 150; 71; 252; 31; 96; 160; 23
    |]; [|
    9; 227; 123; 46; 248; 112; 216; 44; 145; 172; 241; 86; 221; 81; 224; 43
    |]; [|
    106; 57; 148; 161; 246; 72; 171; 30; 17; 116; 26; 62; 168; 127; 59; 4
    |]; [|
    213; 130; 50; 102; 100; 80; 0; 143; 65; 34; 113; 75; 51; 169; 77; 126
    |]; [|
    48; 155; 166; 40; 134; 253; 140; 202; 79; 231; 29; 198; 136; 25; 174; 186
    |]; [|
    54; 47; 194; 217; 104; 237; 188; 63; 103; 173; 41; 210; 7; 181; 42; 119
    |]; [|
    165; 255; 35; 36; 152; 185; 211; 15; 220; 53; 197; 121; 199; 74; 245; 214
    |]; [|
    56; 135; 61; 178; 22; 218; 10; 2; 234; 133; 137; 32; 49; 13; 176; 38
    |]; [|
    191; 52; 238; 118; 212; 94; 247; 193; 151; 226; 111; 154; 177; 45; 200; 225
    |]; [|
    64; 142; 76; 91; 164; 21; 228; 175; 242; 249; 183; 219; 18; 229; 235; 105
    |]; [|
    163; 67; 149; 162; 92; 70; 88; 189; 207; 55; 98; 192; 16; 122; 187; 20
    |]; [|
    158; 68; 11; 73; 204; 87; 5; 3; 206; 232; 14; 254; 89; 159; 60; 99
    |]; [|
    129; 196; 66; 109; 93; 167; 184; 1; 240; 28; 170; 120; 39; 147; 131; 85
    |]; [|
    243; 222; 250; 139; 101; 141; 179; 19; 180; 117; 24; 205; 128; 201; 83; 33
    |]; [|
    215; 233; 195; 209; 182; 157; 69; 138; 230; 223; 190; 90; 236; 156; 153; 12
    |]; [|
    251; 203; 78; 37; 146; 132; 6; 107; 115; 110; 27; 244; 95; 239; 97; 125
    |] |] [@@ocamlformat "disable"]

  let inv_s_box t = inv_s_box_tble.(t mod 16).(t / 16)
end

module ByteMatrix = struct
  type t = GaloisField.t array array

  let of_string s =
    let a = Array.make_matrix 4 4 0 in
    Array.mapi
      (fun i x ->
        Array.mapi
          (fun j y ->
            let index = (4 * i) + j in
            s.[index] |> GaloisField.of_char)
          x)
      a

  let to_string t =
    Array.fold_left
      (fun acc1 x ->
        acc1
        ^ Array.fold_left
            (fun acc2 y ->
              acc2 ^ (y |> GaloisField.to_char |> String.make 1))
            "" x)
      "" t

  let s_box = Array.map (Array.map GaloisField.s_box)

  let inv_s_box = Array.map (Array.map GaloisField.inv_s_box)

  let mul a b =
    let mul_aux a v i =
      GaloisField.(
        mul v.(0) a.(0).(i)
        |> add (mul v.(1) a.(1).(i))
        |> add (mul v.(2) a.(2).(i))
        |> add (mul v.(3) a.(3).(i)))
    in
    let new_matrix = Array.make_matrix 4 4 0 in
    Array.mapi
      (fun i x -> Array.mapi (fun j y -> mul_aux a b.(i) j) x)
      new_matrix

  (**[shift_row t m n] shifts row [m] of matrix [t] [n] steps to the
     left. Requires: m is from 0 to 3.*)
  let shift_row t m n =
    let shift_row_left t i =
      let placeholder = t.(0).(i) in
      t.(0).(i) <- t.(1).(i);
      t.(1).(i) <- t.(2).(i);
      t.(2).(i) <- t.(3).(i);
      t.(3).(i) <- placeholder
    in
    for i = 1 to n do
      shift_row_left t m
    done

  let shift_rows t =
    shift_row t 1 1;
    shift_row t 2 2;
    shift_row t 3 3;
    t

  let inv_shift_rows t =
    shift_row t 1 3;
    shift_row t 2 2;
    shift_row t 3 1;
    t

  let mix_column t =
    mul
      [|
        [| 2; 1; 1; 3 |];
        [| 3; 2; 1; 1 |];
        [| 1; 3; 2; 1 |];
        [| 1; 1; 3; 2 |];
      |]
      t

  let inv_mix_column t =
    mul
      [|
        [| 14; 9; 13; 11 |];
        [| 11; 14; 9; 13 |];
        [| 13; 11; 14; 9 |];
        [| 9; 13; 11; 14 |];
      |]
      t
end

(**[split_string n s] is a list consisting of [s] split into
   [n]-character strings. If it doesn't split evenly then the last
   string is filled with the null character ['\000']. Example:
   [split_string 8 "01234567890123456789"] is
   [\["01234567";"89012345";"6789\000\000\000\000"\]] *)
let rec split_string n s =
  if s = "" then []
  else
    let block_length = min (String.length s) n in
    (String.sub s 0 block_length ^ String.make (n - block_length) '\000')
    :: split_string n
         (String.sub s block_length
            (max (String.length s - block_length) 0))

(**[rand_prime n] is a random prime of at least n bits.*)
let rand_prime n =
  (*[rand n] is a random positive integer of size n bits.*)
  let rand n =
    Random.self_init ();
    let rec big_string_int m =
      if m <= 0 then ""
      else
        string_of_int (Random.int (pow 2 (min m 30) - 1))
        ^ big_string_int (m - 30)
    in
    Z.of_string (big_string_int n)
  in
  Z.nextprime (rand n)

type dh_keys = {
  private_key : Z.t * Z.t;
  public_key : Z.t;
}

let rec id_gen n s =
  Random.self_init ();
  if n = 0 then s
  else id_gen (n - 1) (s ^ (Random.int 1073741823 |> string_of_int))

let rand_int =
  Random.self_init ();
  Random.int 1073741823

type pub_info = {
  mod_p : Z.t;
  prim_root_p : Z.t;
}

let dh_get_public_key k = Z.to_string k.public_key

(**[gen_p q] is a tuple of an arbitrary precision prime p where [p-1]
   has [q] as a factor, and the dividend of [p-1] and [q].*)
let gen_p q =
  let rec gen_p_tr q acc =
    let p = Z.(succ (q * acc)) in
    if Z.probab_prime p 7 > 0 then (p, acc) else gen_p_tr q Z.(succ acc)
  in
  gen_p_tr q Z.one

(**[prime_factors n] is a list of the prime factors of the arbitrary
   precision integer [n].*)
let prime_factors n =
  let rec aux p n =
    if Z.(equal n one) then []
    else if Z.divisible n p then p :: aux Z.(of_int 2) Z.(divexact n p)
    else aux Z.(nextprime p) n
  in
  List.sort_uniq Z.compare (aux Z.(of_int 2) n)

let dh_pub_info () =
  let q = rand_prime 1024 in
  let mod_p = gen_p q in
  (*[gen_prim_root] generates a primitive root in modulo [p].*)
  let rec gen_prim_root q p acc =
    let divisor_lst = prime_factors (snd p) in
    if
      List.filter
        (fun x -> Z.(equal (powm acc (divexact (fst p) x) (fst p)) one))
        divisor_lst
      = []
    then acc
    else gen_prim_root q p (Z.succ acc)
  in
  let prim_root_p = gen_prim_root q mod_p Z.one in
  { mod_p = fst mod_p; prim_root_p }

let create_dh_keys pub_info =
  let my_key = rand_prime 128 in
  {
    private_key = (my_key, Z.zero);
    public_key = Z.powm pub_info.prim_root_p my_key pub_info.mod_p;
  }

let create_dh_shared_key keys their_key pub_key =
  print_string "\n Creating DH shared key \n";
  print_newline ();

  let their_key = Z.of_string their_key in
  let _ =
    print_string ("\n their key is " ^ Z.to_string their_key ^ "\n")
  in
  let shared_key =
    Z.powm their_key (fst keys.private_key) pub_key.mod_p
  in
  let _ =
    print_string ("\n shared key is " ^ Z.to_string shared_key ^ "\n\n")
  in
  { keys with private_key = (fst keys.private_key, shared_key) }

(**[trim_string s] is [s] with all characters [\000] removed from the
   end.*)
let rec trim_string s =
  if Str.last_chars s 1 = "\000" then
    trim_string (Str.string_before s (String.length s - 1))
  else s

let encrypt_dh k s =
  let shared_key = Z.of_string k in
  let plain_txt = split_string (Z.numbits shared_key / 8) s in
  List.map
    (fun x -> Z.(to_string (of_bits x lxor shared_key)))
    plain_txt

let decrypt_dh k s =
  let shared_key = Z.of_string k in
  let plain_txt =
    List.map (fun x -> Z.(to_bits (of_string x lxor shared_key))) s
  in
  List.fold_left (fun x y -> x ^ trim_string y) "" plain_txt

type rsa_keys = {
  private_key : Z.t;
  public_key : Z.t * Z.t;
}

let rsa_get_public_key k =
  (Z.to_string (fst k.public_key), Z.to_string (snd k.public_key))

let create_rsa_keys () =
  let p = rand_prime 1536 in
  let q = rand_prime 1536 in
  let n = Z.mul p q in
  let phi_n = Z.((p - one) * (q - one)) in
  let rec gen_e acc =
    if Z.gcd acc phi_n = Z.one then acc
    else gen_e (acc |> Z.succ |> Z.succ)
  in
  let e = gen_e Z.(of_int 3) in
  let d = Z.invert e phi_n in
  { private_key = d; public_key = (e, n) }

let encrypt_rsa k s =
  let exp = Z.of_string (fst k) in
  let modulo = Z.of_string (snd k) in
  let plain_txt = split_string (Z.numbits modulo / 8) s in
  List.map
    (fun x -> Z.(to_string (powm (of_bits x) exp modulo)))
    plain_txt

let decrypt_rsa k s =
  let exp = k.private_key in
  let modulo = snd k.public_key in
  let plain_txt =
    List.map (fun x -> Z.(to_bits (powm (of_string x) exp modulo))) s
  in
  List.fold_left (fun x y -> x ^ trim_string y) "" plain_txt
