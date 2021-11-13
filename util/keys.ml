(**[pow a b] is [a] to the power of [b]. Requires: [b >= 0].*)
let rec pow a b = match b with 0 -> 1 | n -> a * pow a (b - 1)

module type GaloisField = sig
  type t
  (**[t] is the type of an element of a Galois field.*)

  val add : t -> t -> t
  (**[add a b] is the sum of [a] and [b] in the field.*)

  val p : t
  (**[p] is the irreducible polynomial in the field used for
     multiplication. It should not be used as an element of the field
     for computation.*)

  val mul : t -> t -> t
  (**[mul a b] is [a] multiplied by [b] in the field using [p] as the
     mod.*)

  val of_char : char -> t
  (**[of_char c] is the unique element of GF(2^8) associated with any
     character [c].*)

  val to_char : t -> char
  (**[to_char t] is the unique ascii character associated with the
     element of GF(2^8) [t]*)

  val s_box : t -> t
  (**[s_box t] is an element of the field GF(2^8) permuted by the
     conversion s_box table.*)

  val inv_s_box : t -> t
  (**[inv_s_box t] is the inverse permutation of [s_box t].*)
end

module GF256 : GaloisField = struct
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
      if a = 0 then 0
      else
        let x = pow 2 (max_pow b) in
        Int.logxor (x * a) (mul_aux x (b - x))
    in
    let a_x_b = mul_aux a b in
    let rec mod_p x =
      if x < 256 then x
      else mod_p (Int.logxor (Int.shift_left p (max_pow x - 8)) x)
    in
    mod_p a_x_b

  let of_char c = Char.code c

  let to_char t = Char.chr t

  let s_box = failwith "Unimplemented: s_box"

  let inv_s_box = failwith "Unimplemented: inv_s_box"
end

module type ByteMatrix = sig
  type t
  (**[t] is the type representing a 4x4 square matrix of bytes.*)

  val of_string : string -> t
  (**[of_string s] is a matrix of bytes made from the 16 character
     string [s].*)

  val to_string : t -> string
  (**[to_string t] is the byte matrix [t] flattened into a 16 character
     string column by column.*)

  val mul : t -> t -> t
  (**[mul a b] is the matrix multiplication of [a] and [b] in the Galois
     field GF(2^8).*)

  val s_box : t -> t

  val inv_s_box : t -> t

  val shift_rows : t -> t

  val inv_shift_rows : t -> t

  val mix_column : t -> t

  val inv_mix_column : t -> t
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

module ByteMatrixImp = struct
  type t = GF256.t array array

  let of_string s =
    let a = Array.make_matrix 4 4 "\000" in
    Array.mapi
      (fun i x ->
        Array.mapi
          (fun j y ->
            let index = (4 * i) + j in
            s.[index] |> GF256.of_char)
          x)
      a

  let to_string t =
    Array.fold_left
      (fun acc1 x ->
        acc1 ^ Array.fold_left (fun acc2 y -> acc2 ^ y) "" x)
      "" t
end

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
