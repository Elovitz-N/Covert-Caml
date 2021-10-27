(**[pow a b] is [a] to the power of [b]. Requires: [b >= 0].*)
let rec pow a b = match b with 0 -> 1 | n -> a * pow a (b - 1)

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

type pub_info = {
  mod_p : Z.t;
  prim_root_p : Z.t;
}

let dh_get_public_key k = Z.to_bits k.public_key

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
  let their_key = Z.of_bits their_key in
  let shared_key =
    Z.powm their_key (fst keys.private_key) pub_key.mod_p
  in
  { keys with private_key = (fst keys.private_key, shared_key) }

(**[split_string s n] is a list consisting of [s] split into
   [n]-character strings except for the last element which may be fewer
   characters. Example: [split_string "01234567890123456789" 16] is
   [\["0123456789012345";"6789"\]] *)
let rec split_string s n =
  if s = "" then []
  else
    let block_length = min (String.length s) n in
    String.sub s 0 block_length
    :: split_string
         (String.sub s block_length
            (max (String.length s - block_length) 0))
         n

(**[trim_string s] is [s] with all characters [\000] removed from the
   end.*)
let rec trim_string s =
  if Str.last_chars s 1 = "\000" then
    trim_string (Str.string_before s (String.length s - 1))
  else s

let encrypt_dh k s =
  let shared_key = snd k.private_key in
  let plain_txt = split_string s (Z.numbits shared_key / 8) in
  List.map (fun x -> Z.(to_bits (of_bits x lxor shared_key))) plain_txt

let decrypt_dh k s =
  let shared_key = snd k.private_key in
  let plain_txt =
    List.map (fun x -> Z.(to_bits (of_bits x lxor shared_key))) s
  in
  List.fold_left (fun x y -> x ^ trim_string y) "" plain_txt

type rsa_keys = {
  private_key : Z.t;
  public_key : Z.t * Z.t;
}

let rsa_get_public_key k =
  (Z.to_bits (fst k.public_key), Z.to_bits (snd k.public_key))

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
  let exp = Z.of_bits (fst k) in
  let modulo = Z.of_bits (snd k) in
  let plain_txt = split_string s (Z.numbits modulo / 8) in
  List.map
    (fun x -> Z.(to_bits (powm (of_bits x) exp modulo)))
    plain_txt

let decrypt_rsa k s =
  let exp = k.private_key in
  let modulo = snd k.public_key in
  let plain_txt =
    List.map (fun x -> Z.(to_bits (powm (of_bits x) exp modulo))) s
  in
  List.fold_left (fun x y -> x ^ trim_string y) "" plain_txt
