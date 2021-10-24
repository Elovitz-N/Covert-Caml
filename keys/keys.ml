(*[rand n] is a random positive integer of size ciel(m/30) bits.*)
let rand_prime n =
  let rand n =
    Random.self_init ();
    let rec big_string_int m =
      if m <= 0 then ""
      else string_of_int (Random.bits ()) ^ big_string_int (m - 30)
    in
    Z.of_string (big_string_int n)
  in
  Z.nextprime (rand n)

type keys = {
  private_key : Z.t * Z.t;
  public_key : Z.t;
}

type pub_info = {
  mod_p : Z.t;
  prim_root_p : Z.t;
}

let get_public_key k = Z.to_string k.public_key

(**[gen_p q] is a arbitrary precision prime p where [p-1] has [q] as a
   factor.*)
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
    else if Z.divisible n p then p :: aux Z.(succ one) Z.(divexact n p)
    else aux Z.(nextprime p) n
  in
  List.sort_uniq Z.compare (aux Z.(succ one) n)

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
  let their_key = Z.of_string their_key in
  let shared_key =
    Z.powm their_key (fst keys.private_key) pub_key.mod_p
  in
  { keys with private_key = (fst keys.private_key, shared_key) }

let encrypt_dh k b p = Z.(snd k.private_key * b mod p.mod_p)

let decrypt_dh k b p =
  Z.(b * invert (snd k.private_key) p.mod_p mod p.mod_p)

let create_rsa_keys () =
  let p = rand_prime 2048 in
  let q = rand_prime 2048 in
  { private_key = (p, q); public_key = Z.mul p q }

let encrypt_rsa s b = failwith "Not Implemented"

let decrypt_rsa k b = failwith "Not Implemented"
