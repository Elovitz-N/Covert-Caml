let rand_prime n =
  (*[rand n] is a random positive integer of size ciel(m/30) bits.*)
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

type pub_info = Z.t * Z.t

let get_public_key k = Z.to_string k.public_key

let dh_pub_info () = failwith "Not Implemented"

let create_dh_keys pub_info =
  let my_key = rand_prime 128 in
  {
    private_key = (my_key, Z.zero);
    public_key = Z.powm (snd pub_info) my_key (fst pub_info);
  }

let create_dh_shared_key keys their_key pub_key =
  let their_key = Z.of_string their_key in
  let shared_key =
    Z.powm their_key (fst keys.private_key) (fst pub_key)
  in
  { keys with private_key = (fst keys.private_key, shared_key) }

let encrypt_dh k b = failwith "Not Implemented"

let decrypt_dh k b = failwith "Not Implemented"

let create_rsa_keys () =
  let p = rand_prime 2048 in
  let q = rand_prime 2048 in
  { private_key = (p, q); public_key = Z.mul p q }

let encrypt_rsa s b = failwith "Not Implemented"

let decrypt_rsa k b = failwith "Not Implemented"
