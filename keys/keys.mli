type dh_keys = {
  private_key : Z.t * Z.t;
  public_key : Z.t;
}
(**[dh_keys] is the type representing a Diffie-Hellman private key,
   public key pair.*)

type pub_info = {
  mod_p : Z.t;
  prim_root_p : Z.t;
}
(**[pub_info] is the type representing the public info used to generate
   keys in Diffie Hellman.*)

val dh_get_public_key : dh_keys -> string
(**[dh_get_public_key k] is the public key of the Diffie-Hellamn public
   key, private key pair [k] as a string.*)

val dh_pub_info : unit -> pub_info
(**[dh_pub_info ()] is a new public info used for diffie-hellman key
   exchange and encryption.*)

val create_dh_keys : pub_info -> dh_keys
(**[create_dh_keys p] is a new private key, public key pair for
   diffie-hellman key exchange before the shared key is generated.*)

val create_dh_shared_key : dh_keys -> string -> pub_info -> dh_keys
(**[create_dh_keys k s p] is the keys [k] updated with the shared key
   generated using [s] as the partner's public key and p as the public
   info.*)

val encrypt_dh : dh_keys -> string -> string list
(**[encrypt_dh k s] is the string [s] encrypted in blocks using the
   diffie-hellman shared private key in [k]. Requires: [k] has had a
   shared key generated using [create_dh_shared_key].*)

val decrypt_dh : dh_keys -> string list -> string
(**[decrypt_dh k s] is the list of encrypted string blocks [s] decrypted
   using the diffie-hellman shared private key in [k]. Requires: [k] has
   had a shared key generated using [create_dh_shared_key].*)

type rsa_keys
(**[dh_keys] is the type representing an RSA private key, public key
   pair.*)

val rsa_get_public_key : rsa_keys -> string * string
(**[rsa_get_public_key k] is the RSA public key pair of the public key,
   private key pair [k] as a string tuple.*)

val create_rsa_keys : unit -> rsa_keys
(**[create_rsa_keys ()] is a new public key, private pair used for rsa
   encryption.*)

val encrypt_rsa : string * string -> string -> string list
(**[encrypt_rsa k s] is the string [s] encrypted using the RSA public
   key pair [k] into blocks smaller than the size of the public modulus.
   Requires: [k] is public key generated using [create_rsa_keys]*)

val id_gen : int -> string -> string
(** [id_gen n s] generates [n] random integers and returns the integers
    concatenated together with string [s] **)

val rand_int : int
(** [rand_int] returns a random integer. **)
val decrypt_rsa : rsa_keys -> string list -> string
(**[decrypt_rsa k s] is the list of encrypted string blocks [s]
   decrypted using the private key in [k].*)
