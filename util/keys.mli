module GaloisField : sig
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

module ByteMatrix : sig
  type t
  (**[t] is the type representing a 4x4 square matrix of bytes.*)

  val of_string : string -> t
  (**[of_string s] is a matrix of bytes made from the 16 character
     string [s].*)

  val to_string : t -> string
  (**[to_string t] is the byte matrix [t] flattened into a 16 character
     string column by column.*)

  val mul : t -> t -> t
  (**[mul a b] is the matrix multiplication [ab] in the Galois field
     GF(2^8).*)

  val s_box : t -> t
  (**[s_box t] is the matrix [t] with each entry permuted by the AES
     s-box.*)

  val inv_s_box : t -> t
  (**[inv_s_box t] is the matrix [t] with each entry permuted by the
     inverse s-box permutation.*)

  val shift_rows : t -> t
  (**[shift_rows t] is the matrix [t] with rows shifted by the AES shift
     rows operation.*)

  val inv_shift_rows : t -> t
  (**[inv_shift_rows t] is the matrix [t] with rows shifted by the
     inverse of the AES shift rows operation.*)

  val mix_column : t -> t
  (**[mix_column t] is the matrix [t] with colums mixed by the AES
     column mixing operation.*)

  val inv_mix_column : t -> t
  (**[inv_mix_column t] is the matrix [t] with colums mixed by the
     invers of the AES column mixing operation.*)
end

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

val encrypt_dh : string -> string -> string
(**[encrypt_dh k s] is the string [s] encrypted using AES in blocks in
   ECB mode with the diffie-hellman shared private key [k].*)

val decrypt_dh : string -> string -> string
(**[decrypt_dh k s] is the string [s] decrypted using AES in blocks in
   ECB mode with the diffie-hellman shared private key [k].*)

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
