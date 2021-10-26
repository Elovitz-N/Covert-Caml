type keys
(**[keys] is the type representing a private key, public key pair.*)

type pub_info
(**[pub_info] is the type representing the public info used to generate
   keys and to encrypt or decrypt.*)

val get_public_key : keys -> string
(**[get_public_key k] is the public key of the public key, private key
   pair [k] as a string.*)

val dh_pub_info : unit -> pub_info
(**[dh_pub_info ()] is a new public info used for diffie-hellman key
   exchange and encryption.*)

val create_dh_keys : pub_info -> keys
(**[create_dh_keys p] is a new private key, public key pair for
   diffie-hellman key exchange before the shared key is generated.*)

val create_dh_shared_key : keys -> string -> pub_info -> keys
(**[create_dh_keys k s p] is the keys [k] updated with the shared key
   generated using [s] as the partner's public key and p as the public
   info.*)

val encrypt_dh : keys -> bytes -> pub_info -> bytes
(**[encrypt_dh k b p] is the bytes [b] encrypted using the
   diffie-hellman shared private key in [k] and the public mod from [p].*)

val decrypt_dh : keys -> bytes -> pub_info -> bytes
(**[decrypt_dh k b] is the bytes [b] decrypted using the diffie-hellman
   shared private key in [k] and the public mod from [p].*)

val create_rsa_keys : unit -> keys
(**[create_rsa_keys ()] is a new public key, private pair used for rsa
   encryption.*)

val encrypt_rsa : string -> bytes -> bytes
(**[encrypt_rsa s b] is the bytes [b] encrypted using the public key
   [s].*)

val decrypt_rsa : keys -> bytes -> bytes
(**[decrypt_rsa k b] is the bytes [b] decrypted using the private key
   pair [k]*)

val id_gen : int -> string -> string
(** [id_gen n s] generates [n] random integers and returns the integers
    concatenated together with string [s] **)

val rand_int : string
(** [rand_int] returns a random integer. **)

val extract_op : string -> string
(** [extract_op str] returns the operation extracted from string [str].
    Raises "Invalid op string" if the string cannot be parsed. Requires:
    [str] is in the form "op=[val]..." **)

val extract_r : string -> string
(** [extract_r str] returns the random value "r" extracted from string
    [str]. Raises "Invalid r string" if the string cannot be parsed.
    Requires: [str] is in the form "op=[val] [id] r=[val] ..." **)
