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
   exchange and encryption*)

val create_dh_keys : pub_info -> keys
(**[create_dh_keys p] is a new private key public for diffie-hellman key
   exchange before the shared key is generated.*)

val create_dh_shared_key : keys -> string -> pub_info -> keys
(**[create_dh_keys k s p] is the keys [k] updated with the shared key
   generated using [s] as the partner's public key and p as the public
   info.*)

val encrypt_dh : keys -> bytes -> bytes
(**[encrypt_dh k b] is the bytes [b] encrypted using the diffie-hellman
   shared private key in [k]*)

val decrypt_dh : keys -> bytes -> bytes
(**[decrypt_dh k b] is the bytes [b] decrypted using the diffie-hellman
   shared private key in [k]*)

val create_rsa_keys : unit -> keys
(**[create_rsa_keys ()] is a new public key, private pair used for rsa
   encryption*)

val encrypt_rsa : string -> bytes -> bytes
(**[encrypt_rsa s b] is the bytes [b] encrypted using the public key [s]*)

val decrypt_rsa : keys -> bytes -> bytes
(**[decrypt_rsa k b] is the bytes [b] decrypted using the private key
   pair [k]*)

val id_gen : string