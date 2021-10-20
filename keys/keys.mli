(* module BigNum : sig type t (**[t] represents a large number*)

   val add : t -> t -> t (**[add x y] is the sum of two big numbers [x]
   and [y].*)

   val subtract : t -> t -> t (**[subtract x y] is the difference of tje
   big numbers [x] - [y].*)

   val is_greater : t -> t -> bool (**[is_greater x y] is whether [x] is
   greater than [y].*)

   val multiply : t -> t -> t (**[multiply x y] is the product of two
   big numbers.*)

   val modulo : t -> t -> t (**[modulo x y] is the remainder when
   dividing x by y.*)

   val rand : int -> int -> t (**[rand n m] is a random number of size
   between 2^(n-1) and 2^(m-1). Require: n<m*)

   val pp : t -> string (**[pp x] is a string representation of x in
   binary*) end *)

val rand_prime : int -> Z.t
(**[rand_prime n] is a pseudo-random prime with greater than ciel(n/30)
   bits.*)
