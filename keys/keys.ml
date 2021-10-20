(* module BigNum = struct type t = bool list

   let rec is_zero x = match x with [] -> true | h :: t -> if h then
   false else is_zero t

   let rec clip x = match List.rev x with [] -> [] | h :: t -> if h then
   x else clip t

   let rec rand n m = if m = 0 then [] else if n = 1 then true :: rand
   (n - 1) (m - 1) else Random.bool () :: rand (n - 1) (m - 1)

   let rec add x y = match (x, y) with | [], y -> y | x, [] -> x | x ::
   xs, y :: ys -> if x && y then false :: add [ true ] (add xs ys) else
   (x || y) :: add xs ys

   let rec subtract x y = match (x, clip y) with | [], y -> failwith
   "less than 0" | x, [] -> x | x :: xs, y :: ys -> if y then if x then
   false :: subtract xs ys else false :: subtract (subtract xs [ true ])
   ys else x :: subtract xs ys

   let rec is_greater x y = try ignore (subtract x y); true with Failure
   _ -> false

   let rec multiply x y = match y with | [] -> [] | y :: ys -> if y then
   add x (false :: multiply x ys) else false :: multiply x ys

   let rec modulo x y = match (clip x, clip y) with | x, [] -> x | [], y
   -> [] | x :: xs, y :: ys -> []

   let pp x = List.fold_left (fun acc x -> if x then "1" ^ acc else "0"
   ^ acc) "" x

   let of_string s = String.list end *)
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

type dh_keys = {
  private_key : Z.t;
  public_key : Z.t;
  shared_key : Z.t;
}

let create_dh_keys pub_key =
  let my_key = rand_prime 128 in
  {
    private_key = my_key;
    public_key = Z.powm (snd pub_key) my_key (fst pub_key);
    shared_key = Z.zero;
  }

let create_dh_shared_key keys their_key pub_key =
  {
    keys with
    shared_key = Z.powm their_key keys.private_key (fst pub_key);
  }

type rsa_keys = {
  private_key : Z.t * Z.t;
  public_key : Z.t;
}

let create_rsa_keys () =
  let p = rand_prime 2048 in
  let q = rand_prime 2048 in
  { private_key = (p, q); public_key = Z.mul p q }
