(* Modifications:
- Bits_Integer.test_bit
- Z -> Z.Int
- HashTable uses native array
- unit32 -- possibly wrong
- Map array_* to FArray.IsabelleMapping.array_*
- Adopt array_blit/blit
- Made implementations for implode and explode 'stack safe' by using map_rev instead of map
--> these are just code printings in String.thy, so they could be improved globally
*)
module Uint : sig
  type t = int
  val dflt_size : Z.Int.t
  val less : t -> t -> bool
  val less_eq : t -> t -> bool
  val set_bit : t -> Z.Int.t -> bool -> t
  val shiftl : t -> Z.Int.t -> t
  val shiftr : t -> Z.Int.t -> t
  val shiftr_signed : t -> Z.Int.t -> t
  val test_bit : t -> Z.Int.t -> bool
  val int_mask : int
  val int32_mask : int32
  val int64_mask : int64
end = struct

type t = int

(* The constant sys_int_size is not yet in BS. This is why we guess it here. *)
let sys_int_size = 32;;

let dflt_size = Z.Int.of_int sys_int_size;;

(* negative numbers have their highest bit set, 
   so they are greater than positive ones *)
let less x y =
  if x<0 then
    y<0 && x<y
  else y < 0 || x < y;;

let less_eq x y =
  if x < 0 then
    y < 0 &&  x <= y
  else y < 0 || x <= y;;

let set_bit x n b =
  let mask = 1 lsl (Z.Int.to_int n)
  in if b then x lor mask
     else x land (lnot mask);;

let shiftl x n = x lsl (Z.Int.to_int n);;

let shiftr x n = x lsr (Z.Int.to_int n);;

let shiftr_signed x n = x asr (Z.Int.to_int n);;

let test_bit x n = x land (1 lsl (Z.Int.to_int n)) <> 0;;

let int_mask =
  if sys_int_size < 32 then lnot 0 else 0xFFFFFFFF;;

let int32_mask = 
  if sys_int_size < 32 then Int32.pred (Int32.shift_left Int32.one sys_int_size)
  else Int32.of_string "0xFFFFFFFF";;

let int64_mask = 
  if sys_int_size < 64 then Int64.pred (Int64.shift_left Int64.one sys_int_size)
  else Int64.of_string "0xFFFFFFFFFFFFFFFF";;

end;; (*struct Uint*)

module Uint32 : sig
  val less : int32 -> int32 -> bool
  val less_eq : int32 -> int32 -> bool
  val set_bit : int32 -> Z.Int.t -> bool -> int32
  val shiftl : int32 -> Z.Int.t -> int32
  val shiftr : int32 -> Z.Int.t -> int32
  val shiftr_signed : int32 -> Z.Int.t -> int32
  val test_bit : int32 -> Z.Int.t -> bool
end = struct

(* negative numbers have their highest bit set, 
   so they are greater than positive ones *)
let less x y =
  if Int32.compare x Int32.zero < 0 then
    Int32.compare y Int32.zero < 0 && Int32.compare x y < 0
  else Int32.compare y Int32.zero < 0 || Int32.compare x y < 0;;

let less_eq x y =
  if Int32.compare x Int32.zero < 0 then
    Int32.compare y Int32.zero < 0 && Int32.compare x y <= 0
  else Int32.compare y Int32.zero < 0 || Int32.compare x y <= 0;;

let set_bit x n b =
  let mask = Int32.shift_left Int32.one (Z.Int.to_int n)
  in if b then Int32.logor x mask
     else Int32.logand x (Int32.lognot mask);;

let shiftl x n = Int32.shift_left x (Z.Int.to_int n);;

let shiftr x n = Int32.shift_right_logical x (Z.Int.to_int n);;

let shiftr_signed x n = Int32.shift_right x (Z.Int.to_int n);;

let test_bit x n =
  Int32.compare 
    (Int32.logand x (Int32.shift_left Int32.one (Z.Int.to_int n)))
    Int32.zero
  <> 0;;

end;; (*struct Uint32*)

module FArray = struct

  type 'a cell = Value of 'a array | Upd of int * 'a * 'a cell ref ;;

  type 'a array = 'a cell ref;;

  let array (size,v) = ref (Value (Array.make size v));;
  let tabulate (size, f) = ref (Value (Array.init size f));;
  let fromList l = ref (Value (Array.of_list l));;

  let rec sub = function
    | ({contents = (Value a)}, idx) -> Array.get a idx
    | ({contents = Upd (i,v,cr)}, idx) ->
        if i=idx then v
        else sub (cr,idx);;

  let rec length = function
    | {contents = Value a} -> Array.length a
    | {contents = Upd (i,v,cr)} -> length cr;;

  let rec realize_aux (aref, v) =
    match aref with
      | {contents = Value a} -> (
        let len = Array.length a in
        let a' = Array.make len v
        in
          Array.blit a 0 a' 0 (Array.length a);
          ref (Value a')
      )
      | {contents = Upd (i,v,cr)} -> (
        let res=realize_aux (cr,v) in
          match res with
            {contents = Value a} -> (Array.set a i v; res)
      );;

  let realize aref =
    match aref with
      | {contents  = Value _} -> aref
      | {contents = Upd (i,v,cr)} -> realize_aux(aref,v);;

  let update (aref,idx,v) =
    match aref with
      | {contents = Value a} -> (
        let nref=ref (Value a) in
          aref := Upd (idx,Array.get a idx,nref);
          Array.set  a idx v;
          nref
      )
      | {contents = Upd _} ->
        let ra = realize_aux(aref,v) in
          match ra with
            {contents = Value a} -> Array.set a idx v;
          ra
      ;;

  let rec grow (aref, inc, x) = match aref with
    | {contents = Value a} -> (
      let len=Array.length a in
      let na = Array.make (len+inc) x
      in
        Array.blit a 0 na 0 (Array.length a);
        ref (Value na)
      )
  | {contents = Upd _} -> (
    grow (realize aref, inc, x)
  );;

exception Size;;

  let rec shrink (aref, sz) = match aref with
    | {contents = Value a} -> (
      if sz > Array.length a then
        raise Size
      else (
        ref (Value (Array.init sz (fun i -> Array.get a i)))
      )
    )
    | { contents = Upd _} -> (
      shrink (realize aref,sz)
    );;

module IsabelleMapping = struct

type 'a array_type = 'a array;;

let new_array (a:'a) (n:Z.Int.t) = array (Z.Int.to_int n, a);;

let array_length (a:'a array_type) = Z.Int.of_int (length a);;

let array_get (a:'a array_type) (i:Z.Int.t) = sub (a, Z.Int.to_int i);;

let array_set (a:'a array_type) (i:Z.Int.t) (e:'a) = update (a, Z.Int.to_int i, e);;

let array_of_list (xs:'a list) = fromList xs;;

let array_grow (a:'a array_type) (i:Z.Int.t) (x:'a) = grow (a, Z.Int.to_int i, x);;

let array_shrink (a:'a array_type) (sz:Z.Int.t) = shrink (a,Z.Int.to_int sz);;

let array_get_oo (d:'a) (a:'a array_type) (i:Z.Int.t) =
  try sub (a,Z.Int.to_int i) with Invalid_argument _ -> d

let array_set_oo (d:(unit->'a array_type)) (a:'a array_type) (i:Z.Int.t) (e:'a) =
  try update (a, Z.Int.to_int i, e) with Invalid_argument _ -> d ()

end;;

end;;

module Tracing : sig
  val count_up : unit -> unit
  val get_count : unit -> int
end = struct
  let counter = ref 0
  let count_up () = (counter := !counter + 1)
  let get_count () = !counter
end


module Bits_Integer : sig
  val shiftl : Z.Int.t -> Z.Int.t -> Z.Int.t
  val shiftr : Z.Int.t -> Z.Int.t -> Z.Int.t
  val test_bit : Z.Int.t -> Z.Int.t -> bool
end = struct

(* We do not need an explicit range checks here,
   because Z.Int.of_int raises Failure 
   if the argument does not fit into an int. *)
let shiftl x n = Z.Int.shift_left x (Z.Int.to_int n);;

let shiftr x n = Z.Int.shift_right x (Z.Int.to_int n);;

(* let test_bit x n =  Z.Int.testbit x (Z.Int.to_int n);; *)
(* XXX Missing form bs-zarith *)
let test_bit x n = Z.Int.to_int x land (1 lsl (Z.Int.to_int n)) <> 0

end;; (*struct Bits_Integer*)

module Model_Checker : sig
  type int = Int_of_integer of Z.Int.t
  type num
  type nat
  val nat_of_integer : Z.Int.t -> nat
  type char
  type 'a act
  val explode : string -> char list
  type fract = Rata of bool * int * int
  type json = Object of (char list * json) list | Arrayb of json list |
    Stringa of char list | Int of int | Nata of nat | Rat of fract |
    Boolean of bool | Null
  type ('a, 'b) bexp
  type ('a, 'b) exp
  type ('a, 'b) acconstraint
  type ('a, 'b) sum
  type 'a result = Result of 'a | Error of string list
  type 'a len_list
  type ('a, 'b, 'c, 'd) formula
  val parse_convert_run : bool -> string -> (unit -> string result)
  val convert_run : bool -> json -> (unit -> string result)
end = struct

type int = Int_of_integer of Z.Int.t;;

let rec integer_of_int (Int_of_integer k) = k;;

let rec equal_inta k l = Z.Int.equal (integer_of_int k) (integer_of_int l);;

type 'a equal = {equal : 'a -> 'a -> bool};;
let equal _A = _A.equal;;

let equal_int = ({equal = equal_inta} : int equal);;

type typerepa = Typerep of string * typerepa list;;

type 'a itself = Type;;

let rec typerep_inta t = Typerep ("Int.int", []);;

type 'a typerep = {typerep : 'a itself -> typerepa};;
let typerep _A = _A.typerep;;

type 'a countable = unit;;

type 'a heap = {countable_heap : 'a countable; typerep_heap : 'a typerep};;

let countable_int = (() : int countable);;

let typerep_int = ({typerep = typerep_inta} : int typerep);;

let heap_int =
  ({countable_heap = countable_int; typerep_heap = typerep_int} : int heap);;

let rec uminus_inta k = Int_of_integer (Z.Int.neg (integer_of_int k));;

let zero_inta : int = Int_of_integer Z.Int.zero;;

let rec apsnd f (x, y) = (x, f y);;

type num = One | Bit0 of num | Bit1 of num;;

let rec divmod_integer
  k l = (if Z.Int.equal k Z.Int.zero then (Z.Int.zero, Z.Int.zero)
          else (if Z.Int.lt Z.Int.zero l
                 then (if Z.Int.lt Z.Int.zero k
                        then (fun k l -> if Z.Int.equal Z.Int.zero l then
                               (Z.Int.zero, l) else Z.Int.div_rem (Z.Int.abs k) (Z.Int.abs l))
                               k l
                        else (let (r, s) =
                                (fun k l -> if Z.Int.equal Z.Int.zero l then
                                  (Z.Int.zero, l) else Z.Int.div_rem (Z.Int.abs k)
                                  (Z.Int.abs l))
                                  k l
                                in
                               (if Z.Int.equal s Z.Int.zero then (Z.Int.neg r, Z.Int.zero)
                                 else (Z.Int.sub (Z.Int.neg r) (Z.Int.of_int 1),
Z.Int.sub l s))))
                 else (if Z.Int.equal l Z.Int.zero then (Z.Int.zero, k)
                        else apsnd Z.Int.neg
                               (if Z.Int.lt k Z.Int.zero
                                 then (fun k l -> if Z.Int.equal Z.Int.zero l then
(Z.Int.zero, l) else Z.Int.div_rem (Z.Int.abs k) (Z.Int.abs l))
k l
                                 else (let (r, s) =
 (fun k l -> if Z.Int.equal Z.Int.zero l then (Z.Int.zero, l) else Z.Int.div_rem (Z.Int.abs k)
   (Z.Int.abs l))
   k l
 in
(if Z.Int.equal s Z.Int.zero then (Z.Int.neg r, Z.Int.zero)
  else (Z.Int.sub (Z.Int.neg r) (Z.Int.of_int 1), Z.Int.sub (Z.Int.neg l) s)))))));;

let rec snd (x1, x2) = x2;;

let rec modulo_integer k l = snd (divmod_integer k l);;

type nat = Nat of Z.Int.t;;

let rec integer_of_nat (Nat x) = x;;

let rec modulo_nat
  m n = Nat (modulo_integer (integer_of_nat m) (integer_of_nat n));;

let rec fst (x1, x2) = x1;;

let rec divide_integer k l = fst (divmod_integer k l);;

let rec divide_nat
  m n = Nat (divide_integer (integer_of_nat m) (integer_of_nat n));;

let rec equal_nata m n = Z.Int.equal (integer_of_nat m) (integer_of_nat n);;

type 'a ord = {less_eq : 'a -> 'a -> bool; less : 'a -> 'a -> bool};;
let less_eq _A = _A.less_eq;;
let less _A = _A.less;;

let rec max _A a b = (if less_eq _A a b then b else a);;

let ord_integer = ({less_eq = Z.Int.leq; less = Z.Int.lt} : Z.Int.t ord);;

let rec nat_of_integer k = Nat (max ord_integer Z.Int.zero k);;

let zero_nata : nat = Nat Z.Int.zero;;

let one_nata : nat = Nat (Z.Int.of_int 1);;

type char = Chara of bool * bool * bool * bool * bool * bool * bool * bool;;

let rec string_of_digit
  n = (if equal_nata n zero_nata
        then [Chara (false, false, false, false, true, true, false, false)]
        else (if equal_nata n one_nata
               then [Chara (true, false, false, false, true, true, false,
                             false)]
               else (if equal_nata n (nat_of_integer (Z.Int.of_int 2))
                      then [Chara (false, true, false, false, true, true, false,
                                    false)]
                      else (if equal_nata n (nat_of_integer (Z.Int.of_int 3))
                             then [Chara (true, true, false, false, true, true,
   false, false)]
                             else (if equal_nata n (nat_of_integer (Z.Int.of_int 4))
                                    then [Chara
    (false, false, true, false, true, true, false, false)]
                                    else (if equal_nata n
       (nat_of_integer (Z.Int.of_int 5))
   then [Chara (true, false, true, false, true, true, false, false)]
   else (if equal_nata n (nat_of_integer (Z.Int.of_int 6))
          then [Chara (false, true, true, false, true, true, false, false)]
          else (if equal_nata n (nat_of_integer (Z.Int.of_int 7))
                 then [Chara (true, true, true, false, true, true, false,
                               false)]
                 else (if equal_nata n (nat_of_integer (Z.Int.of_int 8))
                        then [Chara (false, false, false, true, true, true,
                                      false, false)]
                        else [Chara (true, false, false, true, true, true,
                                      false, false)])))))))));;

let rec less_nat m n = Z.Int.lt (integer_of_nat m) (integer_of_nat n);;

let rec shows_string x = (fun a -> x @ a);;

let rec comp f g = (fun x -> f (g x));;

let rec showsp_nat
  p n = (if less_nat n (nat_of_integer (Z.Int.of_int 10))
          then shows_string (string_of_digit n)
          else comp (showsp_nat p (divide_nat n (nat_of_integer (Z.Int.of_int 10))))
                 (shows_string
                   (string_of_digit
                     (modulo_nat n (nat_of_integer (Z.Int.of_int 10))))));;

let rec less_int k l = Z.Int.lt (integer_of_int k) (integer_of_int l);;

let rec nat k = Nat (max ord_integer Z.Int.zero (integer_of_int k));;

let rec showsp_int
  p i = (if less_int i zero_inta
          then comp (shows_string
                      [Chara (true, false, true, true, false, true, false,
                               false)])
                 (showsp_nat p (nat (uminus_inta i)))
          else showsp_nat p (nat i));;

let rec shows_prec_int x = showsp_int x;;

let rec shows_sep
  s sep x2 = match s, sep, x2 with s, sep, [] -> shows_string []
    | s, sep, [x] -> s x
    | s, sep, x :: v :: va ->
        comp (comp (s x) sep) (shows_sep s sep (v :: va));;

let rec null = function [] -> true
               | x :: xs -> false;;

let rec shows_list_gen
  showsx e l s r xs =
    (if null xs then shows_string e
      else comp (comp (shows_string l) (shows_sep showsx (shows_string s) xs))
             (shows_string r));;

let rec showsp_list
  s p xs =
    shows_list_gen (s zero_nata)
      [Chara (true, true, false, true, true, false, true, false);
        Chara (true, false, true, true, true, false, true, false)]
      [Chara (true, true, false, true, true, false, true, false)]
      [Chara (false, false, true, true, false, true, false, false);
        Chara (false, false, false, false, false, true, false, false)]
      [Chara (true, false, true, true, true, false, true, false)] xs;;

let rec shows_list_int x = showsp_list shows_prec_int zero_nata x;;

type 'a show =
  {shows_prec : nat -> 'a -> char list -> char list;
    shows_list : 'a list -> char list -> char list};;
let shows_prec _A = _A.shows_prec;;
let shows_list _A = _A.shows_list;;

let show_int =
  ({shows_prec = shows_prec_int; shows_list = shows_list_int} : int show);;

let rec plus_inta
  k l = Int_of_integer (Z.Int.add (integer_of_int k) (integer_of_int l));;

type 'a plus = {plus : 'a -> 'a -> 'a};;
let plus _A = _A.plus;;

let plus_int = ({plus = plus_inta} : int plus);;

type 'a zero = {zero : 'a};;
let zero _A = _A.zero;;

let zero_int = ({zero = zero_inta} : int zero);;

let rec minus_inta
  k l = Int_of_integer (Z.Int.sub (integer_of_int k) (integer_of_int l));;

type 'a minus = {minus : 'a -> 'a -> 'a};;
let minus _A = _A.minus;;

let minus_int = ({minus = minus_inta} : int minus);;

type 'a uminus = {uminus : 'a -> 'a};;
let uminus _A = _A.uminus;;

let uminus_int = ({uminus = uminus_inta} : int uminus);;

let rec less_eq_int k l = Z.Int.leq (integer_of_int k) (integer_of_int l);;

let ord_int = ({less_eq = less_eq_int; less = less_int} : int ord);;

type 'a preorder = {ord_preorder : 'a ord};;

type 'a order = {preorder_order : 'a preorder};;

let preorder_int = ({ord_preorder = ord_int} : int preorder);;

let order_int = ({preorder_order = preorder_int} : int order);;

type 'a semigroup_add = {plus_semigroup_add : 'a plus};;

type 'a cancel_semigroup_add =
  {semigroup_add_cancel_semigroup_add : 'a semigroup_add};;

type 'a monoid_add =
  {semigroup_add_monoid_add : 'a semigroup_add; zero_monoid_add : 'a zero};;

type 'a group_add =
  {cancel_semigroup_add_group_add : 'a cancel_semigroup_add;
    minus_group_add : 'a minus; monoid_add_group_add : 'a monoid_add;
    uminus_group_add : 'a uminus};;

let semigroup_add_int = ({plus_semigroup_add = plus_int} : int semigroup_add);;

let cancel_semigroup_add_int =
  ({semigroup_add_cancel_semigroup_add = semigroup_add_int} :
    int cancel_semigroup_add);;

let monoid_add_int =
  ({semigroup_add_monoid_add = semigroup_add_int; zero_monoid_add = zero_int} :
    int monoid_add);;

let group_add_int =
  ({cancel_semigroup_add_group_add = cancel_semigroup_add_int;
     minus_group_add = minus_int; monoid_add_group_add = monoid_add_int;
     uminus_group_add = uminus_int}
    : int group_add);;

let rec def_hashmap_size_int x = (fun _ -> nat_of_integer (Z.Int.of_int 16)) x;;

type 'a hashable =
  {hashcode : 'a -> int32; def_hashmap_size : 'a itself -> nat};;
let hashcode _A = _A.hashcode;;
let def_hashmap_size _A = _A.def_hashmap_size;;

let rec test_bit_integer x n = Bits_Integer.test_bit x (integer_of_nat n);;

let rec uint32
  i = (let ia = Z.Int.logand i (Z.Int.of_string "4294967295") in
        (if test_bit_integer ia (nat_of_integer (Z.Int.of_int 31))
          then Z.Int.to_int32 (Z.Int.sub ia (Z.Int.of_string "4294967296"))
          else Z.Int.to_int32 ia));;

let rec uint32 i = Z.Int.to_int32 i;;

let rec uint32_of_int i = uint32 (integer_of_int i);;

let rec hashcode_int i = uint32_of_int i;;

let hashable_int =
  ({hashcode = hashcode_int; def_hashmap_size = def_hashmap_size_int} :
    int hashable);;

type 'a linorder = {order_linorder : 'a order};;

let linorder_int = ({order_linorder = order_int} : int linorder);;

type 'a ab_semigroup_add = {semigroup_add_ab_semigroup_add : 'a semigroup_add};;

type 'a cancel_ab_semigroup_add =
  {ab_semigroup_add_cancel_ab_semigroup_add : 'a ab_semigroup_add;
    cancel_semigroup_add_cancel_ab_semigroup_add : 'a cancel_semigroup_add;
    minus_cancel_ab_semigroup_add : 'a minus};;

type 'a comm_monoid_add =
  {ab_semigroup_add_comm_monoid_add : 'a ab_semigroup_add;
    monoid_add_comm_monoid_add : 'a monoid_add};;

type 'a cancel_comm_monoid_add =
  {cancel_ab_semigroup_add_cancel_comm_monoid_add : 'a cancel_ab_semigroup_add;
    comm_monoid_add_cancel_comm_monoid_add : 'a comm_monoid_add};;

type 'a ab_group_add =
  {cancel_comm_monoid_add_ab_group_add : 'a cancel_comm_monoid_add;
    group_add_ab_group_add : 'a group_add};;

let ab_semigroup_add_int =
  ({semigroup_add_ab_semigroup_add = semigroup_add_int} :
    int ab_semigroup_add);;

let cancel_ab_semigroup_add_int =
  ({ab_semigroup_add_cancel_ab_semigroup_add = ab_semigroup_add_int;
     cancel_semigroup_add_cancel_ab_semigroup_add = cancel_semigroup_add_int;
     minus_cancel_ab_semigroup_add = minus_int}
    : int cancel_ab_semigroup_add);;

let comm_monoid_add_int =
  ({ab_semigroup_add_comm_monoid_add = ab_semigroup_add_int;
     monoid_add_comm_monoid_add = monoid_add_int}
    : int comm_monoid_add);;

let cancel_comm_monoid_add_int =
  ({cancel_ab_semigroup_add_cancel_comm_monoid_add =
      cancel_ab_semigroup_add_int;
     comm_monoid_add_cancel_comm_monoid_add = comm_monoid_add_int}
    : int cancel_comm_monoid_add);;

let ab_group_add_int =
  ({cancel_comm_monoid_add_ab_group_add = cancel_comm_monoid_add_int;
     group_add_ab_group_add = group_add_int}
    : int ab_group_add);;

type 'a ordered_ab_semigroup_add =
  {ab_semigroup_add_ordered_ab_semigroup_add : 'a ab_semigroup_add;
    order_ordered_ab_semigroup_add : 'a order};;

type 'a strict_ordered_ab_semigroup_add =
  {ordered_ab_semigroup_add_strict_ordered_ab_semigroup_add :
     'a ordered_ab_semigroup_add};;

type 'a ordered_cancel_ab_semigroup_add =
  {cancel_ab_semigroup_add_ordered_cancel_ab_semigroup_add :
     'a cancel_ab_semigroup_add;
    strict_ordered_ab_semigroup_add_ordered_cancel_ab_semigroup_add :
      'a strict_ordered_ab_semigroup_add};;

type 'a ordered_ab_semigroup_add_imp_le =
  {ordered_cancel_ab_semigroup_add_ordered_ab_semigroup_add_imp_le :
     'a ordered_cancel_ab_semigroup_add};;

type 'a strict_ordered_comm_monoid_add =
  {comm_monoid_add_strict_ordered_comm_monoid_add : 'a comm_monoid_add;
    strict_ordered_ab_semigroup_add_strict_ordered_comm_monoid_add :
      'a strict_ordered_ab_semigroup_add};;

type 'a ordered_comm_monoid_add =
  {comm_monoid_add_ordered_comm_monoid_add : 'a comm_monoid_add;
    ordered_ab_semigroup_add_ordered_comm_monoid_add :
      'a ordered_ab_semigroup_add};;

type 'a ordered_cancel_comm_monoid_add =
  {ordered_cancel_ab_semigroup_add_ordered_cancel_comm_monoid_add :
     'a ordered_cancel_ab_semigroup_add;
    ordered_comm_monoid_add_ordered_cancel_comm_monoid_add :
      'a ordered_comm_monoid_add;
    strict_ordered_comm_monoid_add_ordered_cancel_comm_monoid_add :
      'a strict_ordered_comm_monoid_add};;

type 'a ordered_ab_semigroup_monoid_add_imp_le =
  {cancel_comm_monoid_add_ordered_ab_semigroup_monoid_add_imp_le :
     'a cancel_comm_monoid_add;
    ordered_ab_semigroup_add_imp_le_ordered_ab_semigroup_monoid_add_imp_le :
      'a ordered_ab_semigroup_add_imp_le;
    ordered_cancel_comm_monoid_add_ordered_ab_semigroup_monoid_add_imp_le :
      'a ordered_cancel_comm_monoid_add};;

type 'a ordered_ab_group_add =
  {ab_group_add_ordered_ab_group_add : 'a ab_group_add;
    ordered_ab_semigroup_monoid_add_imp_le_ordered_ab_group_add :
      'a ordered_ab_semigroup_monoid_add_imp_le};;

let ordered_ab_semigroup_add_int =
  ({ab_semigroup_add_ordered_ab_semigroup_add = ab_semigroup_add_int;
     order_ordered_ab_semigroup_add = order_int}
    : int ordered_ab_semigroup_add);;

let strict_ordered_ab_semigroup_add_int =
  ({ordered_ab_semigroup_add_strict_ordered_ab_semigroup_add =
      ordered_ab_semigroup_add_int}
    : int strict_ordered_ab_semigroup_add);;

let ordered_cancel_ab_semigroup_add_int =
  ({cancel_ab_semigroup_add_ordered_cancel_ab_semigroup_add =
      cancel_ab_semigroup_add_int;
     strict_ordered_ab_semigroup_add_ordered_cancel_ab_semigroup_add =
       strict_ordered_ab_semigroup_add_int}
    : int ordered_cancel_ab_semigroup_add);;

let ordered_ab_semigroup_add_imp_le_int =
  ({ordered_cancel_ab_semigroup_add_ordered_ab_semigroup_add_imp_le =
      ordered_cancel_ab_semigroup_add_int}
    : int ordered_ab_semigroup_add_imp_le);;

let strict_ordered_comm_monoid_add_int =
  ({comm_monoid_add_strict_ordered_comm_monoid_add = comm_monoid_add_int;
     strict_ordered_ab_semigroup_add_strict_ordered_comm_monoid_add =
       strict_ordered_ab_semigroup_add_int}
    : int strict_ordered_comm_monoid_add);;

let ordered_comm_monoid_add_int =
  ({comm_monoid_add_ordered_comm_monoid_add = comm_monoid_add_int;
     ordered_ab_semigroup_add_ordered_comm_monoid_add =
       ordered_ab_semigroup_add_int}
    : int ordered_comm_monoid_add);;

let ordered_cancel_comm_monoid_add_int =
  ({ordered_cancel_ab_semigroup_add_ordered_cancel_comm_monoid_add =
      ordered_cancel_ab_semigroup_add_int;
     ordered_comm_monoid_add_ordered_cancel_comm_monoid_add =
       ordered_comm_monoid_add_int;
     strict_ordered_comm_monoid_add_ordered_cancel_comm_monoid_add =
       strict_ordered_comm_monoid_add_int}
    : int ordered_cancel_comm_monoid_add);;

let ordered_ab_semigroup_monoid_add_imp_le_int =
  ({cancel_comm_monoid_add_ordered_ab_semigroup_monoid_add_imp_le =
      cancel_comm_monoid_add_int;
     ordered_ab_semigroup_add_imp_le_ordered_ab_semigroup_monoid_add_imp_le =
       ordered_ab_semigroup_add_imp_le_int;
     ordered_cancel_comm_monoid_add_ordered_ab_semigroup_monoid_add_imp_le =
       ordered_cancel_comm_monoid_add_int}
    : int ordered_ab_semigroup_monoid_add_imp_le);;

let ordered_ab_group_add_int =
  ({ab_group_add_ordered_ab_group_add = ab_group_add_int;
     ordered_ab_semigroup_monoid_add_imp_le_ordered_ab_group_add =
       ordered_ab_semigroup_monoid_add_imp_le_int}
    : int ordered_ab_group_add);;

type 'a linordered_ab_semigroup_add =
  {ordered_ab_semigroup_add_linordered_ab_semigroup_add :
     'a ordered_ab_semigroup_add;
    linorder_linordered_ab_semigroup_add : 'a linorder};;

type 'a linordered_cancel_ab_semigroup_add =
  {linordered_ab_semigroup_add_linordered_cancel_ab_semigroup_add :
     'a linordered_ab_semigroup_add;
    ordered_ab_semigroup_add_imp_le_linordered_cancel_ab_semigroup_add :
      'a ordered_ab_semigroup_add_imp_le};;

type 'a linordered_ab_monoid_add =
  {linordered_ab_semigroup_add_linordered_ab_monoid_add :
     'a linordered_ab_semigroup_add;
    ordered_comm_monoid_add_linordered_ab_monoid_add :
      'a ordered_comm_monoid_add};;

type 'a linordered_cancel_ab_monoid_add =
  {linordered_ab_monoid_add_linordered_cancel_ab_monoid_add :
     'a linordered_ab_monoid_add;
    linordered_cancel_ab_semigroup_add_linordered_cancel_ab_monoid_add :
      'a linordered_cancel_ab_semigroup_add};;

type 'a linordered_ab_group_add =
  {linordered_cancel_ab_monoid_add_linordered_ab_group_add :
     'a linordered_cancel_ab_monoid_add;
    ordered_ab_group_add_linordered_ab_group_add : 'a ordered_ab_group_add};;

let linordered_ab_semigroup_add_int =
  ({ordered_ab_semigroup_add_linordered_ab_semigroup_add =
      ordered_ab_semigroup_add_int;
     linorder_linordered_ab_semigroup_add = linorder_int}
    : int linordered_ab_semigroup_add);;

let linordered_cancel_ab_semigroup_add_int =
  ({linordered_ab_semigroup_add_linordered_cancel_ab_semigroup_add =
      linordered_ab_semigroup_add_int;
     ordered_ab_semigroup_add_imp_le_linordered_cancel_ab_semigroup_add =
       ordered_ab_semigroup_add_imp_le_int}
    : int linordered_cancel_ab_semigroup_add);;

let linordered_ab_monoid_add_int =
  ({linordered_ab_semigroup_add_linordered_ab_monoid_add =
      linordered_ab_semigroup_add_int;
     ordered_comm_monoid_add_linordered_ab_monoid_add =
       ordered_comm_monoid_add_int}
    : int linordered_ab_monoid_add);;

let linordered_cancel_ab_monoid_add_int =
  ({linordered_ab_monoid_add_linordered_cancel_ab_monoid_add =
      linordered_ab_monoid_add_int;
     linordered_cancel_ab_semigroup_add_linordered_cancel_ab_monoid_add =
       linordered_cancel_ab_semigroup_add_int}
    : int linordered_cancel_ab_monoid_add);;

let linordered_ab_group_add_int =
  ({linordered_cancel_ab_monoid_add_linordered_ab_group_add =
      linordered_cancel_ab_monoid_add_int;
     ordered_ab_group_add_linordered_ab_group_add = ordered_ab_group_add_int}
    : int linordered_ab_group_add);;

let equal_nat = ({equal = equal_nata} : nat equal);;

let rec typerep_nata t = Typerep ("Nat.nat", []);;

let countable_nat = (() : nat countable);;

let typerep_nat = ({typerep = typerep_nata} : nat typerep);;

let heap_nat =
  ({countable_heap = countable_nat; typerep_heap = typerep_nat} : nat heap);;

let rec shows_prec_nat x = showsp_nat x;;

let rec shows_list_nat x = showsp_list shows_prec_nat zero_nata x;;

let show_nat =
  ({shows_prec = shows_prec_nat; shows_list = shows_list_nat} : nat show);;

type 'a one = {one : 'a};;
let one _A = _A.one;;

let one_nat = ({one = one_nata} : nat one);;

let rec plus_nata m n = Nat (Z.Int.add (integer_of_nat m) (integer_of_nat n));;

let plus_nat = ({plus = plus_nata} : nat plus);;

let zero_nat = ({zero = zero_nata} : nat zero);;

let rec less_eq_nat m n = Z.Int.leq (integer_of_nat m) (integer_of_nat n);;

let ord_nat = ({less_eq = less_eq_nat; less = less_nat} : nat ord);;

let rec def_hashmap_size_nat x = (fun _ -> nat_of_integer (Z.Int.of_int 16)) x;;

let rec int_of_nat n = Int_of_integer (integer_of_nat n);;

let rec hashcode_nat n = uint32_of_int (int_of_nat n);;

let hashable_nat =
  ({hashcode = hashcode_nat; def_hashmap_size = def_hashmap_size_nat} :
    nat hashable);;

type ('a, 'b) phantom = Phantom of 'b;;

let finite_UNIV_nata : (nat, bool) phantom = Phantom false;;

let card_UNIV_nata : (nat, nat) phantom = Phantom zero_nata;;

type 'a finite_UNIV = {finite_UNIV : ('a, bool) phantom};;
let finite_UNIV _A = _A.finite_UNIV;;

type 'a card_UNIV =
  {finite_UNIV_card_UNIV : 'a finite_UNIV; card_UNIV : ('a, nat) phantom};;
let card_UNIV _A = _A.card_UNIV;;

let finite_UNIV_nat = ({finite_UNIV = finite_UNIV_nata} : nat finite_UNIV);;

let card_UNIV_nat =
  ({finite_UNIV_card_UNIV = finite_UNIV_nat; card_UNIV = card_UNIV_nata} :
    nat card_UNIV);;

let rec eq _A a b = equal _A a b;;

let rec equal_lista _A
  x0 x1 = match x0, x1 with [], x21 :: x22 -> false
    | x21 :: x22, [] -> false
    | x21 :: x22, y21 :: y22 -> eq _A x21 y21 && equal_lista _A x22 y22
    | [], [] -> true;;

let rec equal_list _A = ({equal = equal_lista _A} : ('a list) equal);;

let rec typerep_lista _A t = Typerep ("List.list", [typerep _A Type]);;

let rec countable_list _A = (() : ('a list) countable);;

let rec typerep_list _A = ({typerep = typerep_lista _A} : ('a list) typerep);;

let rec heap_list _A =
  ({countable_heap = (countable_list _A.countable_heap);
     typerep_heap = (typerep_list _A.typerep_heap)}
    : ('a list) heap);;

let rec shows_prec_list _A p xs = shows_list _A xs;;

let rec shows_list_list _A
  xss = showsp_list (shows_prec_list _A) zero_nata xss;;

let rec show_list _A =
  ({shows_prec = shows_prec_list _A; shows_list = shows_list_list _A} :
    ('a list) show);;

let rec times_nat m n = Nat (Z.Int.mul (integer_of_nat m) (integer_of_nat n));;

let rec def_hashmap_size_list _A
  = (fun _ ->
      times_nat (nat_of_integer (Z.Int.of_int 2)) (def_hashmap_size _A Type));;

let rec foldl f a x2 = match f, a, x2 with f, a, [] -> a
                | f, a, x :: xs -> foldl f (f a x) xs;;

let rec hashcode_list _A
  = foldl (fun h x ->
            Int32.add (Int32.mul h (uint32 (Z.Int.of_int 33))) (hashcode _A x))
      (uint32 (Z.Int.of_int 5381));;

let rec hashable_list _A =
  ({hashcode = hashcode_list _A; def_hashmap_size = def_hashmap_size_list _A} :
    ('a list) hashable);;

let rec typerep_arraya _A t = Typerep ("Heap.array", [typerep _A Type]);;

let countable_array = (() : ('a array) countable);;

let rec typerep_array _A =
  ({typerep = typerep_arraya _A} : ('a array) typerep);;

let rec heap_array _A =
  ({countable_heap = countable_array; typerep_heap = (typerep_array _A)} :
    ('a array) heap);;

let rec equal_bool p pa = match p, pa with p, true -> p
                     | p, false -> not p
                     | true, p -> p
                     | false, p -> not p;;

let rec equal_chara
  (Chara (x1, x2, x3, x4, x5, x6, x7, x8))
    (Chara (y1, y2, y3, y4, y5, y6, y7, y8)) =
    equal_bool x1 y1 &&
      (equal_bool x2 y2 &&
        (equal_bool x3 y3 &&
          (equal_bool x4 y4 &&
            (equal_bool x5 y5 &&
              (equal_bool x6 y6 && (equal_bool x7 y7 && equal_bool x8 y8))))));;

let equal_char = ({equal = equal_chara} : char equal);;

let rec shows_prec_char p c = (fun a -> c :: a);;

let rec shows_list_char cs = shows_string cs;;

let show_char =
  ({shows_prec = shows_prec_char; shows_list = shows_list_char} : char show);;

type 'a zero_neq_one =
  {one_zero_neq_one : 'a one; zero_zero_neq_one : 'a zero};;

let rec of_bool _A = function true -> one _A.one_zero_neq_one
                     | false -> zero _A.zero_zero_neq_one;;

let one_integera : Z.Int.t = (Z.Int.of_int 1);;

let zero_integer = ({zero = Z.Int.zero} : Z.Int.t zero);;

let one_integer = ({one = one_integera} : Z.Int.t one);;

let zero_neq_one_integer =
  ({one_zero_neq_one = one_integer; zero_zero_neq_one = zero_integer} :
    Z.Int.t zero_neq_one);;

let rec integer_of_char
  (Chara (b0, b1, b2, b3, b4, b5, b6, b7)) =
    Z.Int.add (Z.Int.mul
            (Z.Int.add
              (Z.Int.mul
                (Z.Int.add
                  (Z.Int.mul
                    (Z.Int.add
                      (Z.Int.mul
                        (Z.Int.add
                          (Z.Int.mul
                            (Z.Int.add
                              (Z.Int.mul
                                (Z.Int.add
                                  (Z.Int.mul (of_bool zero_neq_one_integer b7)
                                    (Z.Int.of_int 2))
                                  (of_bool zero_neq_one_integer b6))
                                (Z.Int.of_int 2))
                              (of_bool zero_neq_one_integer b5))
                            (Z.Int.of_int 2))
                          (of_bool zero_neq_one_integer b4))
                        (Z.Int.of_int 2))
                      (of_bool zero_neq_one_integer b3))
                    (Z.Int.of_int 2))
                  (of_bool zero_neq_one_integer b2))
                (Z.Int.of_int 2))
              (of_bool zero_neq_one_integer b1))
            (Z.Int.of_int 2))
      (of_bool zero_neq_one_integer b0);;

let rec nat_of_char c = Nat (integer_of_char c);;

let rec less_eq_char c1 c2 = less_eq_nat (nat_of_char c1) (nat_of_char c2);;

let rec less_char c1 c2 = less_nat (nat_of_char c1) (nat_of_char c2);;

let ord_char = ({less_eq = less_eq_char; less = less_char} : char ord);;

let preorder_char = ({ord_preorder = ord_char} : char preorder);;

let order_char = ({preorder_order = preorder_char} : char order);;

let linorder_char = ({order_linorder = order_char} : char linorder);;

type 'a dBMEntry = Le of 'a | Lt of 'a | INF;;

let rec typerep_DBMEntrya _A t = Typerep ("DBM.DBMEntry", [typerep _A Type]);;

let rec countable_DBMEntry _A = (() : 'a dBMEntry countable);;

let rec typerep_DBMEntry _A =
  ({typerep = typerep_DBMEntrya _A} : 'a dBMEntry typerep);;

let rec heap_DBMEntry _A =
  ({countable_heap = (countable_DBMEntry _A.countable_heap);
     typerep_heap = (typerep_DBMEntry _A.typerep_heap)}
    : 'a dBMEntry heap);;

let rec dbm_add _A
  x0 uu = match x0, uu with INF, uu -> INF
    | Le v, INF -> INF
    | Lt v, INF -> INF
    | Le a, Le b ->
        Le (plus _A.ordered_ab_semigroup_add_imp_le_linordered_cancel_ab_semigroup_add.ordered_cancel_ab_semigroup_add_ordered_ab_semigroup_add_imp_le.cancel_ab_semigroup_add_ordered_cancel_ab_semigroup_add.ab_semigroup_add_cancel_ab_semigroup_add.semigroup_add_ab_semigroup_add.plus_semigroup_add
             a b)
    | Le a, Lt b ->
        Lt (plus _A.ordered_ab_semigroup_add_imp_le_linordered_cancel_ab_semigroup_add.ordered_cancel_ab_semigroup_add_ordered_ab_semigroup_add_imp_le.cancel_ab_semigroup_add_ordered_cancel_ab_semigroup_add.ab_semigroup_add_cancel_ab_semigroup_add.semigroup_add_ab_semigroup_add.plus_semigroup_add
             a b)
    | Lt a, Le b ->
        Lt (plus _A.ordered_ab_semigroup_add_imp_le_linordered_cancel_ab_semigroup_add.ordered_cancel_ab_semigroup_add_ordered_ab_semigroup_add_imp_le.cancel_ab_semigroup_add_ordered_cancel_ab_semigroup_add.ab_semigroup_add_cancel_ab_semigroup_add.semigroup_add_ab_semigroup_add.plus_semigroup_add
             a b)
    | Lt a, Lt b ->
        Lt (plus _A.ordered_ab_semigroup_add_imp_le_linordered_cancel_ab_semigroup_add.ordered_cancel_ab_semigroup_add_ordered_ab_semigroup_add_imp_le.cancel_ab_semigroup_add_ordered_cancel_ab_semigroup_add.ab_semigroup_add_cancel_ab_semigroup_add.semigroup_add_ab_semigroup_add.plus_semigroup_add
             a b);;

let rec plus_DBMEntrya _A
  = dbm_add
      _A.linordered_cancel_ab_semigroup_add_linordered_cancel_ab_monoid_add;;

let rec plus_DBMEntry _A = ({plus = plus_DBMEntrya _A} : 'a dBMEntry plus);;

let rec zero_DBMEntrya _A = Le (zero _A);;

let rec zero_DBMEntry _A = ({zero = zero_DBMEntrya _A} : 'a dBMEntry zero);;

let rec equal_DBMEntry _A x0 x1 = match x0, x1 with Lt x2, INF -> false
                            | INF, Lt x2 -> false
                            | Le x1, INF -> false
                            | INF, Le x1 -> false
                            | Le x1, Lt x2 -> false
                            | Lt x2, Le x1 -> false
                            | Lt x2, Lt y2 -> eq _A x2 y2
                            | Le x1, Le y1 -> eq _A x1 y1
                            | INF, INF -> true;;

let rec dbm_lt _A
  xa0 x = match xa0, x with INF, x -> false
    | Lt a, Lt b -> less _A.order_linorder.preorder_order.ord_preorder a b
    | Lt a, Le b -> less_eq _A.order_linorder.preorder_order.ord_preorder a b
    | Le a, Lt b -> less _A.order_linorder.preorder_order.ord_preorder a b
    | Le a, Le b -> less _A.order_linorder.preorder_order.ord_preorder a b
    | Le a, INF -> true
    | Lt a, INF -> true;;

let rec dbm_le (_A1, _A2) a b = equal_DBMEntry _A1 a b || dbm_lt _A2 a b;;

let rec less_eq_DBMEntry (_A1, _A2) = dbm_le (_A1, _A2);;

let rec less_DBMEntry _A = dbm_lt _A;;

let rec ord_DBMEntry (_A1, _A2) =
  ({less_eq = less_eq_DBMEntry (_A1, _A2); less = less_DBMEntry _A2} :
    'a dBMEntry ord);;

let rec preorder_DBMEntry (_A1, _A2) =
  ({ord_preorder = (ord_DBMEntry (_A1, _A2))} : 'a dBMEntry preorder);;

let rec order_DBMEntry (_A1, _A2) =
  ({preorder_order = (preorder_DBMEntry (_A1, _A2))} : 'a dBMEntry order);;

let rec semigroup_add_DBMEntry _A =
  ({plus_semigroup_add = (plus_DBMEntry _A)} : 'a dBMEntry semigroup_add);;

let rec monoid_add_DBMEntry _A =
  ({semigroup_add_monoid_add = (semigroup_add_DBMEntry _A);
     zero_monoid_add =
       (zero_DBMEntry
         _A.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.ordered_comm_monoid_add_linordered_ab_monoid_add.comm_monoid_add_ordered_comm_monoid_add.monoid_add_comm_monoid_add.zero_monoid_add)}
    : 'a dBMEntry monoid_add);;

let rec linorder_DBMEntry (_A1, _A2) =
  ({order_linorder = (order_DBMEntry (_A1, _A2))} : 'a dBMEntry linorder);;

let rec ab_semigroup_add_DBMEntry _A =
  ({semigroup_add_ab_semigroup_add = (semigroup_add_DBMEntry _A)} :
    'a dBMEntry ab_semigroup_add);;

let rec comm_monoid_add_DBMEntry _A =
  ({ab_semigroup_add_comm_monoid_add = (ab_semigroup_add_DBMEntry _A);
     monoid_add_comm_monoid_add = (monoid_add_DBMEntry _A)}
    : 'a dBMEntry comm_monoid_add);;

let rec ordered_ab_semigroup_add_DBMEntry (_A1, _A2) =
  ({ab_semigroup_add_ordered_ab_semigroup_add = (ab_semigroup_add_DBMEntry _A1);
     order_ordered_ab_semigroup_add =
       (order_DBMEntry
         (_A2, _A1.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.linordered_ab_semigroup_add_linordered_ab_monoid_add.linorder_linordered_ab_semigroup_add))}
    : 'a dBMEntry ordered_ab_semigroup_add);;

let rec ordered_comm_monoid_add_DBMEntry (_A1, _A2) =
  ({comm_monoid_add_ordered_comm_monoid_add = (comm_monoid_add_DBMEntry _A1);
     ordered_ab_semigroup_add_ordered_comm_monoid_add =
       (ordered_ab_semigroup_add_DBMEntry (_A1, _A2))}
    : 'a dBMEntry ordered_comm_monoid_add);;

let rec linordered_ab_semigroup_add_DBMEntry (_A1, _A2) =
  ({ordered_ab_semigroup_add_linordered_ab_semigroup_add =
      (ordered_ab_semigroup_add_DBMEntry (_A1, _A2));
     linorder_linordered_ab_semigroup_add =
       (linorder_DBMEntry
         (_A2, _A1.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.linordered_ab_semigroup_add_linordered_ab_monoid_add.linorder_linordered_ab_semigroup_add))}
    : 'a dBMEntry linordered_ab_semigroup_add);;

let rec linordered_ab_monoid_add_DBMEntry (_A1, _A2) =
  ({linordered_ab_semigroup_add_linordered_ab_monoid_add =
      (linordered_ab_semigroup_add_DBMEntry (_A1, _A2));
     ordered_comm_monoid_add_linordered_ab_monoid_add =
       (ordered_comm_monoid_add_DBMEntry (_A1, _A2))}
    : 'a dBMEntry linordered_ab_monoid_add);;

let rec shows_space
  x = shows_prec_char zero_nata
        (Chara (false, false, false, false, false, true, false, false)) x;;

type 'a act = In of 'a | Out of 'a | Sil of 'a;;

let rec id x = (fun xa -> xa) x;;

let rec shows_pr
  p = (if less_nat zero_nata p
        then shows_prec_char zero_nata
               (Chara (true, false, false, true, false, true, false, false))
        else id);;

let rec shows_pl
  p = (if less_nat zero_nata p
        then shows_prec_char zero_nata
               (Chara (false, false, false, true, false, true, false, false))
        else id);;

let rec showsp_act
  show_a p x2 = match show_a, p, x2 with
    show_a, p, Sil x ->
      comp (comp (comp (comp (shows_pl p)
                         (shows_string
                           [Chara (true, true, false, false, true, false, true,
                                    false);
                             Chara (true, false, false, true, false, true, true,
                                     false);
                             Chara (false, false, true, true, false, true, true,
                                     false)]))
                   shows_space)
             (show_a one_nata x))
        (shows_pr p)
    | show_a, p, Out x ->
        comp (comp (comp (comp (shows_pl p)
                           (shows_string
                             [Chara (true, true, true, true, false, false, true,
                                      false);
                               Chara (true, false, true, false, true, true,
                                       true, false);
                               Chara (false, false, true, false, true, true,
                                       true, false)]))
                     shows_space)
               (show_a one_nata x))
          (shows_pr p)
    | show_a, p, In x ->
        comp (comp (comp (comp (shows_pl p)
                           (shows_string
                             [Chara (true, false, false, true, false, false,
                                      true, false);
                               Chara (false, true, true, true, false, true,
                                       true, false)]))
                     shows_space)
               (show_a one_nata x))
          (shows_pr p);;

let rec shows_prec_act _A = showsp_act (shows_prec _A);;

let rec shows_list_act _A = showsp_list (shows_prec_act _A) zero_nata;;

let rec show_act _A =
  ({shows_prec = shows_prec_act _A; shows_list = shows_list_act _A} :
    'a act show);;

let equal_literal = ({equal = (fun a b -> ((a : string) = b))} : string equal);;

let rec bit_cut_integer
  k = (if Z.Int.equal k Z.Int.zero then (Z.Int.zero, false)
        else (let (r, s) =
                (fun k l -> if Z.Int.equal Z.Int.zero l then (Z.Int.zero, l) else Z.Int.div_rem
                  (Z.Int.abs k) (Z.Int.abs l))
                  k (Z.Int.of_int 2)
                in
               ((if Z.Int.lt Z.Int.zero k then r else Z.Int.sub (Z.Int.neg r) s),
                 Z.Int.equal s (Z.Int.of_int 1))));;

let rec char_of_integer
  k = (let (q0, b0) = bit_cut_integer k in
       let (q1, b1) = bit_cut_integer q0 in
       let (q2, b2) = bit_cut_integer q1 in
       let (q3, b3) = bit_cut_integer q2 in
       let (q4, b4) = bit_cut_integer q3 in
       let (q5, b5) = bit_cut_integer q4 in
       let (q6, b6) = bit_cut_integer q5 in
       let a = bit_cut_integer q6 in
       let (_, aa) = a in
        Chara (b0, b1, b2, b3, b4, b5, b6, aa));;

let rec map f x1 = match f, x1 with f, [] -> []
              | f, x21 :: x22 -> f x21 :: map f x22;;

let rec map_rev f a xs = match xs with
  [] -> a
| x :: xs -> map_rev f (f x :: a) xs;;

let rec explode
  s = List.rev (map_rev char_of_integer []
        (let s = s in let rec exp i l = if i < 0 then l else exp (i - 1) (let k = Char.code (String.get s i) in
      if k < 128 then Z.Int.of_int k :: l else failwith "Non-ASCII character in literal") in exp (String.length s - 1) []));;

let rec shows_prec_literal p s = shows_string (explode s);;

let rec foldr f x1 = match f, x1 with f, [] -> id
                | f, x :: xs -> comp (f x) (foldr f xs);;

let rec shows_list_literal x = foldr (fun s -> shows_string (explode s)) x;;

let show_literal =
  ({shows_prec = shows_prec_literal; shows_list = shows_list_literal} :
    string show);;

let countable_literal = (() : string countable);;

let finite_UNIV_literala : (string, bool) phantom = Phantom false;;

let card_UNIV_literala : (string, nat) phantom = Phantom zero_nata;;

let finite_UNIV_literal =
  ({finite_UNIV = finite_UNIV_literala} : string finite_UNIV);;

let card_UNIV_literal =
  ({finite_UNIV_card_UNIV = finite_UNIV_literal; card_UNIV = card_UNIV_literala}
    : string card_UNIV);;

type fract = Rata of bool * int * int;;

let rec equal_fract
  (Rata (x1, x2, x3)) (Rata (y1, y2, y3)) =
    equal_bool x1 y1 && (equal_inta x2 y2 && equal_inta x3 y3);;

type json = Object of (char list * json) list | Arrayb of json list |
  Stringa of char list | Int of int | Nata of nat | Rat of fract |
  Boolean of bool | Null;;

let rec equal_proda _A _B (x1, x2) (y1, y2) = eq _A x1 y1 && eq _B x2 y2;;

let rec equal_prod _A _B = ({equal = equal_proda _A _B} : ('a * 'b) equal);;

let rec equal_JSON () = ({equal = equal_JSONa} : json equal)
and equal_JSONa
  x0 x1 = match x0, x1 with Boolean x7, Null -> false
    | Null, Boolean x7 -> false
    | Rat x6, Null -> false
    | Null, Rat x6 -> false
    | Rat x6, Boolean x7 -> false
    | Boolean x7, Rat x6 -> false
    | Nata x5, Null -> false
    | Null, Nata x5 -> false
    | Nata x5, Boolean x7 -> false
    | Boolean x7, Nata x5 -> false
    | Nata x5, Rat x6 -> false
    | Rat x6, Nata x5 -> false
    | Int x4, Null -> false
    | Null, Int x4 -> false
    | Int x4, Boolean x7 -> false
    | Boolean x7, Int x4 -> false
    | Int x4, Rat x6 -> false
    | Rat x6, Int x4 -> false
    | Int x4, Nata x5 -> false
    | Nata x5, Int x4 -> false
    | Stringa x3, Null -> false
    | Null, Stringa x3 -> false
    | Stringa x3, Boolean x7 -> false
    | Boolean x7, Stringa x3 -> false
    | Stringa x3, Rat x6 -> false
    | Rat x6, Stringa x3 -> false
    | Stringa x3, Nata x5 -> false
    | Nata x5, Stringa x3 -> false
    | Stringa x3, Int x4 -> false
    | Int x4, Stringa x3 -> false
    | Arrayb x2, Null -> false
    | Null, Arrayb x2 -> false
    | Arrayb x2, Boolean x7 -> false
    | Boolean x7, Arrayb x2 -> false
    | Arrayb x2, Rat x6 -> false
    | Rat x6, Arrayb x2 -> false
    | Arrayb x2, Nata x5 -> false
    | Nata x5, Arrayb x2 -> false
    | Arrayb x2, Int x4 -> false
    | Int x4, Arrayb x2 -> false
    | Arrayb x2, Stringa x3 -> false
    | Stringa x3, Arrayb x2 -> false
    | Object x1, Null -> false
    | Null, Object x1 -> false
    | Object x1, Boolean x7 -> false
    | Boolean x7, Object x1 -> false
    | Object x1, Rat x6 -> false
    | Rat x6, Object x1 -> false
    | Object x1, Nata x5 -> false
    | Nata x5, Object x1 -> false
    | Object x1, Int x4 -> false
    | Int x4, Object x1 -> false
    | Object x1, Stringa x3 -> false
    | Stringa x3, Object x1 -> false
    | Object x1, Arrayb x2 -> false
    | Arrayb x2, Object x1 -> false
    | Boolean x7, Boolean y7 -> equal_bool x7 y7
    | Rat x6, Rat y6 -> equal_fract x6 y6
    | Nata x5, Nata y5 -> equal_nata x5 y5
    | Int x4, Int y4 -> equal_inta x4 y4
    | Stringa x3, Stringa y3 -> equal_lista equal_char x3 y3
    | Arrayb x2, Arrayb y2 -> equal_lista (equal_JSON ()) x2 y2
    | Object x1, Object y1 ->
        equal_lista (equal_prod (equal_list equal_char) (equal_JSON ())) x1 y1
    | Null, Null -> true;;
let equal_JSON = equal_JSON ();;

let rec typerep_proda _A _B
  t = Typerep ("Product_Type.prod", [typerep _A Type; typerep _B Type]);;

let rec countable_prod _A _B = (() : ('a * 'b) countable);;

let rec typerep_prod _A _B =
  ({typerep = typerep_proda _A _B} : ('a * 'b) typerep);;

let rec heap_prod _A _B =
  ({countable_heap = (countable_prod _A.countable_heap _B.countable_heap);
     typerep_heap = (typerep_prod _A.typerep_heap _B.typerep_heap)}
    : ('a * 'b) heap);;

let rec showsp_prod
  s1 s2 p (x, y) =
    comp (comp (comp (comp (shows_string
                             [Chara (false, false, false, true, false, true,
                                      false, false)])
                       (s1 one_nata x))
                 (shows_string
                   [Chara (false, false, true, true, false, true, false, false);
                     Chara (false, false, false, false, false, true, false,
                             false)]))
           (s2 one_nata y))
      (shows_string
        [Chara (true, false, false, true, false, true, false, false)]);;

let rec shows_prec_prod _A _B = showsp_prod (shows_prec _A) (shows_prec _B);;

let rec shows_list_prod _A _B = showsp_list (shows_prec_prod _A _B) zero_nata;;

let rec show_prod _A _B =
  ({shows_prec = shows_prec_prod _A _B; shows_list = shows_list_prod _A _B} :
    ('a * 'b) show);;

let rec def_hashmap_size_prod _A _B
  = (fun _ -> plus_nata (def_hashmap_size _A Type) (def_hashmap_size _B Type));;

let rec hashcode_prod _A _B
  x = Int32.add (Int32.mul (hashcode _A (fst x)) (uint32 (Z.Int.of_int 33)))
        (hashcode _B (snd x));;

let rec hashable_prod _A _B =
  ({hashcode = hashcode_prod _A _B;
     def_hashmap_size = def_hashmap_size_prod _A _B}
    : ('a * 'b) hashable);;

type ('a, 'b) bexp = True | Not of ('a, 'b) bexp |
  And of ('a, 'b) bexp * ('a, 'b) bexp | Or of ('a, 'b) bexp * ('a, 'b) bexp |
  Imply of ('a, 'b) bexp * ('a, 'b) bexp | Eq of ('a, 'b) exp * ('a, 'b) exp |
  Lea of ('a, 'b) exp * ('a, 'b) exp | Lta of ('a, 'b) exp * ('a, 'b) exp |
  Ge of ('a, 'b) exp * ('a, 'b) exp | Gt of ('a, 'b) exp * ('a, 'b) exp
and ('a, 'b) exp = Const of 'b | Var of 'a |
  If_then_else of ('a, 'b) bexp * ('a, 'b) exp * ('a, 'b) exp |
  Binop of ('b -> 'b -> 'b) * ('a, 'b) exp * ('a, 'b) exp |
  Unop of ('b -> 'b) * ('a, 'b) exp;;

let rec shows_exp _A _B
  = function Const c -> shows_prec _B zero_nata c []
    | Var v -> shows_prec _A zero_nata v []
    | If_then_else (b, e1, e2) ->
        shows_bexp _A _B b @
          [Chara (false, false, false, false, false, true, false, false);
            Chara (true, true, true, true, true, true, false, false);
            Chara (false, false, false, false, false, true, false, false)] @
            shows_exp _A _B e1 @
              [Chara (false, false, false, false, false, true, false, false);
                Chara (false, true, false, true, true, true, false, false);
                Chara (false, false, false, false, false, true, false, false)] @
                shows_exp _A _B e2
    | Binop (uu, e1, e2) ->
        [Chara (false, true, false, false, false, true, true, false);
          Chara (true, false, false, true, false, true, true, false);
          Chara (false, true, true, true, false, true, true, false);
          Chara (true, true, true, true, false, true, true, false);
          Chara (false, false, false, false, true, true, true, false);
          Chara (false, false, false, false, false, true, false, false)] @
          shows_exp _A _B e1 @
            [Chara (false, false, false, false, false, true, false, false)] @
              shows_exp _A _B e2
    | Unop (uv, e) ->
        [Chara (true, false, true, false, true, true, true, false);
          Chara (false, true, true, true, false, true, true, false);
          Chara (true, true, true, true, false, true, true, false);
          Chara (false, false, false, false, true, true, true, false);
          Chara (false, false, false, false, false, true, false, false)] @
          shows_exp _A _B e
and shows_bexp _A _B
  = function
    Lta (a, b) ->
      shows_exp _A _B a @
        [Chara (false, false, false, false, false, true, false, false);
          Chara (false, false, true, true, true, true, false, false);
          Chara (false, false, false, false, false, true, false, false)] @
          shows_exp _A _B b
    | Lea (a, b) ->
        shows_exp _A _B a @
          [Chara (false, false, false, false, false, true, false, false);
            Chara (false, false, true, true, true, true, false, false);
            Chara (true, false, true, true, true, true, false, false);
            Chara (false, false, false, false, false, true, false, false)] @
            shows_exp _A _B b
    | Eq (a, b) ->
        shows_exp _A _B a @
          [Chara (false, false, false, false, false, true, false, false);
            Chara (true, false, true, true, true, true, false, false);
            Chara (false, false, false, false, false, true, false, false)] @
            shows_exp _A _B b
    | Ge (a, b) ->
        shows_exp _A _B a @
          [Chara (false, false, false, false, false, true, false, false);
            Chara (false, true, true, true, true, true, false, false);
            Chara (true, false, true, true, true, true, false, false);
            Chara (false, false, false, false, false, true, false, false)] @
            shows_exp _A _B b
    | Gt (a, b) ->
        shows_exp _A _B a @
          [Chara (false, false, false, false, false, true, false, false);
            Chara (false, true, true, true, true, true, false, false);
            Chara (false, false, false, false, false, true, false, false)] @
            shows_exp _A _B b
    | True ->
        [Chara (false, false, true, false, true, true, true, false);
          Chara (false, true, false, false, true, true, true, false);
          Chara (true, false, true, false, true, true, true, false);
          Chara (true, false, true, false, false, true, true, false)]
    | Not b ->
        [Chara (true, false, false, false, false, true, false, false);
          Chara (false, false, false, false, false, true, false, false)] @
          shows_bexp _A _B b
    | And (a, b) ->
        shows_bexp _A _B a @
          [Chara (false, false, false, false, false, true, false, false);
            Chara (false, true, true, false, false, true, false, false);
            Chara (false, true, true, false, false, true, false, false);
            Chara (false, false, false, false, false, true, false, false)] @
            shows_bexp _A _B b
    | Or (a, b) ->
        shows_bexp _A _B a @
          [Chara (false, false, false, false, false, true, false, false);
            Chara (false, false, true, true, true, true, true, false);
            Chara (false, false, true, true, true, true, true, false);
            Chara (false, false, false, false, false, true, false, false)] @
            shows_bexp _A _B b
    | Imply (a, b) ->
        shows_bexp _A _B a @
          [Chara (false, false, false, false, false, true, false, false);
            Chara (true, false, true, true, false, true, false, false);
            Chara (false, true, true, true, true, true, false, false);
            Chara (false, false, false, false, false, true, false, false)] @
            shows_bexp _A _B b;;

let rec shows_prec_exp _A _B p e rest = shows_exp _A _B e @ rest;;

let rec intersperse
  sep x1 = match sep, x1 with
    sep, x :: y :: xs -> x :: sep :: intersperse sep (y :: xs)
    | uu, [] -> []
    | uu, [v] -> [v];;

let rec concat xss = foldr (fun a b -> a @ b) xss [];;

let rec shows_list_exp _A _B
  es s =
    [Chara (true, true, false, true, true, false, true, false)] @
      concat
        (intersperse
          [Chara (false, false, true, true, false, true, false, false);
            Chara (false, false, false, false, false, true, false, false)]
          (map (shows_exp _A _B) es)) @
        [Chara (true, false, true, true, true, false, true, false)] @ s;;

let rec show_exp _A _B =
  ({shows_prec = shows_prec_exp _A _B; shows_list = shows_list_exp _A _B} :
    ('a, 'b) exp show);;

type ('a, 'b) acconstraint = LT of 'a * 'b | LE of 'a * 'b | EQ of 'a * 'b |
  GT of 'a * 'b | GE of 'a * 'b;;

let rec showsp_acconstraint
  show_c show_t p x3 = match show_c, show_t, p, x3 with
    show_c, show_t, p, GE (x, y) ->
      comp (comp (comp (comp (comp (comp (shows_pl p)
                                     (shows_string
                                       [Chara
  (true, true, true, false, false, false, true, false);
 Chara (true, false, true, false, false, false, true, false)]))
                               shows_space)
                         (show_c one_nata x))
                   shows_space)
             (show_t one_nata y))
        (shows_pr p)
    | show_c, show_t, p, GT (x, y) ->
        comp (comp (comp (comp (comp (comp (shows_pl p)
                                       (shows_string
 [Chara (true, true, true, false, false, false, true, false);
   Chara (false, false, true, false, true, false, true, false)]))
                                 shows_space)
                           (show_c one_nata x))
                     shows_space)
               (show_t one_nata y))
          (shows_pr p)
    | show_c, show_t, p, EQ (x, y) ->
        comp (comp (comp (comp (comp (comp (shows_pl p)
                                       (shows_string
 [Chara (true, false, true, false, false, false, true, false);
   Chara (true, false, false, false, true, false, true, false)]))
                                 shows_space)
                           (show_c one_nata x))
                     shows_space)
               (show_t one_nata y))
          (shows_pr p)
    | show_c, show_t, p, LE (x, y) ->
        comp (comp (comp (comp (comp (comp (shows_pl p)
                                       (shows_string
 [Chara (false, false, true, true, false, false, true, false);
   Chara (true, false, true, false, false, false, true, false)]))
                                 shows_space)
                           (show_c one_nata x))
                     shows_space)
               (show_t one_nata y))
          (shows_pr p)
    | show_c, show_t, p, LT (x, y) ->
        comp (comp (comp (comp (comp (comp (shows_pl p)
                                       (shows_string
 [Chara (false, false, true, true, false, false, true, false);
   Chara (false, false, true, false, true, false, true, false)]))
                                 shows_space)
                           (show_c one_nata x))
                     shows_space)
               (show_t one_nata y))
          (shows_pr p);;

let rec shows_prec_acconstraint _A _B
  = showsp_acconstraint (shows_prec _A) (shows_prec _B);;

let rec shows_list_acconstraint _A _B
  = showsp_list (shows_prec_acconstraint _A _B) zero_nata;;

let rec show_acconstraint _A _B =
  ({shows_prec = shows_prec_acconstraint _A _B;
     shows_list = shows_list_acconstraint _A _B}
    : ('a, 'b) acconstraint show);;

let rec shows_prec_bexp _A _B p e rest = shows_bexp _A _B e @ rest;;

let rec shows_list_bexp _A _B
  es s =
    [Chara (true, true, false, true, true, false, true, false)] @
      concat
        (intersperse
          [Chara (false, false, true, true, false, true, false, false);
            Chara (false, false, false, false, false, true, false, false)]
          (map (shows_bexp _A _B) es)) @
        [Chara (true, false, true, true, true, false, true, false)] @ s;;

let rec show_bexp _A _B =
  ({shows_prec = shows_prec_bexp _A _B; shows_list = shows_list_bexp _A _B} :
    ('a, 'b) bexp show);;

type 'a set = Set of 'a list | Coset of 'a list;;

type ('a, 'b) sum = Inl of 'a | Inr of 'b;;

type 'a iarray = IArray of 'a list;;

type message = ExploredState;;

type ('a, 'b) hashtable = HashTable of (('a * 'b) list) array * nat;;

type 'a result = Result of 'a | Error of string list;;

type 'a len_list = LL of nat * 'a list;;

type ('a, 'b) hashmap = HashMap of (('a * 'b) list) FArray.IsabelleMapping.array_type * nat;;

type 'a label = Del | Internal of 'a | Bin of 'a | Broad of 'a;;

type ('a, 'b, 'c, 'd) gen_g_impl_ext = Gen_g_impl_ext of 'a * 'b * 'c * 'd;;

type resulta = Renaming_Failed | Preconds_Unsat | Sat | Unsat;;

type ('a, 'b, 'c, 'd) sexp = Truea | Nota of ('a, 'b, 'c, 'd) sexp |
  Anda of ('a, 'b, 'c, 'd) sexp * ('a, 'b, 'c, 'd) sexp |
  Ora of ('a, 'b, 'c, 'd) sexp * ('a, 'b, 'c, 'd) sexp |
  Implya of ('a, 'b, 'c, 'd) sexp * ('a, 'b, 'c, 'd) sexp | Eqa of 'c * 'd |
  Leb of 'c * 'd | Ltb of 'c * 'd | Gea of 'c * 'd | Gta of 'c * 'd |
  Loc of 'a * 'b;;

type ('a, 'b, 'c, 'd) formula = EX of ('a, 'b, 'c, 'd) sexp |
  EG of ('a, 'b, 'c, 'd) sexp | AX of ('a, 'b, 'c, 'd) sexp |
  AG of ('a, 'b, 'c, 'd) sexp |
  Leadsto of ('a, 'b, 'c, 'd) sexp * ('a, 'b, 'c, 'd) sexp;;

let rec suc n = plus_nata n one_nata;;

let rec list_ex p x1 = match p, x1 with p, [] -> false
                  | p, x :: xs -> p x || list_ex p xs;;

let rec bex (Set xs) p = list_ex p xs;;

let rec minus_nat
  m n = Nat (max ord_integer Z.Int.zero
              (Z.Int.sub (integer_of_nat m) (integer_of_nat n)));;

let rec nth
  (x :: xs) n =
    (if equal_nata n zero_nata then x else nth xs (minus_nat n one_nata));;

let rec fold f x1 s = match f, x1, s with f, x :: xs, s -> fold f xs (f x s)
               | f, [], s -> s;;

let rec rev xs = fold (fun a b -> a :: b) xs [];;

let rec upt i j = (if less_nat i j then i :: upt (suc i) j else []);;

let rec list_all p x1 = match p, x1 with p, [] -> true
                   | p, x :: xs -> p x && list_all p xs;;

let rec ball (Set xs) p = list_all p xs;;

let rec len _A
  a = (fun f_ () -> f_ (((fun () -> Z.Int.of_int (Array.length a))) ()) ())
        (fun i -> (fun () -> (nat_of_integer i)));;

let rec newa _A
  = comp (fun a b -> (fun () -> Array.make (Z.Int.to_int a) b)) integer_of_nat;;

let rec ntha _A a n = (fun () -> Array.get a (Z.Int.to_int (integer_of_nat n)));;

let rec upd _A
  i x a =
    (fun f_ () -> f_ (((fun () -> Array.set a (Z.Int.to_int (integer_of_nat i)) x))
      ()) ())
      (fun _ -> (fun () -> a));;

let rec maps f x1 = match f, x1 with f, [] -> []
               | f, x :: xs -> f x @ maps f xs;;

let rec take
  n x1 = match n, x1 with n, [] -> []
    | n, x :: xs ->
        (if equal_nata n zero_nata then []
          else x :: take (minus_nat n one_nata) xs);;

let rec image f (Set xs) = Set (map f xs);;

let rec make _A
  n f = (fun () -> Array.init (Z.Int.to_int (integer_of_nat n)) (fun k_ ->
          (comp f nat_of_integer) (Z.Int.of_int k_)));;

let rec inj_on _A _B
  f a = ball a
          (fun x ->
            ball a (fun y -> (if eq _B (f x) (f y) then eq _A x y else true)));;

let rec suba (IArray asa, n) = nth asa (nat_of_integer n);;

let rec sub asa n = suba (asa, integer_of_nat n);;

let rec map_of _A
  x0 k = match x0, k with
    (l, v) :: ps, k -> (if eq _A l k then Some v else map_of _A ps k)
    | [], k -> None;;

let rec removeAll _A
  x xa1 = match x, xa1 with x, [] -> []
    | x, y :: xs ->
        (if eq _A x y then removeAll _A x xs else y :: removeAll _A x xs);;

let rec membera _A x0 y = match x0, y with [], y -> false
                     | x :: xs, y -> eq _A x y || membera _A xs y;;

let rec inserta _A x xs = (if membera _A xs x then xs else x :: xs);;

let rec insert _A
  x xa1 = match x, xa1 with x, Coset xs -> Coset (removeAll _A x xs)
    | x, Set xs -> Set (inserta _A x xs);;

let rec member _A
  x xa1 = match x, xa1 with x, Coset xs -> not (membera _A xs x)
    | x, Set xs -> membera _A xs x;;

let rec remove _A
  x xa1 = match x, xa1 with x, Coset xs -> Coset (inserta _A x xs)
    | x, Set xs -> Set (removeAll _A x xs);;

let rec fun_upd _A f a b = (fun x -> (if eq _A x a then b else f x));;

let rec filter
  p x1 = match p, x1 with p, [] -> []
    | p, x :: xs -> (if p x then x :: filter p xs else filter p xs);;

let rec foldli
  x0 c f sigma = match x0, c, f, sigma with [], c, f, sigma -> sigma
    | x :: xs, c, f, sigma ->
        (if c sigma then foldli xs c f (f x sigma) else sigma);;

let rec extract
  p x1 = match p, x1 with
    p, x :: xs ->
      (if p x then Some ([], (x, xs))
        else (match extract p xs with None -> None
               | Some (ys, (y, zs)) -> Some (x :: ys, (y, zs))))
    | p, [] -> None;;

let rec hd (x21 :: x22) = x21;;

let rec tl = function [] -> []
             | x21 :: x22 -> x22;;

let rec remdups _A
  = function [] -> []
    | x :: xs ->
        (if membera _A xs x then remdups _A xs else x :: remdups _A xs);;

let rec uncurry f = (fun (a, b) -> f a b);;

let rec distinct _A = function [] -> true
                      | x :: xs -> not (membera _A xs x) && distinct _A xs;;

let rec trace m x = (let _ = (fun x -> Tracing.count_up ()) m in x);;

let rec replicate
  n (x: 'a): 'a list = (if equal_nata n zero_nata then []
          else x :: replicate (minus_nat n one_nata) x);;

let rec is_none = function Some x -> false
                  | None -> true;;

let rec implode
  cs = (let xs = (List.map integer_of_char
                   cs)
      and chr k =
        let l = Z.Int.to_int k
          in if 0 <= l && l < 128
          then Char.chr l
          else failwith "Non-ASCII character in literal"
      in String.init (List.length xs) (List.nth (List.map chr xs)));;

let rec implode
  cs = (let xs = (map_rev integer_of_char []
                   cs)
      and chr k =
        let l = Z.Int.to_int k
          in if 0 <= l && l < 128
          then Char.chr l
          else failwith "Non-ASCII character in literal"
      in String.init (List.length xs) (List.nth (map_rev chr [] xs)));;


let rec tracea x = trace ExploredState x;;

let blita _ _ _ _ _ = failwith "Array_Blit.blit\039";;

let array_blit src si dst di len = Array.blit src (Z.Int.to_int si) dst (Z.Int.to_int di) (Z.Int.to_int len);;

let rec blit _A
  src si dst di len () =
    array_blit src (integer_of_nat si) dst (integer_of_nat di) (integer_of_nat len);;

let rec v_dbm (_A1, _A2, _A3) _B
  n = (fun (i, j) ->
        (if eq _A2 i j ||
              (eq _A2 i (zero _A1) && less _A3 (zero _A1) j ||
                (less _A3 n i || less _A3 n j))
          then zero_DBMEntrya _B else INF));;

let rec imp_fora
  i u f s =
    (if less_eq_nat u i then (fun () -> s)
      else (fun f_ () -> f_ ((f i s) ()) ())
             (imp_fora (plus_nata i one_nata) u f));;

let rec mtx_set _A
  m mtx e v = upd _A (plus_nata (times_nat (fst e) m) (snd e)) v mtx;;

let rec mtx_get _A
  m mtx e = ntha _A mtx (plus_nata (times_nat (fst e) m) (snd e));;

let rec fw_upd_impl (_A1, _A2)
  n = (fun ai bib bia bi ->
        (fun f_ () -> f_ ((mtx_get _A2 (suc n) ai (bia, bib)) ()) ())
          (fun x ->
            (fun f_ () -> f_ ((mtx_get _A2 (suc n) ai (bib, bi)) ()) ())
              (fun xa ->
                (let xb =
                   plus _A1.ordered_comm_monoid_add_linordered_ab_monoid_add.comm_monoid_add_ordered_comm_monoid_add.monoid_add_comm_monoid_add.semigroup_add_monoid_add.plus_semigroup_add
                     x xa
                   in
                  (fun f_ () -> f_ ((mtx_get _A2 (suc n) ai (bia, bi)) ()) ())
                    (fun xaa ->
                      (if less _A1.linordered_ab_semigroup_add_linordered_ab_monoid_add.linorder_linordered_ab_semigroup_add.order_linorder.preorder_order.ord_preorder
                            xb xaa
                        then mtx_set _A2 (suc n) ai (bia, bi) xb
                        else (fun () -> ai)))))));;

let rec fw_impl (_A1, _A2)
  n = imp_fora zero_nata (plus_nata n one_nata)
        (fun xb ->
          imp_fora zero_nata (plus_nata n one_nata)
            (fun xd ->
              imp_fora zero_nata (plus_nata n one_nata)
                (fun xf sigma -> fw_upd_impl (_A1, _A2) n sigma xb xd xf)));;

let rec gen_length n x1 = match n, x1 with n, x :: xs -> gen_length (suc n) xs
                     | n, [] -> n;;

let rec map_filter
  f x1 = match f, x1 with f, [] -> []
    | f, x :: xs ->
        (match f x with None -> map_filter f xs
          | Some y -> y :: map_filter f xs);;

let cODE_ABORT _ = failwith "Misc.CODE_ABORT";;

let rec bind m f = (match m with Inl a -> Inl a | Inr a -> f a);;

let rec fwi_impl (_A1, _A2)
  n = (fun ai bi ->
        imp_fora zero_nata (plus_nata n one_nata)
          (fun xa ->
            imp_fora zero_nata (plus_nata n one_nata)
              (fun xc sigma -> fw_upd_impl (_A1, _A2) n sigma bi xa xc))
          ai);;

let rec the (Some x2) = x2;;

let rec gen_pick
  it s =
    the (it s (fun a -> (match a with None -> true | Some _ -> false))
           (fun x _ -> Some x)
          None);;

let rec ll_fuel (LL (x1, x2)) = x1;;

let rec ensure_cparser
  p = (fun ts ->
        bind (p ts)
          (fun (x, tsa) ->
            (if less_nat (ll_fuel tsa) (ll_fuel ts) then Inr (x, tsa)
              else Inl (fun _ ->
                         shows_prec_list show_char zero_nata
                           [Chara (false, false, true, false, false, false,
                                    true, false);
                             Chara (true, false, false, true, true, true, true,
                                     false);
                             Chara (false, true, true, true, false, true, true,
                                     false);
                             Chara (true, false, false, false, false, true,
                                     true, false);
                             Chara (true, false, true, true, false, true, true,
                                     false);
                             Chara (true, false, false, true, false, true, true,
                                     false);
                             Chara (true, true, false, false, false, true, true,
                                     false);
                             Chara (false, false, false, false, false, true,
                                     false, false);
                             Chara (false, false, false, false, true, true,
                                     true, false);
                             Chara (true, false, false, false, false, true,
                                     true, false);
                             Chara (false, true, false, false, true, true, true,
                                     false);
                             Chara (true, true, false, false, true, true, true,
                                     false);
                             Chara (true, false, true, false, false, true, true,
                                     false);
                             Chara (false, true, false, false, true, true, true,
                                     false);
                             Chara (false, false, false, false, false, true,
                                     false, false);
                             Chara (true, true, false, false, false, true, true,
                                     false);
                             Chara (false, false, false, true, false, true,
                                     true, false);
                             Chara (true, false, true, false, false, true, true,
                                     false);
                             Chara (true, true, false, false, false, true, true,
                                     false);
                             Chara (true, true, false, true, false, true, true,
                                     false);
                             Chara (false, false, false, false, false, true,
                                     false, false);
                             Chara (false, true, true, false, false, true, true,
                                     false);
                             Chara (true, false, false, false, false, true,
                                     true, false);
                             Chara (true, false, false, true, false, true, true,
                                     false);
                             Chara (false, false, true, true, false, true, true,
                                     false);
                             Chara (true, false, true, false, false, true, true,
                                     false);
                             Chara (false, false, true, false, false, true,
                                     true, false)]))));;

let rec sum_join = function Inl x -> x
                   | Inr x -> x;;

let rec return x = (fun ts -> Inr (x, ts));;

let rec ensure_parser
  p = (fun ts ->
        bind (p ts)
          (fun (x, tsa) ->
            (if less_eq_nat (ll_fuel tsa) (ll_fuel ts) then Inr (x, tsa)
              else Inl (fun _ ->
                         shows_prec_list show_char zero_nata
                           [Chara (false, false, true, false, false, false,
                                    true, false);
                             Chara (true, false, false, true, true, true, true,
                                     false);
                             Chara (false, true, true, true, false, true, true,
                                     false);
                             Chara (true, false, false, false, false, true,
                                     true, false);
                             Chara (true, false, true, true, false, true, true,
                                     false);
                             Chara (true, false, false, true, false, true, true,
                                     false);
                             Chara (true, true, false, false, false, true, true,
                                     false);
                             Chara (false, false, false, false, false, true,
                                     false, false);
                             Chara (false, false, false, false, true, true,
                                     true, false);
                             Chara (true, false, false, false, false, true,
                                     true, false);
                             Chara (false, true, false, false, true, true, true,
                                     false);
                             Chara (true, true, false, false, true, true, true,
                                     false);
                             Chara (true, false, true, false, false, true, true,
                                     false);
                             Chara (false, true, false, false, true, true, true,
                                     false);
                             Chara (false, false, false, false, false, true,
                                     false, false);
                             Chara (true, true, false, false, false, true, true,
                                     false);
                             Chara (false, false, false, true, false, true,
                                     true, false);
                             Chara (true, false, true, false, false, true, true,
                                     false);
                             Chara (true, true, false, false, false, true, true,
                                     false);
                             Chara (true, true, false, true, false, true, true,
                                     false);
                             Chara (false, false, false, false, false, true,
                                     false, false);
                             Chara (false, true, true, false, false, true, true,
                                     false);
                             Chara (true, false, false, false, false, true,
                                     true, false);
                             Chara (true, false, false, true, false, true, true,
                                     false);
                             Chara (false, false, true, true, false, true, true,
                                     false);
                             Chara (true, false, true, false, false, true, true,
                                     false);
                             Chara (false, false, true, false, false, true,
                                     true, false)]))));;

let rec bindb
  m f = (fun ts ->
          bind (ensure_parser m ts) (fun (x, a) -> ensure_parser (f x) a));;

let rec catch_error m f = (match m with Inl a -> f a | Inr a -> Inr a);;

let rec alt
  p1 p2 l =
    catch_error (bind (p1 l) (fun (r, la) -> Inr (Inl r, la)))
      (fun e1 ->
        catch_error (bind (p2 l) (fun (r, la) -> Inr (Inr r, la)))
          (fun e2 ->
            Inl (fun _ ->
                  comp (comp (e1 ())
                         (shows_prec_list show_char zero_nata
                           [Chara (false, true, false, true, false, false,
                                    false, false);
                             Chara (false, false, false, false, false, true,
                                     false, false);
                             Chara (false, false, false, false, false, true,
                                     false, false);
                             Chara (false, false, true, true, true, true, true,
                                     false);
                             Chara (false, false, false, false, false, true,
                                     false, false)]))
                    (e2 ()))));;

let rec repeat
  p l = bindb (alt (bindb (ensure_cparser p)
                     (fun a -> bindb (repeat p) (fun b -> return (a :: b))))
                (return []))
          (fun x -> return (sum_join x)) l;;

let rec ll_list (LL (x1, x2)) = x2;;

let rec get_tokens x = (fun ll -> Inr (ll_list ll, ll)) x;;

let rec error_aux e = (fun _ -> Inl e);;

let rec shows_quote
  s = comp (comp (shows_prec_char zero_nata
                   (Chara (true, true, true, false, false, true, false, false)))
             s)
        (shows_prec_char zero_nata
          (Chara (true, true, true, false, false, true, false, false)));;

let rec err_expecting_aux _A
  msg = bindb get_tokens
          (fun ts ->
            error_aux
              (fun _ ->
                comp (comp (comp (shows_string
                                   [Chara (true, false, true, false, false,
    true, true, false);
                                     Chara (false, false, false, true, true,
     true, true, false);
                                     Chara (false, false, false, false, true,
     true, true, false);
                                     Chara (true, false, true, false, false,
     true, true, false);
                                     Chara (true, true, false, false, false,
     true, true, false);
                                     Chara (false, false, true, false, true,
     true, true, false);
                                     Chara (true, false, false, true, false,
     true, true, false);
                                     Chara (false, true, true, true, false,
     true, true, false);
                                     Chara (true, true, true, false, false,
     true, true, false);
                                     Chara (false, false, false, false, false,
     true, false, false)])
                             (msg ()))
                       (shows_string
                         [Chara (false, false, true, true, false, true, false,
                                  false);
                           Chara (false, false, false, false, false, true,
                                   false, false);
                           Chara (false, true, false, false, false, true, true,
                                   false);
                           Chara (true, false, true, false, true, true, true,
                                   false);
                           Chara (false, false, true, false, true, true, true,
                                   false);
                           Chara (false, false, false, false, false, true,
                                   false, false);
                           Chara (false, true, true, false, false, true, true,
                                   false);
                           Chara (true, true, true, true, false, true, true,
                                   false);
                           Chara (true, false, true, false, true, true, true,
                                   false);
                           Chara (false, true, true, true, false, true, true,
                                   false);
                           Chara (false, false, true, false, false, true, true,
                                   false);
                           Chara (false, true, false, true, true, true, false,
                                   false);
                           Chara (false, false, false, false, false, true,
                                   false, false)]))
                  (shows_quote
                    (shows_prec_list _A zero_nata
                      (take (nat_of_integer (Z.Int.of_int 100)) ts)))));;

let rec get
  ll = (let LL (nat, lista) = ll in
         (if equal_nata nat zero_nata
           then Inl (fun _ ->
                      shows_string
                        [Chara (true, false, true, false, false, false, true,
                                 false);
                          Chara (false, false, false, true, true, true, true,
                                  false);
                          Chara (false, false, false, false, true, true, true,
                                  false);
                          Chara (true, false, true, false, false, true, true,
                                  false);
                          Chara (true, true, false, false, false, true, true,
                                  false);
                          Chara (false, false, true, false, true, true, true,
                                  false);
                          Chara (true, false, false, true, false, true, true,
                                  false);
                          Chara (false, true, true, true, false, true, true,
                                  false);
                          Chara (true, true, true, false, false, true, true,
                                  false);
                          Chara (false, false, false, false, false, true, false,
                                  false);
                          Chara (true, false, true, true, false, true, true,
                                  false);
                          Chara (true, true, true, true, false, true, true,
                                  false);
                          Chara (false, true, false, false, true, true, true,
                                  false);
                          Chara (true, false, true, false, false, true, true,
                                  false);
                          Chara (false, false, false, false, false, true, false,
                                  false);
                          Chara (true, false, false, true, false, true, true,
                                  false);
                          Chara (false, true, true, true, false, true, true,
                                  false);
                          Chara (false, false, false, false, true, true, true,
                                  false);
                          Chara (true, false, true, false, true, true, true,
                                  false);
                          Chara (false, false, true, false, true, true, true,
                                  false)])
           else (match lista
                  with [] ->
                    Inl (fun _ ->
                          shows_string
                            [Chara (true, false, true, false, false, false,
                                     true, false);
                              Chara (false, false, false, true, true, true,
                                      true, false);
                              Chara (false, false, false, false, true, true,
                                      true, false);
                              Chara (true, false, true, false, false, true,
                                      true, false);
                              Chara (true, true, false, false, false, true,
                                      true, false);
                              Chara (false, false, true, false, true, true,
                                      true, false);
                              Chara (true, false, false, true, false, true,
                                      true, false);
                              Chara (false, true, true, true, false, true, true,
                                      false);
                              Chara (true, true, true, false, false, true, true,
                                      false);
                              Chara (false, false, false, false, false, true,
                                      false, false);
                              Chara (true, false, true, true, false, true, true,
                                      false);
                              Chara (true, true, true, true, false, true, true,
                                      false);
                              Chara (false, true, false, false, true, true,
                                      true, false);
                              Chara (true, false, true, false, false, true,
                                      true, false);
                              Chara (false, false, false, false, false, true,
                                      false, false);
                              Chara (true, false, false, true, false, true,
                                      true, false);
                              Chara (false, true, true, true, false, true, true,
                                      false);
                              Chara (false, false, false, false, true, true,
                                      true, false);
                              Chara (true, false, true, false, true, true, true,
                                      false);
                              Chara (false, false, true, false, true, true,
                                      true, false)])
                  | x :: xs -> Inr (x, LL (minus_nat nat one_nata, xs)))));;

let rec any (_A1, _A2)
  ts = bindb get
         (fun t ->
           (if membera _A1 ts t then return t
             else err_expecting_aux _A2
                    (fun _ ->
                      comp (shows_string
                             [Chara (true, true, true, true, false, false, true,
                                      false);
                               Chara (false, true, true, true, false, true,
                                       true, false);
                               Chara (true, false, true, false, false, true,
                                       true, false);
                               Chara (false, false, false, false, false, true,
                                       false, false);
                               Chara (true, true, true, true, false, true, true,
                                       false);
                               Chara (false, true, true, false, false, true,
                                       true, false);
                               Chara (false, false, false, false, false, true,
                                       false, false)])
                        (shows_prec_list _A2 zero_nata ts))));;

let rec lx_ws
  x = repeat
        (any (equal_char, show_char)
          [Chara (false, false, false, false, false, true, false, false);
            Chara (false, true, false, true, false, false, false, false);
            Chara (true, false, false, true, false, false, false, false);
            Chara (true, false, true, true, false, false, false, false)])
        x;;

let rec exactly (_A1, _A2)
  ts = bindb (alt (foldr
                    (fun t p ->
                      bindb get
                        (fun x ->
                          (if eq _A1 x t
                            then bindb p (fun xa -> return (x :: xa))
                            else error_aux (fun _ -> id))))
                    ts (return []))
               (err_expecting_aux _A2
                 (fun _ ->
                   comp (shows_string
                          [Chara (true, false, true, false, false, false, true,
                                   false);
                            Chara (false, false, false, true, true, true, true,
                                    false);
                            Chara (true, false, false, false, false, true, true,
                                    false);
                            Chara (true, true, false, false, false, true, true,
                                    false);
                            Chara (false, false, true, false, true, true, true,
                                    false);
                            Chara (false, false, true, true, false, true, true,
                                    false);
                            Chara (true, false, false, true, true, true, true,
                                    false);
                            Chara (false, false, false, false, false, true,
                                    false, false)])
                     (shows_prec_list _A2 zero_nata ts))))
         (fun x -> return (sum_join x));;

let rec bracket_close
  x = bindb lx_ws
        (fun _ ->
          exactly (equal_char, show_char)
            [Chara (true, false, true, true, true, false, true, false)])
        x;;

let rec bracket_open
  x = bindb lx_ws
        (fun _ ->
          exactly (equal_char, show_char)
            [Chara (true, true, false, true, true, false, true, false)])
        x;;

let rec chainL1
  a f = bindb a
          (fun x ->
            bindb (repeat
                    (bindb f
                      (fun aa ->
                        bindb a (fun b -> return (fun ab -> aa ab b)))))
              (fun xs -> return (foldl (fun aa fa -> fa aa) x xs)));;

let rec comma
  x = bindb lx_ws
        (fun _ ->
          exactly (equal_char, show_char)
            [Chara (false, false, true, true, false, true, false, false)])
        x;;

let rec parse_list
  a = chainL1 (bindb a (fun x -> return [x]))
        (bindb comma (fun _ -> return (fun aa b -> aa @ b)));;

let rec brace_close
  x = bindb lx_ws
        (fun _ ->
          exactly (equal_char, show_char)
            [Chara (true, false, true, true, true, true, true, false)])
        x;;

let rec json_character
  x = bindb get
        (fun xa ->
          (if not (membera equal_char
                    [Chara (false, true, false, false, false, true, false,
                             false);
                      Chara (false, true, false, false, true, false, false,
                              true);
                      Chara (false, true, false, true, false, false, false,
                              false)]
                    xa)
            then return xa
            else err_expecting_aux show_char
                   (fun _ ->
                     shows_string
                       [Chara (false, true, false, true, false, false, true,
                                false);
                         Chara (true, true, false, false, true, false, true,
                                 false);
                         Chara (true, true, true, true, false, false, true,
                                 false);
                         Chara (false, true, true, true, false, false, true,
                                 false);
                         Chara (false, false, false, false, false, true, false,
                                 false);
                         Chara (true, true, false, false, true, true, true,
                                 false);
                         Chara (false, false, true, false, true, true, true,
                                 false);
                         Chara (false, true, false, false, true, true, true,
                                 false);
                         Chara (true, false, false, true, false, true, true,
                                 false);
                         Chara (false, true, true, true, false, true, true,
                                 false);
                         Chara (true, true, true, false, false, true, true,
                                 false);
                         Chara (false, false, false, false, false, true, false,
                                 false);
                         Chara (true, true, false, false, false, true, true,
                                 false);
                         Chara (false, false, false, true, false, true, true,
                                 false);
                         Chara (true, false, false, false, false, true, true,
                                 false);
                         Chara (false, true, false, false, true, true, true,
                                 false);
                         Chara (true, false, false, false, false, true, true,
                                 false);
                         Chara (true, true, false, false, false, true, true,
                                 false);
                         Chara (false, false, true, false, true, true, true,
                                 false);
                         Chara (true, false, true, false, false, true, true,
                                 false);
                         Chara (false, true, false, false, true, true, true,
                                 false)])))
        x;;

let rec identifier
  x = bindb (exactly (equal_char, show_char)
              [Chara (false, true, false, false, false, true, false, false)])
        (fun _ ->
          bindb json_character
            (fun xa ->
              bindb (repeat json_character)
                (fun xaa ->
                  bindb (exactly (equal_char, show_char)
                          [Chara (false, true, false, false, false, true, false,
                                   false)])
                    (fun _ -> return (xa :: xaa)))))
        x;;

let rec brace_open
  x = bindb lx_ws
        (fun _ ->
          exactly (equal_char, show_char)
            [Chara (true, true, false, true, true, true, true, false)])
        x;;

let rec colon
  x = bindb lx_ws
        (fun _ ->
          exactly (equal_char, show_char)
            [Chara (false, true, false, true, true, true, false, false)])
        x;;

let rec range (_A1, _A2)
  a b = bindb get
          (fun x ->
            (if less_eq _A1.order_linorder.preorder_order.ord_preorder a x &&
                  less_eq _A1.order_linorder.preorder_order.ord_preorder x b
              then return x
              else err_expecting_aux _A2
                     (fun _ ->
                       comp (comp (comp (shows_string
  [Chara (false, false, true, false, true, false, true, false);
    Chara (true, true, true, true, false, true, true, false);
    Chara (true, true, false, true, false, true, true, false);
    Chara (true, false, true, false, false, true, true, false);
    Chara (false, true, true, true, false, true, true, false);
    Chara (false, false, false, false, false, true, false, false);
    Chara (true, false, false, true, false, true, true, false);
    Chara (false, true, true, true, false, true, true, false);
    Chara (false, false, false, false, false, true, false, false);
    Chara (false, true, false, false, true, true, true, false);
    Chara (true, false, false, false, false, true, true, false);
    Chara (false, true, true, true, false, true, true, false);
    Chara (true, true, true, false, false, true, true, false);
    Chara (true, false, true, false, false, true, true, false);
    Chara (false, false, false, false, false, true, false, false)])
                                    (shows_prec _A2 zero_nata a))
                              (shows_string
                                [Chara (false, false, false, false, false, true,
 false, false);
                                  Chara (true, false, true, true, false, true,
  false, false);
                                  Chara (false, false, false, false, false,
  true, false, false)]))
                         (shows_prec _A2 zero_nata b))));;

let rec lx_digit
  x = range (linorder_char, show_char)
        (Chara (false, false, false, false, true, true, false, false))
        (Chara (true, false, false, true, true, true, false, false)) x;;

let rec lx_nat_aux
  acc l =
    bindb (alt (bindb lx_digit
                 (fun x ->
                   lx_nat_aux
                     (plus_nata (times_nat (nat_of_integer (Z.Int.of_int 10)) acc)
                       (minus_nat (nat_of_char x)
                         (nat_of_char
                           (Chara
                             (false, false, false, false, true, true, false,
                               false)))))))
            (return acc))
      (fun x -> return (sum_join x)) l;;

let rec lx_nat
  x = bindb lx_digit
        (fun xa ->
          lx_nat_aux
            (minus_nat (nat_of_char xa)
              (nat_of_char
                (Chara
                  (false, false, false, false, true, true, false, false)))))
        x;;

let rec lx_int
  x = bindb (alt (bindb
                   (exactly (equal_char, show_char)
                     [Chara (true, false, true, true, false, true, false,
                              false)])
                   (fun _ ->
                     bindb lx_nat
                       (fun xa -> return (comp uminus_inta int_of_nat xa))))
              (bindb lx_nat (fun xa -> return (int_of_nat xa))))
        (fun xa -> return (sum_join xa)) x;;

let rec json_string
  x = bindb (exactly (equal_char, show_char)
              [Chara (false, true, false, false, false, true, false, false)])
        (fun _ ->
          bindb (repeat json_character)
            (fun a ->
              bindb (exactly (equal_char, show_char)
                      [Chara (false, true, false, false, false, true, false,
                               false)])
                (fun _ -> return a)))
        x;;

let rec nats_to_nat
  x xa1 = match x, xa1 with x, [] -> x
    | x, n :: ns ->
        nats_to_nat (plus_nata (times_nat (nat_of_integer (Z.Int.of_int 10)) x) n)
          ns;;

let rec lx_rat
  x = bindb lx_int
        (fun xa ->
          bindb (exactly (equal_char, show_char)
                  [Chara (false, true, true, true, false, true, false, false)])
            (fun _ ->
              bindb lx_digit
                (fun xaa ->
                  bindb (repeat
                          (bindb lx_digit
                            (fun xb ->
                              return
                                (minus_nat (nat_of_char xb)
                                  (nat_of_char
                                    (Chara
                                      (false, false, false, false, true, true,
false, false)))))))
                    (fun xb ->
                      return
                        (if less_eq_int zero_inta xa
                          then Rata (true, xa,
                                      comp int_of_nat (nats_to_nat zero_nata)
(minus_nat (nat_of_char xaa)
   (nat_of_char
     (Chara (false, false, false, false, true, true, false, false))) ::
  xb))
                          else Rata (false, xa,
                                      comp int_of_nat (nats_to_nat zero_nata)
(minus_nat (nat_of_char xaa)
   (nat_of_char
     (Chara (false, false, false, false, true, true, false, false))) ::
  xb)))))))
        x;;

let rec atom
  x = bindb lx_ws
        (fun _ ->
          bindb (alt (bindb json_string (fun xa -> return (Stringa xa)))
                  (bindb
                    (alt (bindb lx_rat (fun xa -> return (Rat xa)))
                      (bindb
                        (alt (bindb lx_nat (fun xa -> return (Nata xa)))
                          (bindb
                            (alt (bindb lx_int (fun xa -> return (Int xa)))
                              (bindb
                                (alt (bindb
                                       (exactly (equal_char, show_char)
 [Chara (false, false, true, false, true, true, true, false);
   Chara (false, true, false, false, true, true, true, false);
   Chara (true, false, true, false, true, true, true, false);
   Chara (true, false, true, false, false, true, true, false)])
                                       (fun _ -> return (Boolean true)))
                                  (bindb
                                    (alt (bindb
   (exactly (equal_char, show_char)
     [Chara (false, true, true, false, false, true, true, false);
       Chara (true, false, false, false, false, true, true, false);
       Chara (false, false, true, true, false, true, true, false);
       Chara (true, true, false, false, true, true, true, false);
       Chara (true, false, true, false, false, true, true, false)])
   (fun _ -> return (Boolean false)))
                                      (bindb
(exactly (equal_char, show_char)
  [Chara (false, true, true, true, false, true, true, false);
    Chara (true, false, true, false, true, true, true, false);
    Chara (false, false, true, true, false, true, true, false);
    Chara (false, false, true, true, false, true, true, false)])
(fun _ -> return Null)))
                                    (fun xa -> return (sum_join xa))))
                                (fun xa -> return (sum_join xa))))
                            (fun xa -> return (sum_join xa))))
                        (fun xa -> return (sum_join xa))))
                    (fun xa -> return (sum_join xa))))
            (fun xa -> return (sum_join xa)))
        x;;

let rec seq
  l = bindb bracket_open
        (fun _ ->
          bindb (alt (parse_list json) (bindb lx_ws (fun _ -> return [])))
            (fun x ->
              bindb bracket_close (fun _ -> return (Arrayb (sum_join x)))))
        l
and json
  l = bindb (alt atom (bindb (alt seq dict) (fun x -> return (sum_join x))))
        (fun x -> return (sum_join x)) l
and dict
  l = bindb brace_open
        (fun _ ->
          bindb (alt (parse_list
                       (bindb lx_ws
                         (fun _ ->
                           bindb identifier
                             (fun a ->
                               bindb colon
                                 (fun _ ->
                                   bindb json (fun b -> return (a, b)))))))
                  (bindb lx_ws (fun _ -> return [])))
            (fun x ->
              bindb brace_close (fun _ -> return (Object (sum_join x)))))
        l;;

let rec list_update
  x0 i y = match x0, i, y with [], i, y -> []
    | x :: xs, i, y ->
        (if equal_nata i zero_nata then y :: xs
          else x :: list_update xs (minus_nat i one_nata) y);;

let rec find_index
  uu x1 = match uu, x1 with uu, [] -> zero_nata
    | p, x :: xs ->
        (if p x then zero_nata else plus_nata (find_index p xs) one_nata);;

let rec index _A xs = (fun a -> find_index (fun x -> eq _A x a) xs);;

let rec of_phantom (Phantom x) = x;;

let rec size_list x = gen_length zero_nata x;;

let rec card (_A1, _A2)
  = function
    Coset xs ->
      minus_nat (of_phantom (card_UNIV _A1)) (size_list (remdups _A2 xs))
    | Set xs -> size_list (remdups _A2 xs);;

(*
let rec ht_new_sz (_A1, _A2) _B
  n = (let l = replicate n [] in
        (fun f_ () -> f_ (((fun () -> Array.of_list l)) ()) ())
          (fun a -> (fun () -> (HashTable (a, zero_nata)))));;
*)
let rec ht_new_sz (_A1, _A2) _B
  n: unit -> ('b, 'c) hashtable = (fun () -> HashTable (Array.of_list (replicate n []: ('b * 'c) list list), zero_nata));;

let rec ht_new (_A1, _A2) _B
  = ht_new_sz (_A1, _A2) _B (def_hashmap_size _A1 Type);;

let rec test_bit_uint32
  x n = less_nat n (nat_of_integer (Z.Int.of_int 32)) &&
          Uint32.test_bit x (integer_of_nat n);;

let rec integer_of_uint32
  n = (if test_bit_uint32 n (nat_of_integer (Z.Int.of_int 31))
        then Z.Int.logor
               (Z.Int.of_int32 (Int32.logand n (uint32 (Z.Int.of_string "2147483647"))))
               (Z.Int.of_string "2147483648")
        else Z.Int.of_int32 n);;

let rec nat_of_uint32 x = nat_of_integer (integer_of_uint32 x);;

let rec nat_of_hashcode x = nat_of_uint32 x;;

let rec bounded_hashcode_nat _A
  n x = modulo_nat (nat_of_hashcode (hashcode _A x)) n;;

let rec the_array (HashTable (a, uu)) = a;;

let rec ls_update _A
  k v x2 = match k, v, x2 with k, v, [] -> ([(k, v)], false)
    | k, v, (l, w) :: ls ->
        (if eq _A k l then ((k, v) :: ls, true)
          else (let r = ls_update _A k v ls in ((l, w) :: fst r, snd r)));;

let rec the_size (HashTable (uu, n)) = n;;

let rec ht_upd (_A1, _A2, _A3) _B
  k v ht =
    (fun f_ () -> f_ ((len (heap_list (heap_prod _A3 _B)) (the_array ht)) ())
      ())
      (fun m ->
        (let i = bounded_hashcode_nat _A2 m k in
          (fun f_ () -> f_
            ((ntha (heap_list (heap_prod _A3 _B)) (the_array ht) i) ()) ())
            (fun l ->
              (let la = ls_update _A1 k v l in
                (fun f_ () -> f_
                  ((upd (heap_list (heap_prod _A3 _B)) i (fst la)
                     (the_array ht))
                  ()) ())
                  (fun _ ->
                    (let n = (if snd la then the_size ht else suc (the_size ht))
                       in
                      (fun () -> (HashTable (the_array ht, n)))))))));;

let top_set : 'a set = Coset [];;

let rec eq_set (_A1, _A2)
  x0 x1 = match x0, x1 with
    Coset xs, Coset ys ->
      list_all (membera _A2 ys) xs && list_all (membera _A2 xs) ys
    | Set xs, Set ys ->
        list_all (membera _A2 ys) xs && list_all (membera _A2 xs) ys
    | Set ys, Coset xs ->
        (let n = card (_A1, _A2) top_set in
          (if equal_nata n zero_nata then false
            else (let xsa = remdups _A2 xs in
                  let ysa = remdups _A2 ys in
                   equal_nata (plus_nata (size_list xsa) (size_list ysa)) n &&
                     (list_all (fun x -> not (membera _A2 ysa x)) xsa &&
                       list_all (fun y -> not (membera _A2 xsa y)) ysa))))
    | Coset xs, Set ys ->
        (let n = card (_A1, _A2) top_set in
          (if equal_nata n zero_nata then false
            else (let xsa = remdups _A2 xs in
                  let ysa = remdups _A2 ys in
                   equal_nata (plus_nata (size_list xsa) (size_list ysa)) n &&
                     (list_all (fun x -> not (membera _A2 ysa x)) xsa &&
                       list_all (fun y -> not (membera _A2 xsa y)) ysa))));;

let rec map_default b f a = (match f a with None -> b | Some ba -> ba);;

let rec ht_insls (_A1, _A2, _A3) _B
  x0 ht = match x0, ht with [], ht -> (fun () -> ht)
    | (k, v) :: l, ht ->
        (fun f_ () -> f_ ((ht_upd (_A1, _A2, _A3) _B k v ht) ()) ())
          (ht_insls (_A1, _A2, _A3) _B l);;

let rec ht_copy (_A1, _A2, _A3) _B
  n src dst =
    (if equal_nata n zero_nata then (fun () -> dst)
      else (fun f_ () -> f_
             ((ntha (heap_list (heap_prod _A3 _B)) (the_array src)
                (minus_nat n one_nata))
             ()) ())
             (fun l ->
               (fun f_ () -> f_ ((ht_insls (_A1, _A2, _A3) _B l dst) ()) ())
                 (ht_copy (_A1, _A2, _A3) _B (minus_nat n one_nata) src)));;

let rec product_lists
  = function [] -> [[]]
    | xs :: xss ->
        maps (fun x -> map (fun a -> x :: a) (product_lists xss)) xs;;

let rec app f a = f a;;

let rec subset (_A1, _A2)
  a b = match a, b with
    Coset xs, Set ys ->
      (let n = card (_A1, _A2) top_set in
        less_nat zero_nata n && equal_nata (card (_A1, _A2) (Set (xs @ ys))) n)
    | Set ys, b -> list_all (fun y -> member _A2 y b) ys
    | a, Coset ys -> list_all (fun y -> not (member _A2 y a)) ys;;

let rec hm_isEmpty ht = (fun () -> (equal_nata (the_size ht) zero_nata));;

let tRACE_impl : (unit -> unit) = (fun () -> (tracea ()));;

let array_geta _ _ = failwith "Diff_Array.array_get\039";;

let rec array_get a = comp (array_geta a) integer_of_nat;;

let array_seta _ _ _ = failwith "Diff_Array.array_set\039";;

let rec array_set a = comp (array_seta a) integer_of_nat;;

let new_arraya _ _ = failwith "Diff_Array.new_array\039";;

let rec new_array v = comp (new_arraya v) integer_of_nat;;

let rec array_get a = comp (FArray.IsabelleMapping.array_get a) integer_of_nat;;

let rec array_set a = comp (FArray.IsabelleMapping.array_set a) integer_of_nat;;

let rec new_array v = comp (FArray.IsabelleMapping.new_array v) integer_of_nat;;

let rec ls_delete _A
  k x1 = match k, x1 with k, [] -> ([], false)
    | k, (l, w) :: ls ->
        (if eq _A k l then (ls, true)
          else (let r = ls_delete _A k ls in ((l, w) :: fst r, snd r)));;

let rec ht_delete (_A1, _A2, _A3) _B
  k ht =
    (fun f_ () -> f_ ((len (heap_list (heap_prod _A3 _B)) (the_array ht)) ())
      ())
      (fun m ->
        (let i = bounded_hashcode_nat _A2 m k in
          (fun f_ () -> f_
            ((ntha (heap_list (heap_prod _A3 _B)) (the_array ht) i) ()) ())
            (fun l ->
              (let la = ls_delete _A1 k l in
                (fun f_ () -> f_
                  ((upd (heap_list (heap_prod _A3 _B)) i (fst la)
                     (the_array ht))
                  ()) ())
                  (fun _ ->
                    (let n =
                       (if snd la then minus_nat (the_size ht) one_nata
                         else the_size ht)
                       in
                      (fun () -> (HashTable (the_array ht, n)))))))));;

let rec ls_lookup _A
  x xa1 = match x, xa1 with x, [] -> None
    | x, (k, v) :: l -> (if eq _A x k then Some v else ls_lookup _A x l);;

let rec ht_lookup (_A1, _A2, _A3) _B
  x ht =
    (fun f_ () -> f_ ((len (heap_list (heap_prod _A3 _B)) (the_array ht)) ())
      ())
      (fun m ->
        (let i = bounded_hashcode_nat _A2 m x in
          (fun f_ () -> f_
            ((ntha (heap_list (heap_prod _A3 _B)) (the_array ht) i) ()) ())
            (fun l -> (fun () -> (ls_lookup _A1 x l)))));;

let rec ht_rehash (_A1, _A2, _A3) _B
  ht = (fun f_ () -> f_ ((len (heap_list (heap_prod _A3 _B)) (the_array ht)) ())
         ())
         (fun n ->
           (fun f_ () -> f_
             ((ht_new_sz (_A2, _A3) _B
                (times_nat (nat_of_integer (Z.Int.of_int 2)) n))
             ()) ())
             (ht_copy (_A1, _A2, _A3) _B n ht));;

let load_factor : nat = nat_of_integer (Z.Int.of_int 75);;

let rec ht_update (_A1, _A2, _A3) _B
  k v ht =
    (fun f_ () -> f_ ((len (heap_list (heap_prod _A3 _B)) (the_array ht)) ())
      ())
      (fun m ->
        (fun f_ () -> f_
          ((if less_eq_nat (times_nat m load_factor)
                 (times_nat (the_size ht) (nat_of_integer (Z.Int.of_int 100)))
             then ht_rehash (_A1, _A2, _A3) _B ht else (fun () -> ht))
          ()) ())
          (ht_upd (_A1, _A2, _A3) _B k v));;

let rec op_list_tl x = tl x;;

let rec map_act f x1 = match f, x1 with f, In x1 -> In (f x1)
                  | f, Out x2 -> Out (f x2)
                  | f, Sil x3 -> Sil (f x3);;

let bot_set : 'a set = Set [];;

let rec set_act _A = function In x1 -> insert _A x1 bot_set
                     | Out x2 -> insert _A x2 bot_set
                     | Sil x3 -> insert _A x3 bot_set;;

let rec array_copy _A
  a = (fun f_ () -> f_ ((len _A a) ()) ())
        (fun l ->
          (if equal_nata l zero_nata then (fun () -> Array.of_list [])
            else (fun f_ () -> f_ ((ntha _A a zero_nata) ()) ())
                   (fun s ->
                     (fun f_ () -> f_ ((newa _A l s) ()) ())
                       (fun aa ->
                         (fun f_ () -> f_ ((blit _A a zero_nata aa zero_nata l)
                           ()) ())
                           (fun _ -> (fun () -> aa))))));;

let array_growa _ _ _ = failwith "Diff_Array.array_grow\039";;

let rec array_grow a = comp (array_growa a) integer_of_nat;;

let rec array_grow a = comp (FArray.IsabelleMapping.array_grow a) integer_of_nat;;

let rec binda m f = (match m with Result a -> f a | Error a -> Error a);;

let rec hm_it_adjust (_A1, _A2) _B
  v ht =
    (if equal_nata v zero_nata then (fun () -> zero_nata)
      else (fun f_ () -> f_
             ((ntha (heap_list (heap_prod _A2 _B)) (the_array ht)
                (suc (minus_nat v one_nata)))
             ()) ())
             (fun a ->
               (match a
                 with [] ->
                   hm_it_adjust (_A1, _A2) _B
                     (minus_nat (suc (minus_nat v one_nata)) one_nata) ht
                 | _ :: _ -> (fun () -> (suc (minus_nat v one_nata))))));;

let rec op_list_rev x = rev x;;

let rec all_interval_nat
  p i j = less_eq_nat j i || p i && all_interval_nat p (suc i) j;;

let rec map_index n f x2 = match n, f, x2 with n, f, [] -> []
                    | n, f, x :: xs -> f n x :: map_index (suc n) f xs;;

let rec pred_act _A = (fun p x -> ball (set_act _A x) p);;

let rec eoi _A
  = bindb get_tokens
      (fun tks ->
        (if null tks then return ()
          else err_expecting_aux _A
                 (fun _ ->
                   shows_string
                     [Chara (true, false, true, false, false, true, true,
                              false);
                       Chara (false, true, true, true, false, true, true,
                               false);
                       Chara (false, false, true, false, false, true, true,
                               false);
                       Chara (false, false, false, false, false, true, false,
                               false);
                       Chara (true, true, true, true, false, true, true, false);
                       Chara (false, true, true, false, false, true, true,
                               false);
                       Chara (false, false, false, false, false, true, false,
                               false);
                       Chara (true, false, false, true, false, true, true,
                               false);
                       Chara (false, true, true, true, false, true, true,
                               false);
                       Chara (false, false, false, false, true, true, true,
                               false);
                       Chara (true, false, true, false, true, true, true,
                               false);
                       Chara (false, false, true, false, true, true, true,
                               false)])));;

let rec neg_dbm_entry _A = function Le a -> Lt (uminus _A a)
                           | Lt a -> Le (uminus _A a)
                           | INF -> INF;;

let rec swap p = (snd p, fst p);;

let rec imp_for
  i u c f s =
    (if less_eq_nat u i then (fun () -> s)
      else (fun f_ () -> f_ ((c s) ()) ())
             (fun ctn ->
               (if ctn
                 then (fun f_ () -> f_ ((f i s) ()) ())
                        (imp_for (plus_nata i one_nata) u c f)
                 else (fun () -> s))));;

let rec whilea b c s = (if b s then whilea b c (c s) else s);;

let rec min _A a b = (if less_eq _A a b then a else b);;

let rec down_impl (_A1, _A2, _A3)
  n = imp_fora one_nata (suc n)
        (fun xb sigma ->
          (fun f_ () -> f_
            ((imp_fora one_nata (suc n)
               (fun xe sigmaa ->
                 (fun f_ () -> f_
                   ((mtx_get (heap_DBMEntry _A3) (suc n) sigma (xe, xb)) ()) ())
                   (fun x_f ->
                     (fun () ->
                       (min (ord_DBMEntry
                              (_A2, _A1.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.linordered_ab_semigroup_add_linordered_ab_monoid_add.linorder_linordered_ab_semigroup_add))
                         x_f sigmaa))))
               (Le (zero _A1.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.ordered_comm_monoid_add_linordered_ab_monoid_add.comm_monoid_add_ordered_comm_monoid_add.monoid_add_comm_monoid_add.zero_monoid_add)))
            ()) ())
            (mtx_set (heap_DBMEntry _A3) (suc n) sigma (zero_nata, xb)));;

let rec free_impl (_A1, _A2)
  n = (fun ai bi ->
        (fun f_ () -> f_
          ((imp_fora zero_nata (suc n)
             (fun xa sigma ->
               (if not (equal_nata xa bi)
                 then (fun f_ () -> f_
                        ((mtx_get (heap_DBMEntry _A2) (suc n) sigma
                           (xa, zero_nata))
                        ()) ())
                        (mtx_set (heap_DBMEntry _A2) (suc n) sigma (xa, bi))
                 else (fun () -> sigma)))
             ai)
          ()) ())
          (imp_fora zero_nata (suc n)
            (fun xb sigma ->
              (if not (equal_nata xb bi)
                then mtx_set (heap_DBMEntry _A2) (suc n) sigma (bi, xb) INF
                else (fun () -> sigma)))));;


let array_lengtha _ = failwith "Diff_Array.array_length\039";;

let rec array_length x = comp nat_of_integer array_lengtha x;;

let array_length x = comp nat_of_integer FArray.IsabelleMapping.array_length x;;

let array_shrinka _ _ = failwith "Diff_Array.array_shrink\039";;

let rec array_shrink a = comp (array_shrinka a) integer_of_nat;;

let array_shrink a = comp (FArray.IsabelleMapping.array_shrink a) integer_of_nat;;

let rec asserta b m = (if b then Result () else Error [m]);;

let op_list_empty : 'a list = [];;

let rec as_get s i = (let a = s in
                      let (aa, _) = a in
                       array_get aa i);;

let rec as_shrink
  s = (let a = s in
       let (aa, n) = a in
       let ab =
         (if less_eq_nat (times_nat (nat_of_integer (Z.Int.of_int 128)) n)
               (array_length aa) &&
               less_nat (nat_of_integer (Z.Int.of_int 4)) n
           then array_shrink aa n else aa)
         in
        (ab, n));;

let rec as_pop s = (let a = s in
                    let (aa, n) = a in
                     as_shrink (aa, minus_nat n one_nata));;

let rec as_set s i x = (let a = s in
                        let (aa, b) = a in
                         (array_set aa i x, b));;

let rec as_top s = (let a = s in
                    let (aa, n) = a in
                     array_get aa (minus_nat n one_nata));;

let rec hm_it_next_key (_A1, _A2) _B
  ht = (fun f_ () -> f_ ((len (heap_list (heap_prod _A2 _B)) (the_array ht)) ())
         ())
         (fun n ->
           (if equal_nata n zero_nata then failwith "Map is empty!"
             else (fun f_ () -> f_
                    ((hm_it_adjust (_A1, _A2) _B (minus_nat n one_nata) ht) ())
                    ())
                    (fun i ->
                      (fun f_ () -> f_
                        ((ntha (heap_list (heap_prod _A2 _B)) (the_array ht) i)
                        ()) ())
                        (fun a ->
                          (match a with [] -> failwith "Map is empty!"
                            | x :: _ -> (fun () -> (fst x)))))));;

let rec heap_WHILET
  b f s =
    (fun f_ () -> f_ ((b s) ()) ())
      (fun bv ->
        (if bv then (fun f_ () -> f_ ((f s) ()) ()) (heap_WHILET b f)
          else (fun () -> s)));;

let rec imp_nfoldli
  x0 c f s = match x0, c, f, s with
    x :: ls, c, f, s ->
      (fun f_ () -> f_ ((c s) ()) ())
        (fun b ->
          (if b then (fun f_ () -> f_ ((f x s) ()) ()) (imp_nfoldli ls c f)
            else (fun () -> s)))
    | [], c, f, s -> (fun () -> s);;

let rec lso_bex_impl
  pi li =
    imp_nfoldli li (fun sigma -> (fun () -> (not sigma))) (fun xa _ -> pi xa)
      false;;

let rec op_list_is_empty x = null x;;

let rec op_list_prepend x = (fun a -> x :: a);;

let rec hms_extract
  lookup delete k m =
    (fun f_ () -> f_ ((lookup k m) ()) ())
      (fun a ->
        (match a with None -> (fun () -> (None, m))
          | Some v ->
            (fun f_ () -> f_ ((delete k m) ()) ())
              (fun ma -> (fun () -> (Some v, ma)))));;

let rec pw_impl _A (_B1, _B2, _B3)
  keyi copyi tracei lei a_0i fi succsi emptyi =
    (fun f_ () -> f_ (a_0i ()) ())
      (fun x ->
        (fun f_ () -> f_ ((emptyi x) ()) ())
          (fun xa ->
            (fun f_ () -> f_ (a_0i ()) ())
              (fun xaa ->
                (fun f_ () -> f_ ((fi xaa) ()) ())
                  (fun xab ->
                    (fun f_ () -> f_
                      ((if not xa && xab
                         then (fun f_ () -> f_
                                ((ht_new (_B2, _B3) (heap_list _A)) ()) ())
                                (fun x_b -> (fun () -> (true, x_b)))
                         else (fun f_ () -> f_ (a_0i ()) ())
                                (fun xb ->
                                  (fun f_ () -> f_ ((emptyi xb) ()) ())
                                    (fun x_a ->
                                      (if x_a
then (fun f_ () -> f_ ((ht_new (_B2, _B3) (heap_list _A)) ()) ())
       (fun x_c -> (fun () -> (false, x_c)))
else (fun f_ () -> f_ (a_0i ()) ())
       (fun xc ->
         (fun f_ () -> f_ ((keyi xc) ()) ())
           (fun xd ->
             (fun f_ () -> f_ (a_0i ()) ())
               (fun xac ->
                 (fun f_ () -> f_ ((ht_new (_B2, _B3) (heap_list _A)) ()) ())
                   (fun xba ->
                     (fun f_ () -> f_
                       ((ht_update (_B1, _B2, _B3) (heap_list _A) xd [xac] xba)
                       ()) ())
                       (fun xe ->
                         (fun f_ () -> f_ (a_0i ()) ())
                           (fun xad ->
                             (fun f_ () -> f_
                               ((heap_WHILET
                                  (fun (_, (a1b, a2b)) ->
                                    (fun () ->
                                      (not a2b && not (op_list_is_empty a1b))))
                                  (fun (a1a, (a1b, a2b)) ->
                                    (let (a1c, a2c) =
                                       (match a1b
 with [] -> cODE_ABORT (fun _ -> (hd a1b, tl a1b)) | a :: b -> (a, b))
                                       in
                                      (fun f_ () -> f_ ((emptyi a1c) ()) ())
(fun x_e ->
  (if x_e then (fun () -> (a1a, (a2c, a2b)))
    else (fun f_ () -> f_ (tRACE_impl ()) ())
           (fun _ ->
             (fun f_ () -> f_
               ((tracei
                   [Chara (true, false, true, false, false, false, true, false);
                     Chara (false, false, false, true, true, true, true, false);
                     Chara (false, false, false, false, true, true, true,
                             false);
                     Chara (false, false, true, true, false, true, true, false);
                     Chara (true, true, true, true, false, true, true, false);
                     Chara (false, true, false, false, true, true, true, false);
                     Chara (true, false, true, false, false, true, true, false);
                     Chara (false, false, true, false, false, true, true,
                             false)]
                  a1c)
               ()) ())
               (fun _ ->
                 (fun f_ () -> f_ ((succsi a1c) ()) ())
                   (fun x_h ->
                     imp_nfoldli x_h (fun (_, (_, b)) -> (fun () -> (not b)))
                       (fun xl (a1d, (a1e, _)) ->
                         (fun f_ () -> f_ ((emptyi xl) ()) ())
                           (fun x_k ->
                             (if x_k
                               then (fun f_ () -> f_
                                      ((tracei
  [Chara (true, false, true, false, false, false, true, false);
    Chara (true, false, true, true, false, true, true, false);
    Chara (false, false, false, false, true, true, true, false);
    Chara (false, false, true, false, true, true, true, false);
    Chara (true, false, false, true, true, true, true, false)]
 xl)
                                      ()) ())
                                      (fun _ -> (fun () -> (a1d, (a1e, false))))
                               else (fun f_ () -> f_ ((fi xl) ()) ())
                                      (fun x_l ->
(if x_l
  then (fun f_ () -> f_
         ((tracei
             [Chara (false, true, true, false, false, false, true, false);
               Chara (true, false, false, true, false, true, true, false);
               Chara (false, true, true, true, false, true, true, false);
               Chara (true, false, false, false, false, true, true, false);
               Chara (false, false, true, true, false, true, true, false)]
            xl)
         ()) ())
         (fun _ -> (fun () -> (a1d, (a1e, true))))
  else (fun f_ () -> f_ ((keyi xl) ()) ())
         (fun x_m ->
           (fun f_ () -> f_
             ((hms_extract (ht_lookup (_B1, _B2, _B3) (heap_list _A))
                (ht_delete (_B1, _B2, _B3) (heap_list _A)) x_m a1d)
             ()) ())
             (fun a ->
               (match a
                 with (None, a2f) ->
                   (fun f_ () -> f_
                     ((tracei
                         [Chara (true, false, false, false, false, false, true,
                                  false);
                           Chara (false, false, true, false, false, true, true,
                                   false);
                           Chara (false, false, true, false, false, true, true,
                                   false)]
                        xl)
                     ()) ())
                     (fun _ ->
                       (fun f_ () -> f_ ((copyi xl) ()) ())
                         (fun xf ->
                           (fun f_ () -> f_
                             ((ht_update (_B1, _B2, _B3) (heap_list _A) x_m [xf]
                                a2f)
                             ()) ())
                             (fun x_r ->
                               (fun () ->
                                 (x_r, (op_list_prepend xl a1e, false))))))
                 | (Some x_q, a2f) ->
                   (fun f_ () -> f_ ((lso_bex_impl (lei xl) x_q) ()) ())
                     (fun x_r ->
                       (if x_r
                         then (fun f_ () -> f_
                                ((tracei
                                    [Chara (true, true, false, false, true,
     false, true, false);
                                      Chara
(true, false, true, false, true, true, true, false);
                                      Chara
(false, true, false, false, false, true, true, false);
                                      Chara
(true, true, false, false, true, true, true, false);
                                      Chara
(true, false, true, false, true, true, true, false);
                                      Chara
(true, false, true, true, false, true, true, false);
                                      Chara
(true, false, true, false, false, true, true, false);
                                      Chara
(false, false, true, false, false, true, true, false)]
                                   xl)
                                ()) ())
                                (fun _ ->
                                  (fun f_ () -> f_
                                    ((ht_update (_B1, _B2, _B3) (heap_list _A)
                                       x_m x_q a2f)
                                    ()) ())
                                    (fun x_t ->
                                      (fun () -> (x_t, (a1e, false)))))
                         else (fun f_ () -> f_
                                ((tracei
                                    [Chara (true, false, false, false, false,
     false, true, false);
                                      Chara
(false, false, true, false, false, true, true, false);
                                      Chara
(false, false, true, false, false, true, true, false)]
                                   xl)
                                ()) ())
                                (fun _ ->
                                  (fun f_ () -> f_ ((copyi xl) ()) ())
                                    (fun xf ->
                                      (fun f_ () -> f_
((ht_update (_B1, _B2, _B3) (heap_list _A) x_m (xf :: x_q) a2f) ()) ())
(fun x_t -> (fun () -> (x_t, (op_list_prepend xl a1e, false))))))))))))))))
                       (a1a, (a2c, false)))))))))
                                  (xe, (op_list_prepend xad [], false)))
                               ()) ())
                               (fun (a1a, (_, a2b)) ->
                                 (fun () -> (a2b, a1a)))))))))))))
                      ()) ())
                      (fun (a1, _) -> (fun () -> a1))))));;

let rec mtx_tabulate (_A1, _A2, _A3) (_B1, _B2)
  n m c =
    (fun f_ () -> f_ ((newa _B2 (times_nat n m) (zero _B1)) ()) ())
      (fun ma ->
        (fun f_ () -> f_
          ((imp_fora zero_nata (times_nat n m)
             (fun k (i, (j, maa)) ->
               (fun f_ () -> f_ ((upd _B2 k (c (i, j)) maa) ()) ())
                 (fun _ ->
                   (let ja = plus_nata j one_nata in
                     (if less_nat ja m then (fun () -> (i, (ja, maa)))
                       else (fun () ->
                              (plus _A2 i (one _A1), (zero_nata, maa)))))))
             (zero _A3, (zero_nata, ma)))
          ()) ())
          (fun (_, a) -> (let (_, aa) = a in (fun () -> aa))));;

let rec v_dbm_impl (_A1, _A2)
  n = mtx_tabulate (one_nat, plus_nat, zero_nat)
        ((zero_DBMEntry
           _A1.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.ordered_comm_monoid_add_linordered_ab_monoid_add.comm_monoid_add_ordered_comm_monoid_add.monoid_add_comm_monoid_add.zero_monoid_add),
          (heap_DBMEntry _A2))
        (suc n) (suc n)
        (v_dbm (zero_nat, equal_nat, ord_nat)
          _A1.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.ordered_comm_monoid_add_linordered_ab_monoid_add.comm_monoid_add_ordered_comm_monoid_add.monoid_add_comm_monoid_add.zero_monoid_add
          n);;

(* let rec array_of_list x = Arraya x;; *)
(* let array_of_list = Array.of_list;; *)
let array_of_list = FArray.IsabelleMapping.array_of_list;;

let rec combine2_gen
  comb x1 x2 = match comb, x1, x2 with
    comb, Error e1, Error e2 -> Error (e1 @ e2)
    | comb, Error e, Result uu -> Error e
    | comb, Result uv, Error e -> Error e
    | comb, Result a, Result b -> comb a b;;

let rec combine
  = function [] -> Result []
    | x :: xs ->
        combine2_gen (fun xa xsa -> Result (xa :: xsa)) x (combine xs);;

let rec err_msg m x1 = match m, x1 with m, Error es -> Error (m :: es)
                  | m, Result v -> Result v;;

let rec stat_stop x = (fun _ -> ()) x;;

let rec as_push
  s x = (let a = s in
         let (aa, n) = a in
         let ab =
           (if equal_nata n (array_length aa)
             then array_grow aa
                    (max ord_nat (nat_of_integer (Z.Int.of_int 4))
                      (times_nat (nat_of_integer (Z.Int.of_int 2)) n))
                    x
             else aa)
           in
         let ac = array_set ab n x in
          (ac, plus_nata n one_nata));;

let rec as_take
  m s = (let a = s in
         let (aa, n) = a in
          (if less_nat m n then as_shrink (aa, m) else (aa, n)));;

let rec rev_append x0 ac = match x0, ac with [], ac -> ac
                     | x :: xs, ac -> rev_append xs (x :: ac);;

let one_int : int = Int_of_integer (Z.Int.of_int 1);;

let rec ran_of_map_impl (_A1, _A2, _A3) _B
  = (fun xi ->
      (fun f_ () -> f_
        ((heap_WHILET
           (fun (_, a2) ->
             (fun f_ () -> f_ ((hm_isEmpty a2) ()) ())
               (fun x_a -> (fun () -> (not x_a))))
           (fun (a1, a2) ->
             (fun f_ () -> f_ ((hm_it_next_key (_A2, _A3) _B a2) ()) ())
               (fun x_a ->
                 (fun f_ () -> f_
                   ((hms_extract (ht_lookup (_A1, _A2, _A3) _B)
                      (ht_delete (_A1, _A2, _A3) _B) x_a a2)
                   ()) ())
                   (fun (a1a, b) -> (fun () -> (the a1a :: a1, b)))))
           ([], xi))
        ()) ())
        (fun (a1, _) -> (fun () -> a1)));;

let rec map_option f x1 = match f, x1 with f, None -> None
                     | f, Some x2 -> Some (f x2);;

let rec sup_set _A
  x0 a = match x0, a with
    Coset xs, a -> Coset (filter (fun x -> not (member _A x a)) xs)
    | Set xs, a -> fold (insert _A) xs a;;

let rec combine2 x = combine2_gen (fun a b -> Result (a, b)) x;;

let rec stat_start x = (fun _ -> ()) x;;

let rec idx_iteratei_aux
  get sz i l c f sigma =
    (if equal_nata i zero_nata || not (c sigma) then sigma
      else idx_iteratei_aux get sz (minus_nat i one_nata) l c f
             (f (get l (minus_nat sz i)) sigma));;

let rec idx_iteratei
  get sz l c f sigma = idx_iteratei_aux get (sz l) (sz l) l c f sigma;;

let rec as_empty _B uu = (array_of_list [], zero _B);;

let rec leadsto_impl_0 _A (_B1, _B2, _B3)
  copyia succsia leia keyia x =
    (let (a1, (a1a, a2a)) = x in
      (fun f_ () -> f_ ((keyia a2a) ()) ())
        (fun xa ->
          (fun f_ () -> f_
            ((hms_extract (ht_lookup (_B1, _B2, _B3) (heap_list _A))
               (ht_delete (_B1, _B2, _B3) (heap_list _A)) xa a1a)
            ()) ())
            (fun xaa ->
              (fun f_ () -> f_
                ((match xaa with (None, a2b) -> (fun () -> (a2b, false))
                   | (Some x_c, a2b) ->
                     (fun f_ () -> f_
                       ((imp_nfoldli x_c (fun sigma -> (fun () -> (not sigma)))
                          (fun xe sigma ->
                            (fun f_ () -> f_ ((leia xe a2a) ()) ())
                              (fun x_f -> (fun () -> (x_f || sigma))))
                          false)
                       ()) ())
                       (fun x_d ->
                         (fun f_ () -> f_
                           ((ht_update (_B1, _B2, _B3) (heap_list _A) xa x_c
                              a2b)
                           ()) ())
                           (fun x_e -> (fun () -> (x_e, x_d)))))
                ()) ())
                (fun a ->
                  (match a with (a1b, true) -> (fun () -> (a1, (a1b, true)))
                    | (a1b, false) ->
                      (fun f_ () -> f_ ((keyia a2a) ()) ())
                        (fun xb ->
                          (fun f_ () -> f_
                            ((hms_extract
                               (ht_lookup (_B1, _B2, _B3) (heap_list _A))
                               (ht_delete (_B1, _B2, _B3) (heap_list _A)) xb a1)
                            ()) ())
                            (fun xab ->
                              (fun f_ () -> f_
                                ((match xab
                                   with (None, a2c) -> (fun () -> (a2c, false))
                                   | (Some x_e, a2c) ->
                                     (fun f_ () -> f_
                                       ((lso_bex_impl (leia a2a) x_e) ()) ())
                                       (fun x_f ->
 (fun f_ () -> f_ ((ht_update (_B1, _B2, _B3) (heap_list _A) xb x_e a2c) ()) ())
   (fun x_g -> (fun () -> (x_g, x_f)))))
                                ()) ())
                                (fun aa ->
                                  (match aa
                                    with (a1c, true) ->
                                      (fun () -> (a1c, (a1b, false)))
                                    | (a1c, false) ->
                                      (fun f_ () -> f_ ((copyia a2a) ()) ())
(fun xc ->
  (fun f_ () -> f_ ((keyia xc) ()) ())
    (fun xd ->
      (fun f_ () -> f_
        ((hms_extract (ht_lookup (_B1, _B2, _B3) (heap_list _A))
           (ht_delete (_B1, _B2, _B3) (heap_list _A)) xd a1b)
        ()) ())
        (fun xac ->
          (fun f_ () -> f_
            ((match xac
               with (None, a2d) ->
                 (fun f_ () -> f_ ((copyia a2a) ()) ())
                   (fun xad ->
                     (fun f_ () -> f_
                       ((ht_update (_B1, _B2, _B3) (heap_list _A) xd
                          (op_list_prepend xad []) a2d)
                       ()) ())
                       (fun x_h -> (fun () -> ((), x_h))))
               | (Some x_g, a2d) ->
                 (fun f_ () -> f_ ((copyia a2a) ()) ())
                   (fun xad ->
                     (fun f_ () -> f_
                       ((ht_update (_B1, _B2, _B3) (heap_list _A) xd
                          (op_list_prepend xad x_g) a2d)
                       ()) ())
                       (fun x_i -> (fun () -> ((), x_i)))))
            ()) ())
            (fun (_, a2d) ->
              (fun f_ () -> f_ ((succsia a2a) ()) ())
                (fun xe ->
                  (fun f_ () -> f_
                    ((imp_nfoldli xe (fun (_, (_, b)) -> (fun () -> (not b)))
                       (fun xi (a1e, (a1f, _)) ->
                         leadsto_impl_0 _A (_B1, _B2, _B3) copyia succsia leia
                           keyia (a1e, (a1f, xi)))
                       (a1c, (a2d, false)))
                    ()) ())
                    (fun (a1e, (a1f, a2f)) ->
                      (fun f_ () -> f_ ((copyia a2a) ()) ())
                        (fun xf ->
                          (fun f_ () -> f_ ((keyia xf) ()) ())
                            (fun xg ->
                              (fun f_ () -> f_
                                ((hms_extract
                                   (ht_lookup (_B1, _B2, _B3) (heap_list _A))
                                   (ht_delete (_B1, _B2, _B3) (heap_list _A)) xg
                                   a1f)
                                ()) ())
                                (fun xad ->
                                  (fun f_ () -> f_
                                    ((match xad
                                       with (None, a2g) ->
 (fun f_ () -> f_ ((ht_update (_B1, _B2, _B3) (heap_list _A) xg [] a2g) ()) ())
   (fun x_k -> (fun () -> ((), x_k)))
                                       | (Some x_j, a2g) ->
 (fun f_ () -> f_
   ((ht_update (_B1, _B2, _B3) (heap_list _A) xg
      (if op_list_is_empty x_j then [] else op_list_tl x_j) a2g)
   ()) ())
   (fun x_l -> (fun () -> ((), x_l))))
                                    ()) ())
                                    (fun (_, a2g) ->
                                      (fun f_ () -> f_ ((copyia a2a) ()) ())
(fun xh ->
  (fun f_ () -> f_ ((keyia xh) ()) ())
    (fun xi ->
      (fun f_ () -> f_
        ((hms_extract (ht_lookup (_B1, _B2, _B3) (heap_list _A))
           (ht_delete (_B1, _B2, _B3) (heap_list _A)) xi a1e)
        ()) ())
        (fun xae ->
          (fun f_ () -> f_
            ((match xae
               with (None, a2h) ->
                 (fun f_ () -> f_ ((copyia a2a) ()) ())
                   (fun xaf ->
                     (fun f_ () -> f_
                       ((ht_update (_B1, _B2, _B3) (heap_list _A) xi [xaf] a2h)
                       ()) ())
                       (fun x_m -> (fun () -> ((), x_m))))
               | (Some x_l, a2h) ->
                 (fun f_ () -> f_ ((copyia a2a) ()) ())
                   (fun xaf ->
                     (fun f_ () -> f_
                       ((ht_update (_B1, _B2, _B3) (heap_list _A) xi
                          (xaf :: x_l) a2h)
                       ()) ())
                       (fun x_n -> (fun () -> ((), x_n)))))
            ()) ())
            (fun (_, a2h) ->
              (fun f_ () -> f_ (tRACE_impl ()) ())
                (fun _ ->
                  (fun () -> (a2h, (a2g, a2f)))))))))))))))))))))))))));;

let rec leadsto_impl _A (_B1, _B2, _B3)
  copyia succsia a_0ia leia keyia succs1i emptyi pi qi tracei =
    (fun f_ () -> f_ (a_0ia ()) ())
      (fun x ->
        (fun f_ () -> f_ ((emptyi x) ()) ())
          (fun xa ->
            (fun f_ () -> f_ (a_0ia ()) ())
              (fun _ ->
                (fun f_ () -> f_
                  ((if not xa && false
                     then (fun f_ () -> f_ ((ht_new (_B2, _B3) (heap_list _A))
                            ()) ())
                            (fun x_b -> (fun () -> (true, x_b)))
                     else (fun f_ () -> f_ (a_0ia ()) ())
                            (fun xb ->
                              (fun f_ () -> f_ ((emptyi xb) ()) ())
                                (fun x_a ->
                                  (if x_a
                                    then (fun f_ () -> f_
   ((ht_new (_B2, _B3) (heap_list _A)) ()) ())
   (fun x_c -> (fun () -> (false, x_c)))
                                    else (fun f_ () -> f_ (a_0ia ()) ())
   (fun xc ->
     (fun f_ () -> f_ ((keyia xc) ()) ())
       (fun xd ->
         (fun f_ () -> f_ (a_0ia ()) ())
           (fun xaa ->
             (fun f_ () -> f_ ((ht_new (_B2, _B3) (heap_list _A)) ()) ())
               (fun xba ->
                 (fun f_ () -> f_
                   ((ht_update (_B1, _B2, _B3) (heap_list _A) xd [xaa] xba) ())
                   ())
                   (fun xe ->
                     (fun f_ () -> f_ (a_0ia ()) ())
                       (fun xab ->
                         (fun f_ () -> f_
                           ((heap_WHILET
                              (fun (_, (a1b, a2b)) ->
                                (fun () ->
                                  (not a2b && not (op_list_is_empty a1b))))
                              (fun (a1a, (a1b, a2b)) ->
                                (let (a1c, a2c) =
                                   (match a1b
                                     with [] ->
                                       cODE_ABORT (fun _ -> (hd a1b, tl a1b))
                                     | a :: b -> (a, b))
                                   in
                                  (fun f_ () -> f_ ((emptyi a1c) ()) ())
                                    (fun x_e ->
                                      (if x_e then (fun () -> (a1a, (a2c, a2b)))
else (fun f_ () -> f_ (tRACE_impl ()) ())
       (fun _ ->
         (fun f_ () -> f_
           ((tracei
               [Chara (true, false, true, false, false, false, true, false);
                 Chara (false, false, false, true, true, true, true, false);
                 Chara (false, false, false, false, true, true, true, false);
                 Chara (false, false, true, true, false, true, true, false);
                 Chara (true, true, true, true, false, true, true, false);
                 Chara (false, true, false, false, true, true, true, false);
                 Chara (true, false, true, false, false, true, true, false);
                 Chara (false, false, true, false, false, true, true, false)]
              a1c)
           ()) ())
           (fun _ ->
             (fun f_ () -> f_ ((succs1i a1c) ()) ())
               (fun x_h ->
                 imp_nfoldli x_h (fun (_, (_, b)) -> (fun () -> (not b)))
                   (fun xl (a1d, (a1e, _)) ->
                     (fun f_ () -> f_ ((emptyi xl) ()) ())
                       (fun x_k ->
                         (if x_k then (fun () -> (a1d, (a1e, false)))
                           else (fun f_ () -> f_ ((keyia xl) ()) ())
                                  (fun x_m ->
                                    (fun f_ () -> f_
                                      ((hms_extract
 (ht_lookup (_B1, _B2, _B3) (heap_list _A))
 (ht_delete (_B1, _B2, _B3) (heap_list _A)) x_m a1d)
                                      ()) ())
                                      (fun a ->
(match a
  with (None, a2f) ->
    (fun f_ () -> f_ ((copyia xl) ()) ())
      (fun xf ->
        (fun f_ () -> f_
          ((ht_update (_B1, _B2, _B3) (heap_list _A) x_m [xf] a2f) ()) ())
          (fun x_o -> (fun () -> (x_o, (op_list_prepend xl a1e, false)))))
  | (Some x_o, a2f) ->
    (fun f_ () -> f_ ((lso_bex_impl (leia xl) x_o) ()) ())
      (fun x_p ->
        (if x_p
          then (fun f_ () -> f_
                 ((ht_update (_B1, _B2, _B3) (heap_list _A) x_m x_o a2f) ()) ())
                 (fun x_q -> (fun () -> (x_q, (a1e, false))))
          else (fun f_ () -> f_ ((copyia xl) ()) ())
                 (fun xf ->
                   (fun f_ () -> f_
                     ((ht_update (_B1, _B2, _B3) (heap_list _A) x_m (xf :: x_o)
                        a2f)
                     ()) ())
                     (fun x_q ->
                       (fun () ->
                         (x_q, (op_list_prepend xl a1e, false)))))))))))))
                   (a1a, (a2c, false)))))))))
                              (xe, (op_list_prepend xab [], false)))
                           ()) ())
                           (fun (a1a, (_, a2b)) ->
                             (fun () -> (a2b, a1a)))))))))))))
                  ()) ())
                  (fun (_, a2) ->
                    (fun f_ () -> f_
                      ((ran_of_map_impl (_B1, _B2, _B3) (heap_list _A) a2) ())
                      ())
                      (fun x_a ->
                        (fun f_ () -> f_ ((ht_new (_B2, _B3) (heap_list _A)) ())
                          ())
                          (fun xb ->
                            (fun f_ () -> f_
                              ((imp_nfoldli x_a
                                 (fun (a1a, _) -> (fun () -> (not a1a)))
                                 (fun xd (_, a2a) ->
                                   imp_nfoldli xd
                                     (fun (a1b, _) -> (fun () -> (not a1b)))
                                     (fun xg (_, a2b) ->
                                       (fun f_ () -> f_ ((pi xg) ()) ())
 (fun xc ->
   (fun f_ () -> f_ ((qi xg) ()) ())
     (fun xaa ->
       (if xc && xaa
         then (fun f_ () -> f_ ((ht_new (_B2, _B3) (heap_list _A)) ()) ())
                (fun xe ->
                  (fun f_ () -> f_
                    ((leadsto_impl_0 _A (_B1, _B2, _B3) copyia succsia leia
                       keyia (a2b, (xe, xg)))
                    ()) ())
                    (fun (a1, (_, a2aa)) -> (fun () -> (a2aa, a1))))
         else (fun () -> (false, a2b))))))
                                     (false, a2a))
                                 (false, xb))
                              ()) ())
                              (fun (a1a, _) -> (fun () -> a1a))))))));;

let rec as_length x = snd x;;

let rec last_seg_tr _A
  s = (let (a, (aa, (_, _))) = s in
       let (_, bc) =
         whilea
           (fun (xe, _) ->
             less_nat xe
               (if equal_nata
                     (plus_nata (minus_nat (as_length aa) one_nata) one_nata)
                     (as_length aa)
                 then as_length a
                 else as_get aa
                        (plus_nata (minus_nat (as_length aa) one_nata)
                          one_nata)))
           (fun (ac, bc) -> (let xa = as_get a ac in (suc ac, xa :: bc)))
           (as_get aa (minus_nat (as_length aa) one_nata), [])
         in
        bc);;

let rec list_map_update_aux
  eq k v x3 accu = match eq, k, v, x3, accu with
    eq, k, v, [], accu -> (k, v) :: accu
    | eq, k, v, x :: xs, accu ->
        (if eq (fst x) k then (k, v) :: xs @ accu
          else list_map_update_aux eq k v xs (x :: accu));;

let rec list_map_update eq k v m = list_map_update_aux eq k v m [];;

let rec list_map_lookup
  eq uu x2 = match eq, uu, x2 with eq, uu, [] -> None
    | eq, k, y :: ys ->
        (if eq (fst y) k then Some (snd y) else list_map_lookup eq k ys);;

let rec ahm_update_aux
  eq bhc (HashMap (a, n)) k v =
    (let h = bhc (array_length a) k in
     let m = array_get a h in
     let insert = is_none (list_map_lookup eq k m) in
      HashMap
        (array_set a h (list_map_update eq k v m),
          (if insert then plus_nata n one_nata else n)));;

let rec ahm_iteratei_aux
  a c f sigma =
    idx_iteratei array_get array_length a c (fun x -> foldli x c f) sigma;;

let rec ahm_rehash_auxa
  bhc n kv a = (let h = bhc n (fst kv) in array_set a h (kv :: array_get a h));;

let rec ahm_rehash_aux
  bhc a sz =
    ahm_iteratei_aux a (fun _ -> true) (ahm_rehash_auxa bhc sz)
      (new_array [] sz);;

let rec ahm_rehash
  bhc (HashMap (a, n)) sz = HashMap (ahm_rehash_aux bhc a sz, n);;

let load_factora : nat = nat_of_integer (Z.Int.of_int 75);;

let rec ahm_filled
  (HashMap (a, n)) =
    less_eq_nat (times_nat (array_length a) load_factora)
      (times_nat n (nat_of_integer (Z.Int.of_int 100)));;

let rec hm_grow
  (HashMap (a, n)) =
    plus_nata (times_nat (nat_of_integer (Z.Int.of_int 2)) (array_length a))
      (nat_of_integer (Z.Int.of_int 3));;

let rec ahm_update
  eq bhc k v hm =
    (let hma = ahm_update_aux eq bhc hm k v in
      (if ahm_filled hma then ahm_rehash bhc hma (hm_grow hma) else hma));;

let rec pop_tr (_A1, _A2)
  s = (let (a, (aa, (ab, bb))) = s in
       let x = minus_nat (as_length aa) one_nata in
       let xa =
         (let (_, bc) =
            whilea
              (fun (xe, _) ->
                less_nat xe
                  (if equal_nata (plus_nata x one_nata) (as_length aa)
                    then as_length a else as_get aa (plus_nata x one_nata)))
              (fun (ac, bc) ->
                (suc ac,
                  ahm_update (eq _A1) (bounded_hashcode_nat _A2) (as_get a ac)
                    (uminus_inta one_int) bc))
              (as_get aa x, ab)
            in
           bc)
         in
       let xb = as_take (as_top aa) a in
       let xc = as_pop aa in
        (xb, (xc, (xa, bb))));;

let rec glist_delete_aux
  eq x xa2 asa = match eq, x, xa2, asa with eq, x, [], asa -> asa
    | eq, x, y :: ys, asa ->
        (if eq x y then rev_append asa ys
          else glist_delete_aux eq x ys (y :: asa));;

let rec glist_delete eq x l = glist_delete_aux eq x l [];;

let rec lx_uppercase
  x = range (linorder_char, show_char)
        (Chara (true, false, false, false, false, false, true, false))
        (Chara (false, true, false, true, true, false, true, false)) x;;

let rec lx_lowercase
  x = range (linorder_char, show_char)
        (Chara (true, false, false, false, false, true, true, false))
        (Chara (false, true, false, true, true, true, true, false)) x;;

let rec lx_alpha
  x = bindb (alt lx_lowercase lx_uppercase) (fun xa -> return (sum_join xa)) x;;

let rec is_Nil a = (match a with [] -> true | _ :: _ -> false);;

let rec abstra_upd_impl (_A1, _A2, _A3, _A4)
  n = (fun ai bi ->
        (match ai
          with LT (x41a, x42a) ->
            (fun f_ () -> f_
              ((mtx_get (heap_DBMEntry _A4) (suc n) bi (x41a, zero_nata)) ())
              ())
              (fun x ->
                mtx_set (heap_DBMEntry _A4) (suc n) bi (x41a, zero_nata)
                  (min (ord_DBMEntry
                         (_A3, _A1.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.linordered_ab_semigroup_add_linordered_ab_monoid_add.linorder_linordered_ab_semigroup_add))
                    x (Lt x42a)))
          | LE (x41a, x42a) ->
            (fun f_ () -> f_
              ((mtx_get (heap_DBMEntry _A4) (suc n) bi (x41a, zero_nata)) ())
              ())
              (fun x ->
                mtx_set (heap_DBMEntry _A4) (suc n) bi (x41a, zero_nata)
                  (min (ord_DBMEntry
                         (_A3, _A1.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.linordered_ab_semigroup_add_linordered_ab_monoid_add.linorder_linordered_ab_semigroup_add))
                    x (Le x42a)))
          | EQ (x41a, x42a) ->
            (fun f_ () -> f_
              ((mtx_get (heap_DBMEntry _A4) (suc n) bi (zero_nata, x41a)) ())
              ())
              (fun x ->
                (fun f_ () -> f_
                  ((mtx_get (heap_DBMEntry _A4) (suc n) bi (x41a, zero_nata))
                  ()) ())
                  (fun x_a ->
                    (fun f_ () -> f_
                      ((mtx_set (heap_DBMEntry _A4) (suc n) bi (zero_nata, x41a)
                         (min (ord_DBMEntry
                                (_A3, _A1.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.linordered_ab_semigroup_add_linordered_ab_monoid_add.linorder_linordered_ab_semigroup_add))
                           x (Le (uminus _A2 x42a))))
                      ()) ())
                      (fun x_b ->
                        mtx_set (heap_DBMEntry _A4) (suc n) x_b
                          (x41a, zero_nata)
                          (min (ord_DBMEntry
                                 (_A3, _A1.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.linordered_ab_semigroup_add_linordered_ab_monoid_add.linorder_linordered_ab_semigroup_add))
                            x_a (Le x42a)))))
          | GT (x41a, x42a) ->
            (fun f_ () -> f_
              ((mtx_get (heap_DBMEntry _A4) (suc n) bi (zero_nata, x41a)) ())
              ())
              (fun x ->
                mtx_set (heap_DBMEntry _A4) (suc n) bi (zero_nata, x41a)
                  (min (ord_DBMEntry
                         (_A3, _A1.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.linordered_ab_semigroup_add_linordered_ab_monoid_add.linorder_linordered_ab_semigroup_add))
                    x (Lt (uminus _A2 x42a))))
          | GE (x41a, x42a) ->
            (fun f_ () -> f_
              ((mtx_get (heap_DBMEntry _A4) (suc n) bi (zero_nata, x41a)) ())
              ())
              (fun x ->
                mtx_set (heap_DBMEntry _A4) (suc n) bi (zero_nata, x41a)
                  (min (ord_DBMEntry
                         (_A3, _A1.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.linordered_ab_semigroup_add_linordered_ab_monoid_add.linorder_linordered_ab_semigroup_add))
                    x (Le (uminus _A2 x42a))))));;

let rec abstr_upd_impl (_A1, _A2, _A3, _A4)
  n = (fun ai ->
        imp_nfoldli ai (fun _ -> (fun () -> true))
          (abstra_upd_impl (_A1, _A2, _A3, _A4) n));;

let rec abstr_FW_impl (_A1, _A2, _A3, _A4)
  n = (fun ai bi ->
        (fun f_ () -> f_ ((abstr_upd_impl (_A1, _A2, _A3, _A4) n ai bi) ()) ())
          (fw_impl
            ((linordered_ab_monoid_add_DBMEntry (_A1, _A3)),
              (heap_DBMEntry _A4))
            n));;

let rec fold_error f x1 a = match f, x1, a with f, [], a -> Result a
                     | f, x :: xs, a -> binda (f x a) (fold_error f xs);;

let rec the_errors (Error es) = es;;

let rec find_max_nat
  n uu =
    (if equal_nata n zero_nata then zero_nata
      else (if uu (minus_nat n one_nata) then minus_nat n one_nata
             else find_max_nat (minus_nat n one_nata) uu));;

let rec stat_newnode x = (fun _ -> ()) x;;

let rec amtx_copy _A = array_copy _A;;

let rec amtx_dflt _A n m v = make _A (times_nat n m) (fun _ -> v);;

let rec gen_token ws p = bindb ws (fun _ -> p);;

let rec ll_from_list l = LL (size_list l, l);;

let rec show_pres = function Inr (ll, uu) -> Inr ll
                    | Inl e -> Inl e;;

let rec parse_all
  ws p =
    comp (comp (comp show_pres
                 (bindb p
                   (fun a ->
                     bindb ws
                       (fun _ -> bindb (eoi show_char) (fun _ -> return a)))))
           ll_from_list)
      explode;;

let rec norm_lower _A e t = (if dbm_lt _A e (Lt t) then Lt t else e);;

let rec norm_upper _A e t = (if dbm_lt _A (Le t) e then INF else e);;

let rec and_entry_impl
  n = (fun ai bib bia bi ->
        (fun f_ () -> f_
          ((mtx_get (heap_DBMEntry heap_int) (suc n) bi (ai, bib)) ()) ())
          (fun x ->
            mtx_set (heap_DBMEntry heap_int) (suc n) bi (ai, bib)
              (min (ord_DBMEntry (equal_int, linorder_int)) x bia)));;

let rec repair_pair_impl (_A1, _A2)
  n = (fun ai bia bi ->
        (fun f_ () -> f_ ((fwi_impl (_A1, _A2) n ai bi) ()) ())
          (fun x -> fwi_impl (_A1, _A2) n x bia));;

let rec restrict_zero_impl
  n = (fun ai bi ->
        (fun f_ () -> f_ ((and_entry_impl n bi zero_nata (Le zero_inta) ai) ())
          ())
          (fun x ->
            (fun f_ () -> f_ ((and_entry_impl n zero_nata bi (Le zero_inta) x)
              ()) ())
              (fun x_a ->
                repair_pair_impl
                  ((linordered_ab_monoid_add_DBMEntry
                     (linordered_cancel_ab_monoid_add_int, equal_int)),
                    (heap_DBMEntry heap_int))
                  n x_a bi zero_nata)));;

let rec pre_reset_impl
  n = (fun ai bi ->
        (fun f_ () -> f_ ((restrict_zero_impl n ai bi) ()) ())
          (fun x ->
            free_impl (linordered_cancel_ab_monoid_add_int, heap_int) n x bi));;

let rec and_entry_repair_impl
  n = (fun ai bib bia bi ->
        (fun f_ () -> f_ ((and_entry_impl n ai bib bia bi) ()) ())
          (fun x ->
            repair_pair_impl
              ((linordered_ab_monoid_add_DBMEntry
                 (linordered_cancel_ab_monoid_add_int, equal_int)),
                (heap_DBMEntry heap_int))
              n x ai bib));;

let rec upd_entry_impl
  n = (fun ai bib bia bi ->
        (fun f_ () -> f_
          ((mtx_get (heap_DBMEntry heap_int) (suc n) bi (ai, bib)) ()) ())
          (fun x ->
            (fun f_ () -> f_ ((amtx_copy (heap_DBMEntry heap_int) bia) ()) ())
              (and_entry_repair_impl n bib ai (neg_dbm_entry uminus_int x))));;

let rec gi_E (Gen_g_impl_ext (gi_V, gi_E, gi_V0, more)) = gi_E;;

let rec more (Gen_g_impl_ext (gi_V, gi_E, gi_V0, more)) = more;;

let rec combine_map f xs = combine (map f xs);;

let rec as_is_empty s = equal_nata (snd s) zero_nata;;

let rec times_int
  k l = Int_of_integer (Z.Int.mul (integer_of_int k) (integer_of_int l));;

let rec minus_set _A
  a x1 = match a, x1 with
    a, Coset xs -> Set (filter (fun x -> member _A x a) xs)
    | a, Set xs -> fold (remove _A) xs a;;

let rec gi_V0 (Gen_g_impl_ext (gi_V, gi_E, gi_V0, more)) = gi_V0;;

let rec select_edge_tr (_A1, _A2)
  s = (let (a, (aa, (ab, bb))) = s in
        (if as_is_empty bb then (None, (a, (aa, (ab, bb))))
          else (let (ac, bc) = as_top bb in
                 (if less_eq_nat (as_get aa (minus_nat (as_length aa) one_nata))
                       ac
                   then (let xa = gen_pick (fun x -> foldli (id x)) bc in
                         let xb = glist_delete (eq _A1) xa bc in
                         let xc =
                           (if is_Nil xb then as_pop bb
                             else as_set bb (minus_nat (as_length bb) one_nata)
                                    (ac, xb))
                           in
                          (Some xa, (a, (aa, (ab, xc)))))
                   else (None, (a, (aa, (ab, bb))))))));;

let rec ahm_lookup_aux
  eq bhc k a = list_map_lookup eq k (array_get a (bhc (array_length a) k));;

let rec ahm_lookup eq bhc k (HashMap (a, uu)) = ahm_lookup_aux eq bhc k a;;

let rec idx_of_tr (_A1, _A2)
  s v = (let (_, (aa, (ab, _))) = v in
         let x =
           (let Some i = ahm_lookup (eq _A1) (bounded_hashcode_nat _A2) s ab in
            let true = less_eq_int zero_inta i in
             nat i)
           in
         let xa =
           find_max_nat (as_length aa) (fun j -> less_eq_nat (as_get aa j) x) in
          xa);;

let rec collapse_tr (_A1, _A2)
  v s = (let (a, (aa, (ab, bb))) = s in
         let x = idx_of_tr (_A1, _A2) v (a, (aa, (ab, bb))) in
         let xa = as_take (plus_nata x one_nata) aa in
          (a, (xa, (ab, bb))));;

let rec as_singleton _B x = (array_of_list [x], one _B);;

let rec new_hashmap_with size = HashMap (new_array [] size, zero_nata);;

let rec ahm_empty def_size = new_hashmap_with def_size;;

let rec push_code (_A1, _A2)
  g_impl =
    (fun x (xa, (xb, (xc, xd))) ->
      (let _ = stat_newnode () in
       let y_a = as_length xa in
       let y_b = as_push xa x in
       let y_c = as_push xb y_a in
       let y_d =
         ahm_update (eq _A1) (bounded_hashcode_nat _A2) x (int_of_nat y_a) xc in
       let y_e =
         (if is_Nil (gi_E g_impl x) then xd
           else as_push xd (y_a, gi_E g_impl x))
         in
        (y_b, (y_c, (y_d, y_e)))));;

let rec compute_SCC_tr (_A1, _A2)
  g = (let _ = stat_start () in
       let xa = ([], ahm_empty (def_hashmap_size _A2 Type)) in
       let a =
         foldli (id (gi_V0 g)) (fun _ -> true)
           (fun xb (a, b) ->
             (if not (match ahm_lookup (eq _A1) (bounded_hashcode_nat _A2) xb b
                       with None -> false
                       | Some i ->
                         (if less_eq_int zero_inta i then false else true))
               then (let xc =
                       (a, (as_singleton one_nat xb,
                             (as_singleton one_nat zero_nata,
                               (ahm_update (eq _A1) (bounded_hashcode_nat _A2)
                                  xb (int_of_nat zero_nata) b,
                                 (if is_Nil (gi_E g xb)
                                   then as_empty zero_nat ()
                                   else as_singleton one_nat
  (zero_nata, gi_E g xb))))))
                       in
                     let (aa, (_, (_, (ad, _)))) =
                       whilea
                         (fun (_, xf) ->
                           not (as_is_empty (let (xg, (_, (_, _))) = xf in xg)))
                         (fun (aa, ba) ->
                           (match select_edge_tr (_A1, _A2) ba
                             with (None, bb) ->
                               (let xf = last_seg_tr _A2 bb in
                                let xg = pop_tr (_A1, _A2) bb in
                                let xh = xf :: aa in
                                 (xh, xg))
                             | (Some xf, bb) ->
                               (if (match
                                     ahm_lookup (eq _A1)
                                       (bounded_hashcode_nat _A2) xf
                                       (let (_, (_, (xl, _))) = bb in xl)
                                     with None -> false
                                     | Some i ->
                                       (if less_eq_int zero_inta i then true
 else false))
                                 then (let ab = collapse_tr (_A1, _A2) xf bb in
(aa, ab))
                                 else (if not
    (match
      ahm_lookup (eq _A1) (bounded_hashcode_nat _A2) xf
        (let (_, (_, (xl, _))) = bb in xl)
      with None -> false
      | Some i -> (if less_eq_int zero_inta i then false else true))
then (aa, push_code (_A1, _A2) g xf bb) else (aa, bb)))))
                         xc
                       in
                      (aa, ad))
               else (a, b)))
           xa
         in
       let (aa, _) = a in
       let _ = stat_stop () in
        aa);;

let rec constraint_clk = function LT (c, uu) -> c
                         | LE (c, uv) -> c
                         | EQ (c, uw) -> c
                         | GE (c, ux) -> c
                         | GT (c, uy) -> c;;

let rec get_entries_impl (_A1, _A2, _A3)
  n = (fun xi ->
        (fun f_ () -> f_
          ((imp_fora zero_nata (suc n)
             (fun xc sigma ->
               (fun f_ () -> f_
                 ((imp_fora zero_nata (suc n)
                    (fun xf sigmaa ->
                      (fun f_ () -> f_
                        ((mtx_get (heap_DBMEntry _A3) (suc n) xi (xc, xf)) ())
                        ())
                        (fun x ->
                          (fun () ->
                            ((if (less_nat zero_nata xc ||
                                   less_nat zero_nata xf) &&
                                   not (equal_DBMEntry _A2 x INF)
                               then op_list_prepend (xc, xf) op_list_empty
                               else op_list_empty) ::
                              sigmaa))))
                    op_list_empty)
                 ()) ())
                 (fun x ->
                   (fun f_ () -> f_
                     ((imp_nfoldli (op_list_rev (op_list_rev x))
                        (fun _ -> (fun () -> true))
                        (fun xf sigmaa -> (fun () -> (xf @ sigmaa)))
                        op_list_empty)
                     ()) ())
                     (fun x_c -> (fun () -> (x_c :: sigma)))))
             op_list_empty)
          ()) ())
          (fun x ->
            imp_nfoldli (op_list_rev (op_list_rev x))
              (fun _ -> (fun () -> true))
              (fun xc sigma -> (fun () -> (xc @ sigma))) op_list_empty));;

let rec upd_entries_impl
  n = (fun ai bib bia bi ->
        (fun f_ () -> f_
          ((imp_nfoldli bi (fun _ -> (fun () -> true))
             (fun xa sigma ->
               (fun f_ () -> f_ ((upd_entry_impl n ai bib xa bia) ()) ())
                 (fun x_b -> (fun () -> (x_b :: sigma))))
             op_list_empty)
          ()) ())
          (fun x ->
            imp_nfoldli x (fun _ -> (fun () -> true))
              (fun xb sigma -> (fun () -> (xb :: sigma))) op_list_empty));;

let rec constraint_pair = function LT (x, m) -> (x, m)
                          | LE (x, m) -> (x, m)
                          | EQ (x, m) -> (x, m)
                          | GE (x, m) -> (x, m)
                          | GT (x, m) -> (x, m);;

let rec check_passed_impl _A (_B1, _B2, _B3)
  succsi a_0i fi lei emptyi keyi copyi tracei qi =
    (fun f_ () -> f_ (a_0i ()) ())
      (fun x ->
        (fun f_ () -> f_ ((emptyi x) ()) ())
          (fun xa ->
            (fun f_ () -> f_ (a_0i ()) ())
              (fun xaa ->
                (fun f_ () -> f_ ((fi xaa) ()) ())
                  (fun xab ->
                    (fun f_ () -> f_
                      ((if not xa && xab
                         then (fun f_ () -> f_
                                ((ht_new (_B2, _B3) (heap_list _A)) ()) ())
                                (fun x_b -> (fun () -> (true, x_b)))
                         else (fun f_ () -> f_ (a_0i ()) ())
                                (fun xb ->
                                  (fun f_ () -> f_ ((emptyi xb) ()) ())
                                    (fun x_a ->
                                      (if x_a
then (fun f_ () -> f_ ((ht_new (_B2, _B3) (heap_list _A)) ()) ())
       (fun x_c -> (fun () -> (false, x_c)))
else (fun f_ () -> f_ (a_0i ()) ())
       (fun xc ->
         (fun f_ () -> f_ ((keyi xc) ()) ())
           (fun xd ->
             (fun f_ () -> f_ (a_0i ()) ())
               (fun xac ->
                 (fun f_ () -> f_ ((ht_new (_B2, _B3) (heap_list _A)) ()) ())
                   (fun xba ->
                     (fun f_ () -> f_
                       ((ht_update (_B1, _B2, _B3) (heap_list _A) xd [xac] xba)
                       ()) ())
                       (fun xe ->
                         (fun f_ () -> f_ (a_0i ()) ())
                           (fun xad ->
                             (fun f_ () -> f_
                               ((heap_WHILET
                                  (fun (_, (a1b, a2b)) ->
                                    (fun () ->
                                      (not a2b && not (op_list_is_empty a1b))))
                                  (fun (a1a, (a1b, a2b)) ->
                                    (let (a1c, a2c) =
                                       (match a1b
 with [] -> cODE_ABORT (fun _ -> (hd a1b, tl a1b)) | a :: b -> (a, b))
                                       in
                                      (fun f_ () -> f_ ((emptyi a1c) ()) ())
(fun x_e ->
  (if x_e then (fun () -> (a1a, (a2c, a2b)))
    else (fun f_ () -> f_ (tRACE_impl ()) ())
           (fun _ ->
             (fun f_ () -> f_
               ((tracei
                   [Chara (true, false, true, false, false, false, true, false);
                     Chara (false, false, false, true, true, true, true, false);
                     Chara (false, false, false, false, true, true, true,
                             false);
                     Chara (false, false, true, true, false, true, true, false);
                     Chara (true, true, true, true, false, true, true, false);
                     Chara (false, true, false, false, true, true, true, false);
                     Chara (true, false, true, false, false, true, true, false);
                     Chara (false, false, true, false, false, true, true,
                             false)]
                  a1c)
               ()) ())
               (fun _ ->
                 (fun f_ () -> f_ ((succsi a1c) ()) ())
                   (fun x_h ->
                     imp_nfoldli x_h (fun (_, (_, b)) -> (fun () -> (not b)))
                       (fun xl (a1d, (a1e, _)) ->
                         (fun f_ () -> f_ ((emptyi xl) ()) ())
                           (fun x_k ->
                             (if x_k then (fun () -> (a1d, (a1e, false)))
                               else (fun f_ () -> f_ ((fi xl) ()) ())
                                      (fun x_l ->
(if x_l then (fun () -> (a1d, (a1e, true)))
  else (fun f_ () -> f_ ((keyi xl) ()) ())
         (fun x_m ->
           (fun f_ () -> f_
             ((hms_extract (ht_lookup (_B1, _B2, _B3) (heap_list _A))
                (ht_delete (_B1, _B2, _B3) (heap_list _A)) x_m a1d)
             ()) ())
             (fun a ->
               (match a
                 with (None, a2f) ->
                   (fun f_ () -> f_ ((copyi xl) ()) ())
                     (fun xf ->
                       (fun f_ () -> f_
                         ((ht_update (_B1, _B2, _B3) (heap_list _A) x_m [xf]
                            a2f)
                         ()) ())
                         (fun x_o ->
                           (fun () -> (x_o, (op_list_prepend xl a1e, false)))))
                 | (Some x_o, a2f) ->
                   (fun f_ () -> f_ ((lso_bex_impl (lei xl) x_o) ()) ())
                     (fun x_p ->
                       (if x_p
                         then (fun f_ () -> f_
                                ((ht_update (_B1, _B2, _B3) (heap_list _A) x_m
                                   x_o a2f)
                                ()) ())
                                (fun x_q -> (fun () -> (x_q, (a1e, false))))
                         else (fun f_ () -> f_ ((copyi xl) ()) ())
                                (fun xf ->
                                  (fun f_ () -> f_
                                    ((ht_update (_B1, _B2, _B3) (heap_list _A)
                                       x_m (xf :: x_o) a2f)
                                    ()) ())
                                    (fun x_q ->
                                      (fun () ->
(x_q, (op_list_prepend xl a1e, false)))))))))))))))
                       (a1a, (a2c, false)))))))))
                                  (xe, (op_list_prepend xad [], false)))
                               ()) ())
                               (fun (a1a, (_, a2b)) ->
                                 (fun () -> (a2b, a1a)))))))))))))
                      ()) ())
                      (fun (_, a2) ->
                        (fun f_ () -> f_
                          ((ran_of_map_impl (_B1, _B2, _B3) (heap_list _A) a2)
                          ()) ())
                          (fun x_a ->
                            imp_nfoldli x_a
                              (fun sigma -> (fun () -> (not sigma)))
                              (fun xd _ ->
                                imp_nfoldli xd
                                  (fun sigma -> (fun () -> (not sigma)))
                                  (fun xg _ ->
                                    (fun f_ () -> f_ ((qi xg) ()) ())
                                      (fun x_g ->
(fun () -> (if x_g then true else false))))
                                  false)
                              false))))));;

let rec maxa _A
  (Set (x :: xs)) =
    fold (max _A.order_linorder.preorder_order.ord_preorder) xs x;;

let rec dbm_subset_impla (_A1, _A2)
  = (fun m a b ->
      imp_for zero_nata
        (times_nat (plus_nata m one_nata) (plus_nata m one_nata))
        (fun aa -> (fun () -> aa))
        (fun i _ ->
          (fun f_ () -> f_ ((ntha _A1 a i) ()) ())
            (fun x ->
              (fun f_ () -> f_ ((ntha _A1 b i) ()) ())
                (fun y -> (fun () -> (less_eq _A2 x y)))))
        true);;

let rec check_diag_impla (_A1, _A2)
  n = (fun ai bi ->
        imp_for zero_nata (suc ai) (fun sigma -> (fun () -> (not sigma)))
          (fun xb sigma ->
            (fun f_ () -> f_ ((mtx_get (heap_DBMEntry _A2) (suc n) bi (xb, xb))
              ()) ())
              (fun x ->
                (fun () ->
                  (less_DBMEntry
                     _A1.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.linordered_ab_semigroup_add_linordered_ab_monoid_add.linorder_linordered_ab_semigroup_add
                     x (Le (zero _A1.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.ordered_comm_monoid_add_linordered_ab_monoid_add.comm_monoid_add_ordered_comm_monoid_add.monoid_add_comm_monoid_add.zero_monoid_add)) ||
                    sigma))))
          false);;

let rec dbm_minus_canonical_impl
  n = (fun ai bi ->
        (fun f_ () -> f_
          ((get_entries_impl
             (linordered_cancel_ab_monoid_add_int, equal_int, heap_int) n bi)
          ()) ())
          (fun x ->
            (fun f_ () -> f_
              ((imp_nfoldli x (fun _ -> (fun () -> true))
                 (fun xb sigma ->
                   (fun f_ () -> f_
                     ((upd_entries_impl n (fst xb) (snd xb) bi ai) ()) ())
                     (fun xa ->
                       (fun f_ () -> f_
                         ((imp_nfoldli xa (fun _ -> (fun () -> true))
                            (fun xe sigmaa -> (fun () -> (xe :: sigmaa)))
                            op_list_empty)
                         ()) ())
                         (fun x_c -> (fun () -> (x_c @ sigma)))))
                 op_list_empty)
              ()) ())
              (fun xa ->
                (fun f_ () -> f_
                  ((imp_nfoldli xa (fun _ -> (fun () -> true))
                     (fun xb sigma -> (fun () -> (xb :: sigma))) op_list_empty)
                  ()) ())
                  (fun xb ->
                    (fun f_ () -> f_
                      ((imp_nfoldli xb (fun _ -> (fun () -> true))
                         (fun xba sigma ->
                           (fun f_ () -> f_
                             ((check_diag_impla
                                (linordered_cancel_ab_monoid_add_int, heap_int)
                                n n xba)
                             ()) ())
                             (fun xc ->
                               (fun () ->
                                 (if not xc then op_list_prepend xba sigma
                                   else sigma))))
                         op_list_empty)
                      ()) ())
                      (fun xc ->
                        imp_nfoldli xc (fun _ -> (fun () -> true))
                          (fun xba sigma -> (fun () -> (xba :: sigma)))
                          op_list_empty)))));;

let rec dbm_subset_fed_impl
  n = (fun ai bi ->
        (fun f_ () -> f_
          ((imp_nfoldli bi (fun _ -> (fun () -> true))
             (fun xa sigma ->
               (fun f_ () -> f_
                 ((check_diag_impla
                    (linordered_cancel_ab_monoid_add_int, heap_int) n n xa)
                 ()) ())
                 (fun x ->
                   (fun () ->
                     (if not x then op_list_prepend xa sigma else sigma))))
             op_list_empty)
          ()) ())
          (fun x ->
            (let xa = op_list_rev x in
              (if op_list_is_empty xa
                then check_diag_impla
                       (linordered_cancel_ab_monoid_add_int, heap_int) n n ai
                else (fun f_ () -> f_
                       ((imp_nfoldli xa (fun sigma -> (fun () -> (not sigma)))
                          (fun xc sigma ->
                            (fun f_ () -> f_
                              ((dbm_subset_impla
                                 ((heap_DBMEntry heap_int),
                                   (ord_DBMEntry (equal_int, linorder_int)))
                                 n ai xc)
                              ()) ())
                              (fun x_d ->
                                (fun () -> (if x_d then true else sigma))))
                          false)
                       ()) ())
                       (fun x_b ->
                         (if x_b then (fun () -> true)
                           else (fun f_ () -> f_
                                  ((imp_nfoldli xa (fun _ -> (fun () -> true))
                                     (fun xd sigma ->
                                       dbm_minus_canonical_impl n sigma xd)
                                     (op_list_prepend ai op_list_empty))
                                  ()) ())
                                  (fun x_c ->
                                    (fun () -> (op_list_is_empty x_c)))))))));;

let rec pre_reset_list_impl
  n = (fun ai bi ->
        imp_nfoldli bi (fun _ -> (fun () -> true))
          (fun x sigma -> pre_reset_impl n sigma x) ai);;

let rec is_result = function Result x1 -> true
                    | Error x2 -> false;;

let rec tk_div
  x = gen_token lx_ws
        (exactly (equal_char, show_char)
          [Chara (true, true, true, true, false, true, false, false)])
        x;;

let rec tk_plus
  x = gen_token lx_ws
        (exactly (equal_char, show_char)
          [Chara (true, true, false, true, false, true, false, false)])
        x;;

let rec collect_clock_pairs cc = image constraint_pair (Set cc);;

let rec tk_minus
  x = gen_token lx_ws
        (exactly (equal_char, show_char)
          [Chara (true, false, true, true, false, true, false, false)])
        x;;

let rec tk_times
  x = gen_token lx_ws
        (exactly (equal_char, show_char)
          [Chara (false, true, false, true, false, true, false, false)])
        x;;

let rec map_exp
  f x1 = match f, x1 with f, Const x1 -> Const x1
    | f, Var x2 -> Var (f x2)
    | f, If_then_else (x31, x32, x33) ->
        If_then_else (map_bexp f x31, map_exp f x32, map_exp f x33)
    | f, Binop (x41, x42, x43) -> Binop (x41, map_exp f x42, map_exp f x43)
    | f, Unop (x51, x52) -> Unop (x51, map_exp f x52)
and map_bexp f x1 = match f, x1 with f, True -> True
               | f, Not x2 -> Not (map_bexp f x2)
               | f, And (x31, x32) -> And (map_bexp f x31, map_bexp f x32)
               | f, Or (x41, x42) -> Or (map_bexp f x41, map_bexp f x42)
               | f, Imply (x51, x52) -> Imply (map_bexp f x51, map_bexp f x52)
               | f, Eq (x61, x62) -> Eq (map_exp f x61, map_exp f x62)
               | f, Lea (x71, x72) -> Lea (map_exp f x71, map_exp f x72)
               | f, Lta (x81, x82) -> Lta (map_exp f x81, map_exp f x82)
               | f, Ge (x91, x92) -> Ge (map_exp f x91, map_exp f x92)
               | f, Gt (x101, x102) -> Gt (map_exp f x101, map_exp f x102);;

let rec set_exp _A
  = function Const x1 -> bot_set
    | Var x2 -> insert _A x2 bot_set
    | If_then_else (x31, x32, x33) ->
        sup_set _A (sup_set _A (set_bexp _A x31) (set_exp _A x32))
          (set_exp _A x33)
    | Binop (x41, x42, x43) -> sup_set _A (set_exp _A x42) (set_exp _A x43)
    | Unop (x51, x52) -> set_exp _A x52
and set_bexp _A
  = function True -> bot_set
    | Not x2 -> set_bexp _A x2
    | And (x31, x32) -> sup_set _A (set_bexp _A x31) (set_bexp _A x32)
    | Or (x41, x42) -> sup_set _A (set_bexp _A x41) (set_bexp _A x42)
    | Imply (x51, x52) -> sup_set _A (set_bexp _A x51) (set_bexp _A x52)
    | Eq (x61, x62) -> sup_set _A (set_exp _A x61) (set_exp _A x62)
    | Lea (x71, x72) -> sup_set _A (set_exp _A x71) (set_exp _A x72)
    | Lta (x81, x82) -> sup_set _A (set_exp _A x81) (set_exp _A x82)
    | Ge (x91, x92) -> sup_set _A (set_exp _A x91) (set_exp _A x92)
    | Gt (x101, x102) -> sup_set _A (set_exp _A x101) (set_exp _A x102);;

let rec tk_lparen
  x = gen_token lx_ws
        (exactly (equal_char, show_char)
          [Chara (false, false, false, true, false, true, false, false)])
        x;;

let rec tk_rparen
  x = gen_token lx_ws
        (exactly (equal_char, show_char)
          [Chara (true, false, false, true, false, true, false, false)])
        x;;

let rec dfs_map_impl_0 _A (_B1, _B2, _B3)
  succsi lei keyi copyi x =
    (let (a1, (a1a, a2a)) = x in
      (fun f_ () -> f_ ((keyi a2a) ()) ())
        (fun xa ->
          (fun f_ () -> f_
            ((hms_extract (ht_lookup (_B1, _B2, _B3) (heap_list _A))
               (ht_delete (_B1, _B2, _B3) (heap_list _A)) xa a1a)
            ()) ())
            (fun xaa ->
              (fun f_ () -> f_
                ((match xaa with (None, a2b) -> (fun () -> (a2b, false))
                   | (Some x_c, a2b) ->
                     (fun f_ () -> f_
                       ((imp_nfoldli x_c (fun sigma -> (fun () -> (not sigma)))
                          (fun xf sigma ->
                            (fun f_ () -> f_ ((lei xf a2a) ()) ())
                              (fun x_f -> (fun () -> (x_f || sigma))))
                          false)
                       ()) ())
                       (fun x_d ->
                         (fun f_ () -> f_
                           ((ht_update (_B1, _B2, _B3) (heap_list _A) xa x_c
                              a2b)
                           ()) ())
                           (fun x_e -> (fun () -> (x_e, x_d)))))
                ()) ())
                (fun a ->
                  (match a with (a1b, true) -> (fun () -> (a1, (a1b, true)))
                    | (a1b, false) ->
                      (fun f_ () -> f_ ((keyi a2a) ()) ())
                        (fun xb ->
                          (fun f_ () -> f_
                            ((hms_extract
                               (ht_lookup (_B1, _B2, _B3) (heap_list _A))
                               (ht_delete (_B1, _B2, _B3) (heap_list _A)) xb a1)
                            ()) ())
                            (fun xab ->
                              (fun f_ () -> f_
                                ((match xab
                                   with (None, a2c) -> (fun () -> (a2c, false))
                                   | (Some x_e, a2c) ->
                                     (fun f_ () -> f_
                                       ((lso_bex_impl (lei a2a) x_e) ()) ())
                                       (fun x_f ->
 (fun f_ () -> f_ ((ht_update (_B1, _B2, _B3) (heap_list _A) xb x_e a2c) ()) ())
   (fun x_g -> (fun () -> (x_g, x_f)))))
                                ()) ())
                                (fun aa ->
                                  (match aa
                                    with (a1c, true) ->
                                      (fun () -> (a1c, (a1b, false)))
                                    | (a1c, false) ->
                                      (fun f_ () -> f_ ((copyi a2a) ()) ())
(fun xc ->
  (fun f_ () -> f_ ((keyi xc) ()) ())
    (fun xd ->
      (fun f_ () -> f_
        ((hms_extract (ht_lookup (_B1, _B2, _B3) (heap_list _A))
           (ht_delete (_B1, _B2, _B3) (heap_list _A)) xd a1b)
        ()) ())
        (fun xac ->
          (fun f_ () -> f_
            ((match xac
               with (None, a2d) ->
                 (fun f_ () -> f_ ((copyi a2a) ()) ())
                   (fun xad ->
                     (fun f_ () -> f_
                       ((ht_update (_B1, _B2, _B3) (heap_list _A) xd
                          (op_list_prepend xad []) a2d)
                       ()) ())
                       (fun x_h -> (fun () -> ((), x_h))))
               | (Some x_g, a2d) ->
                 (fun f_ () -> f_ ((copyi a2a) ()) ())
                   (fun xad ->
                     (fun f_ () -> f_
                       ((ht_update (_B1, _B2, _B3) (heap_list _A) xd
                          (op_list_prepend xad x_g) a2d)
                       ()) ())
                       (fun x_i -> (fun () -> ((), x_i)))))
            ()) ())
            (fun (_, a2d) ->
              (fun f_ () -> f_ ((succsi a2a) ()) ())
                (fun xe ->
                  (fun f_ () -> f_
                    ((imp_nfoldli xe (fun (_, (_, b)) -> (fun () -> (not b)))
                       (fun xk (a1e, (a1f, _)) ->
                         dfs_map_impl_0 _A (_B1, _B2, _B3) succsi lei keyi copyi
                           (a1e, (a1f, xk)))
                       (a1c, (a2d, false)))
                    ()) ())
                    (fun (a1e, (a1f, a2f)) ->
                      (fun f_ () -> f_ ((copyi a2a) ()) ())
                        (fun xf ->
                          (fun f_ () -> f_ ((keyi xf) ()) ())
                            (fun xg ->
                              (fun f_ () -> f_
                                ((hms_extract
                                   (ht_lookup (_B1, _B2, _B3) (heap_list _A))
                                   (ht_delete (_B1, _B2, _B3) (heap_list _A)) xg
                                   a1f)
                                ()) ())
                                (fun xad ->
                                  (fun f_ () -> f_
                                    ((match xad
                                       with (None, a2g) ->
 (fun f_ () -> f_ ((ht_update (_B1, _B2, _B3) (heap_list _A) xg [] a2g) ()) ())
   (fun x_k -> (fun () -> ((), x_k)))
                                       | (Some x_j, a2g) ->
 (fun f_ () -> f_
   ((ht_update (_B1, _B2, _B3) (heap_list _A) xg
      (if op_list_is_empty x_j then [] else op_list_tl x_j) a2g)
   ()) ())
   (fun x_l -> (fun () -> ((), x_l))))
                                    ()) ())
                                    (fun (_, a2g) ->
                                      (fun f_ () -> f_ ((copyi a2a) ()) ())
(fun xh ->
  (fun f_ () -> f_ ((keyi xh) ()) ())
    (fun xi ->
      (fun f_ () -> f_
        ((hms_extract (ht_lookup (_B1, _B2, _B3) (heap_list _A))
           (ht_delete (_B1, _B2, _B3) (heap_list _A)) xi a1e)
        ()) ())
        (fun xae ->
          (fun f_ () -> f_
            ((match xae
               with (None, a2h) ->
                 (fun f_ () -> f_ ((copyi a2a) ()) ())
                   (fun xaf ->
                     (fun f_ () -> f_
                       ((ht_update (_B1, _B2, _B3) (heap_list _A) xi [xaf] a2h)
                       ()) ())
                       (fun x_m -> (fun () -> ((), x_m))))
               | (Some x_l, a2h) ->
                 (fun f_ () -> f_ ((copyi a2a) ()) ())
                   (fun xaf ->
                     (fun f_ () -> f_
                       ((ht_update (_B1, _B2, _B3) (heap_list _A) xi
                          (xaf :: x_l) a2h)
                       ()) ())
                       (fun x_n -> (fun () -> ((), x_n)))))
            ()) ())
            (fun (_, a2h) ->
              (fun f_ () -> f_ (tRACE_impl ()) ())
                (fun _ ->
                  (fun () -> (a2h, (a2g, a2f)))))))))))))))))))))))))));;

let rec sup_seta _A (Set xs) = fold (sup_set _A) xs bot_set;;

let rec make_string (_A1, _A2, _A3)
  show_clock show_num e i j =
    (if equal_nata i j
      then (if less_DBMEntry
                 _A1.linordered_cancel_ab_monoid_add_linordered_ab_group_add.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.linordered_ab_semigroup_add_linordered_ab_monoid_add.linorder_linordered_ab_semigroup_add
                 e (zero_DBMEntrya
                     _A1.ordered_ab_group_add_linordered_ab_group_add.ab_group_add_ordered_ab_group_add.group_add_ab_group_add.monoid_add_group_add.zero_monoid_add)
             then Some [Chara (true, false, true, false, false, false, true,
                                false);
                         Chara (true, false, true, true, false, false, true,
                                 false);
                         Chara (false, false, false, false, true, false, true,
                                 false);
                         Chara (false, false, true, false, true, false, true,
                                 false);
                         Chara (true, false, false, true, true, false, true,
                                 false)]
             else None)
      else (if equal_nata i zero_nata
             then (match e
                    with Le a ->
                      (if eq _A2 a
                            (zero _A1.ordered_ab_group_add_linordered_ab_group_add.ab_group_add_ordered_ab_group_add.group_add_ab_group_add.monoid_add_group_add.zero_monoid_add)
                        then None
                        else Some (show_clock j @
                                    [Chara (false, false, false, false, false,
     true, false, false);
                                      Chara
(false, true, true, true, true, true, false, false);
                                      Chara
(true, false, true, true, true, true, false, false);
                                      Chara
(false, false, false, false, false, true, false, false)] @
                                      show_num
(uminus
  _A1.ordered_ab_group_add_linordered_ab_group_add.ab_group_add_ordered_ab_group_add.group_add_ab_group_add.uminus_group_add
  a)))
                    | Lt a ->
                      Some (show_clock j @
                             [Chara (false, false, false, false, false, true,
                                      false, false);
                               Chara (false, true, true, true, true, true,
                                       false, false);
                               Chara (false, false, false, false, false, true,
                                       false, false)] @
                               show_num
                                 (uminus
                                   _A1.ordered_ab_group_add_linordered_ab_group_add.ab_group_add_ordered_ab_group_add.group_add_ab_group_add.uminus_group_add
                                   a))
                    | INF -> None)
             else (if equal_nata j zero_nata
                    then (match e
                           with Le a ->
                             Some (show_clock i @
                                    [Chara (false, false, false, false, false,
     true, false, false);
                                      Chara
(false, false, true, true, true, true, false, false);
                                      Chara
(true, false, true, true, true, true, false, false);
                                      Chara
(false, false, false, false, false, true, false, false)] @
                                      show_num a)
                           | Lt a ->
                             Some (show_clock i @
                                    [Chara (false, false, false, false, false,
     true, false, false);
                                      Chara
(false, false, true, true, true, true, false, false);
                                      Chara
(false, false, false, false, false, true, false, false)] @
                                      show_num a)
                           | INF -> None)
                    else (match e
                           with Le a ->
                             Some (show_clock i @
                                    [Chara (false, false, false, false, false,
     true, false, false);
                                      Chara
(true, false, true, true, false, true, false, false);
                                      Chara
(false, false, false, false, false, true, false, false)] @
                                      show_clock j @
[Chara (false, false, false, false, false, true, false, false);
  Chara (false, false, true, true, true, true, false, false);
  Chara (true, false, true, true, true, true, false, false);
  Chara (false, false, false, false, false, true, false, false)] @
  show_num a)
                           | Lt a ->
                             Some (show_clock i @
                                    [Chara (false, false, false, false, false,
     true, false, false);
                                      Chara
(true, false, true, true, false, true, false, false);
                                      Chara
(false, false, false, false, false, true, false, false)] @
                                      show_clock j @
[Chara (false, false, false, false, false, true, false, false);
  Chara (false, false, true, true, true, true, false, false);
  Chara (false, false, false, false, false, true, false, false)] @
  show_num a)
                           | INF -> None))));;

let rec dfs_map_impl _A (_B1, _B2, _B3)
  succsi a_0i lei keyi copyi =
    (fun f_ () -> f_ ((ht_new (_B2, _B3) (heap_list _A)) ()) ())
      (fun x ->
        (fun f_ () -> f_ ((ht_new (_B2, _B3) (heap_list _A)) ()) ())
          (fun xa ->
            (fun f_ () -> f_ (a_0i ()) ())
              (fun xb ->
                (fun f_ () -> f_
                  ((dfs_map_impl_0 _A (_B1, _B2, _B3) succsi lei keyi copyi
                     (x, (xa, xb)))
                  ()) ())
                  (fun xc ->
                    (fun f_ () -> f_
                      ((let (a1, (_, a2a)) = xc in (fun () -> (a2a, a1))) ())
                      ())
                      (fun (a1, _) -> (fun () -> a1))))));;

let rec err s = Error [s];;

let rec geta _A
  m x = (match m x
          with None ->
            Error ["(Get) key not found: " ^
                     implode (shows_prec _A zero_nata x [])]
          | Some a -> Result a);;

let rec pad
  m s = replicate m
          (Chara (false, false, false, false, false, true, false, false)) @
          s;;

let rec norm_upd_impl (_A1, _A2)
  n = (fun ai bia bi ->
        (fun f_ () -> f_
          ((mtx_get (heap_DBMEntry _A2) (suc n) ai (zero_nata, zero_nata)) ())
          ())
          (fun x ->
            (fun f_ () -> f_
              ((mtx_set (heap_DBMEntry _A2) (suc n) ai (zero_nata, zero_nata)
                 (norm_lower
                   _A1.linordered_cancel_ab_monoid_add_linordered_ab_group_add.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.linordered_ab_semigroup_add_linordered_ab_monoid_add.linorder_linordered_ab_semigroup_add
                   (norm_upper
                     _A1.linordered_cancel_ab_monoid_add_linordered_ab_group_add.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.linordered_ab_semigroup_add_linordered_ab_monoid_add.linorder_linordered_ab_semigroup_add
                     x (zero _A1.ordered_ab_group_add_linordered_ab_group_add.ab_group_add_ordered_ab_group_add.group_add_ab_group_add.monoid_add_group_add.zero_monoid_add))
                   (zero _A1.ordered_ab_group_add_linordered_ab_group_add.ab_group_add_ordered_ab_group_add.group_add_ab_group_add.monoid_add_group_add.zero_monoid_add)))
              ()) ())
              (fun xa ->
                (fun f_ () -> f_
                  ((imp_fora one_nata (suc bi)
                     (fun xc sigma ->
                       (fun f_ () -> f_
                         ((mtx_get (heap_DBMEntry _A2) (suc n) sigma
                            (zero_nata, xc))
                         ()) ())
                         (fun xb ->
                           mtx_set (heap_DBMEntry _A2) (suc n) sigma
                             (zero_nata, xc)
                             (norm_lower
                               _A1.linordered_cancel_ab_monoid_add_linordered_ab_group_add.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.linordered_ab_semigroup_add_linordered_ab_monoid_add.linorder_linordered_ab_semigroup_add
                               (norm_upper
                                 _A1.linordered_cancel_ab_monoid_add_linordered_ab_group_add.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.linordered_ab_semigroup_add_linordered_ab_monoid_add.linorder_linordered_ab_semigroup_add
                                 xb (zero _A1.ordered_ab_group_add_linordered_ab_group_add.ab_group_add_ordered_ab_group_add.group_add_ab_group_add.monoid_add_group_add.zero_monoid_add))
                               (uminus
                                 _A1.ordered_ab_group_add_linordered_ab_group_add.ab_group_add_ordered_ab_group_add.group_add_ab_group_add.uminus_group_add
                                 (sub bia xc)))))
                     xa)
                  ()) ())
                  (imp_fora one_nata (suc bi)
                    (fun xb sigma ->
                      (fun f_ () -> f_
                        ((mtx_get (heap_DBMEntry _A2) (suc n) sigma
                           (xb, zero_nata))
                        ()) ())
                        (fun xc ->
                          (fun f_ () -> f_
                            ((mtx_set (heap_DBMEntry _A2) (suc n) sigma
                               (xb, zero_nata)
                               (norm_lower
                                 _A1.linordered_cancel_ab_monoid_add_linordered_ab_group_add.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.linordered_ab_semigroup_add_linordered_ab_monoid_add.linorder_linordered_ab_semigroup_add
                                 (norm_upper
                                   _A1.linordered_cancel_ab_monoid_add_linordered_ab_group_add.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.linordered_ab_semigroup_add_linordered_ab_monoid_add.linorder_linordered_ab_semigroup_add
                                   xc (sub bia xb))
                                 (zero _A1.ordered_ab_group_add_linordered_ab_group_add.ab_group_add_ordered_ab_group_add.group_add_ab_group_add.monoid_add_group_add.zero_monoid_add)))
                            ()) ())
                            (imp_fora one_nata (suc bi)
                              (fun xe sigmaa ->
                                (fun f_ () -> f_
                                  ((mtx_get (heap_DBMEntry _A2) (suc n) sigmaa
                                     (xb, xe))
                                  ()) ())
                                  (fun xd ->
                                    mtx_set (heap_DBMEntry _A2) (suc n) sigmaa
                                      (xb, xe)
                                      (norm_lower
_A1.linordered_cancel_ab_monoid_add_linordered_ab_group_add.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.linordered_ab_semigroup_add_linordered_ab_monoid_add.linorder_linordered_ab_semigroup_add
(norm_upper
  _A1.linordered_cancel_ab_monoid_add_linordered_ab_group_add.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.linordered_ab_semigroup_add_linordered_ab_monoid_add.linorder_linordered_ab_semigroup_add
  xd (sub bia xb))
(uminus
  _A1.ordered_ab_group_add_linordered_ab_group_add.ab_group_add_ordered_ab_group_add.group_add_ab_group_add.uminus_group_add
  (sub bia xe))))))))))));;

let rec dbm_list_to_string (_A1, _A2, _A3)
  n show_clock show_num xs =
    app (comp (comp (comp (comp concat
                            (intersperse
                              [Chara (false, false, true, true, false, true,
                                       false, false);
                                Chara (false, false, false, false, false, true,
false, false)]))
                      rev)
                snd)
          snd)
      (fold (fun e (i, (j, acc)) ->
              (let v = make_string (_A1, _A2, _A3) show_clock show_num e i j in
               let ja = modulo_nat (plus_nata j one_nata) (plus_nata n one_nata)
                 in
               let ia =
                 (if equal_nata ja zero_nata then plus_nata i one_nata else i)
                 in
                (match v with None -> (ia, (ja, acc))
                  | Some s -> (ia, (ja, s :: acc)))))
        xs (zero_nata, (zero_nata, [])));;

let rec dbm_to_list_impl (_A1, _A2)
  n = (fun xi ->
        (fun f_ () -> f_
          ((imp_fora zero_nata (suc n)
             (fun xc ->
               imp_fora zero_nata (suc n)
                 (fun xe sigma ->
                   (fun f_ () -> f_ ((mtx_get _A2 (suc n) xi (xc, xe)) ()) ())
                     (fun x_e -> (fun () -> (x_e :: sigma)))))
             [])
          ()) ())
          (fun x -> (fun () -> (op_list_rev x))));;

let rec show_dbm_impl (_A1, _A2, _A3)
  n show_clock show_num =
    (fun xi ->
      (fun f_ () -> f_
        ((dbm_to_list_impl
           ((linordered_ab_monoid_add_DBMEntry
              (_A1.linordered_cancel_ab_monoid_add_linordered_ab_group_add,
                _A2)),
             (heap_DBMEntry _A3))
           n xi)
        ()) ())
        (fun x ->
          (fun () ->
            (dbm_list_to_string (_A1, _A2, _A3) n show_clock show_num x))));;

let rec scan_parens
  lparen rparen inner =
    bindb (gen_token lx_ws (exactly (equal_char, show_char) lparen))
      (fun _ ->
        bindb (gen_token lx_ws inner)
          (fun a ->
            bindb (gen_token lx_ws (exactly (equal_char, show_char) rparen))
              (fun _ -> return a)));;

let rec lx_underscore
  x = bindb (exactly (equal_char, show_char)
              [Chara (true, true, true, true, true, false, true, false)])
        (fun _ ->
          return (Chara (true, true, true, true, true, false, true, false)))
        x;;

let rec lx_hyphen
  x = bindb (exactly (equal_char, show_char)
              [Chara (true, false, true, true, false, true, false, false)])
        (fun _ ->
          return (Chara (true, false, true, true, false, true, false, false)))
        x;;

let rec ta_var_ident
  x = bindb (alt (bindb (alt lx_alpha lx_digit)
                   (fun xa -> return (sum_join xa)))
              lx_underscore)
        (fun xa ->
          bindb (repeat
                  (bindb
                    (alt (bindb (alt lx_alpha lx_digit)
                           (fun xb -> return (sum_join xb)))
                      (bindb (alt lx_underscore lx_hyphen)
                        (fun xb -> return (sum_join xb))))
                    (fun xb -> return (sum_join xb))))
            (fun xaa ->
              return (uncurry (fun a b -> a :: b) (sum_join xa, xaa))))
        x;;

let rec scan_var x = ta_var_ident x;;

let rec divide_int
  k l = Int_of_integer (divide_integer (integer_of_int k) (integer_of_int l));;

let rec scan_infix_pair
  a b s =
    bindb (gen_token lx_ws a)
      (fun aa ->
        bindb (gen_token lx_ws (exactly (equal_char, show_char) s))
          (fun _ -> bindb (gen_token lx_ws b) (fun ba -> return (aa, ba))));;

let rec aexp
  l = bindb (alt (bindb (gen_token lx_ws lx_int) (fun x -> return (Const x)))
              (bindb
                (alt (bindb (gen_token lx_ws scan_var)
                       (fun x -> return (comp (fun a -> Var a) implode x)))
                  (bindb
                    (alt (bindb
                           (scan_parens
                             [Chara (false, false, false, true, false, true,
                                      false, false)]
                             [Chara (true, false, false, true, false, true,
                                      false, false)]
                             (bindb (gen_token lx_ws scan_exp)
                               (fun a ->
                                 bindb (gen_token lx_ws
 (exactly (equal_char, show_char)
   [Chara (true, true, true, true, true, true, false, false)]))
                                   (fun _ ->
                                     bindb (gen_token lx_ws scan_7)
                                       (fun x ->
 bindb (gen_token lx_ws
         (exactly (equal_char, show_char)
           [Chara (false, true, false, true, true, true, false, false)]))
   (fun _ -> bindb scan_exp (fun xa -> return (a, (x, xa)))))))))
                           (fun x ->
                             return
                               (let (e1, a) = x in
                                let (b, aa) = a in
                                 If_then_else (b, e1, aa))))
                      (bindb (gen_token lx_ws tk_lparen)
                        (fun _ ->
                          bindb (gen_token lx_ws scan_exp)
                            (fun a -> bindb tk_rparen (fun _ -> return a)))))
                    (fun x -> return (sum_join x))))
                (fun x -> return (sum_join x))))
        (fun x -> return (sum_join x)) l
and mexp
  l = chainL1 aexp
        (bindb
          (alt (bindb tk_times (fun _ -> return times_int))
            (bindb tk_div (fun _ -> return divide_int)))
          (fun x -> return (fun a b -> Binop (sum_join x, a, b))))
        l
and scan_exp
  l = chainL1 mexp
        (bindb
          (alt (bindb tk_plus (fun _ -> return plus_inta))
            (bindb tk_minus (fun _ -> return minus_inta)))
          (fun x -> return (fun a b -> Binop (sum_join x, a, b))))
        l
and scan_0
  l = bindb (alt (bindb
                   (gen_token lx_ws
                     (bindb
                       (alt (exactly (equal_char, show_char)
                              [Chara (false, true, true, true, true, true, true,
                                       false)])
                         (exactly (equal_char, show_char)
                           [Chara (true, false, false, false, false, true,
                                    false, false)]))
                       (fun x -> return (sum_join x))))
                   (fun _ ->
                     bindb (scan_parens
                             [Chara (false, false, false, true, false, true,
                                      false, false)]
                             [Chara (true, false, false, true, false, true,
                                      false, false)]
                             scan_7)
                       (fun x -> return (Not x))))
              (bindb
                (alt (bindb
                       (gen_token lx_ws
                         (exactly (equal_char, show_char)
                           [Chara (false, false, true, false, true, true, true,
                                    false);
                             Chara (false, true, false, false, true, true, true,
                                     false);
                             Chara (true, false, true, false, true, true, true,
                                     false);
                             Chara (true, false, true, false, false, true, true,
                                     false)]))
                       (fun _ -> return True))
                  (bindb
                    (alt (bindb
                           (scan_infix_pair aexp aexp
                             [Chara (false, false, true, true, true, true,
                                      false, false);
                               Chara (true, false, true, true, true, true,
                                       false, false)])
                           (fun x ->
                             return (uncurry (fun a b -> Lea (a, b)) x)))
                      (bindb
                        (alt (bindb
                               (scan_infix_pair aexp aexp
                                 [Chara (false, false, true, true, true, true,
  false, false)])
                               (fun x ->
                                 return (uncurry (fun a b -> Lta (a, b)) x)))
                          (bindb
                            (alt (bindb
                                   (scan_infix_pair aexp aexp
                                     [Chara
(true, false, true, true, true, true, false, false);
                                       Chara
 (true, false, true, true, true, true, false, false)])
                                   (fun x ->
                                     return (uncurry (fun a b -> Eq (a, b)) x)))
                              (bindb
                                (alt (bindb
                                       (scan_infix_pair aexp aexp
 [Chara (false, true, true, true, true, true, false, false)])
                                       (fun x ->
 return (uncurry (fun a b -> Gt (a, b)) x)))
                                  (bindb
                                    (alt (bindb
   (scan_infix_pair aexp aexp
     [Chara (false, true, true, true, true, true, false, false);
       Chara (true, false, true, true, true, true, false, false)])
   (fun x -> return (uncurry (fun a b -> Ge (a, b)) x)))
                                      (scan_parens
[Chara (false, false, false, true, false, true, false, false)]
[Chara (true, false, false, true, false, true, false, false)] scan_7))
                                    (fun x -> return (sum_join x))))
                                (fun x -> return (sum_join x))))
                            (fun x -> return (sum_join x))))
                        (fun x -> return (sum_join x))))
                    (fun x -> return (sum_join x))))
                (fun x -> return (sum_join x))))
        (fun x -> return (sum_join x)) l
and scan_6
  l = bindb (alt (bindb
                   (scan_infix_pair scan_0 scan_6
                     [Chara (false, true, true, false, false, true, false,
                              false);
                       Chara (false, true, true, false, false, true, false,
                               false)])
                   (fun x -> return (uncurry (fun a b -> And (a, b)) x)))
              scan_0)
        (fun x -> return (sum_join x)) l
and scan_7
  l = bindb (alt (bindb
                   (scan_infix_pair scan_6 scan_7
                     [Chara (true, false, true, true, false, true, false,
                              false);
                       Chara (false, true, true, true, true, true, false,
                               false)])
                   (fun x -> return (uncurry (fun a b -> Imply (a, b)) x)))
              (bindb
                (alt (bindb
                       (scan_infix_pair scan_6 scan_7
                         [Chara (false, false, true, true, true, true, true,
                                  false);
                           Chara (false, false, true, true, true, true, true,
                                   false)])
                       (fun x -> return (uncurry (fun a b -> Or (a, b)) x)))
                  scan_6)
                (fun x -> return (sum_join x))))
        (fun x -> return (sum_join x)) l;;

let rec vars_of_exp _A
  = function Const c -> bot_set
    | Var x -> insert _A x bot_set
    | If_then_else (b, e1, e2) ->
        sup_set _A (sup_set _A (vars_of_bexp _A b) (vars_of_exp _A e1))
          (vars_of_exp _A e2)
    | Binop (uu, e1, e2) -> sup_set _A (vars_of_exp _A e1) (vars_of_exp _A e2)
    | Unop (uv, e) -> vars_of_exp _A e
and vars_of_bexp _A
  = function Not e -> vars_of_bexp _A e
    | And (e1, e2) -> sup_set _A (vars_of_bexp _A e1) (vars_of_bexp _A e2)
    | Or (e1, e2) -> sup_set _A (vars_of_bexp _A e1) (vars_of_bexp _A e2)
    | Imply (e1, e2) -> sup_set _A (vars_of_bexp _A e1) (vars_of_bexp _A e2)
    | Eq (i, x) -> sup_set _A (vars_of_exp _A i) (vars_of_exp _A x)
    | Lea (i, x) -> sup_set _A (vars_of_exp _A i) (vars_of_exp _A x)
    | Lta (i, x) -> sup_set _A (vars_of_exp _A i) (vars_of_exp _A x)
    | Ge (i, x) -> sup_set _A (vars_of_exp _A i) (vars_of_exp _A x)
    | Gt (i, x) -> sup_set _A (vars_of_exp _A i) (vars_of_exp _A x)
    | True -> bot_set;;

let rec parse
  parser s =
    (match parse_all lx_ws parser s
      with Inl e ->
        Error [implode
                 (e () [Chara (false, false, false, false, true, false, true,
                                false);
                         Chara (true, false, false, false, false, true, true,
                                 false);
                         Chara (false, true, false, false, true, true, true,
                                 false);
                         Chara (true, true, false, false, true, true, true,
                                 false);
                         Chara (true, false, true, false, false, true, true,
                                 false);
                         Chara (false, true, false, false, true, true, true,
                                 false);
                         Chara (false, true, false, true, true, true, false,
                                 false);
                         Chara (false, false, false, false, false, true, false,
                                 false)])]
      | Inr a -> Result a);;

let rec default_map_of _B a xs = map_default a (map_of _B xs);;

let rec automaton_of _D
  = (fun (committed, (urgent, (trans, inv))) ->
      (Set committed, (Set urgent, (Set trans, default_map_of _D [] inv))));;

let rec bvali (_A1, _A2)
  s x1 = match s, x1 with s, True -> true
    | s, Not e -> not (bvali (_A1, _A2) s e)
    | s, And (e1, e2) -> bvali (_A1, _A2) s e1 && bvali (_A1, _A2) s e2
    | s, Or (e1, e2) -> bvali (_A1, _A2) s e1 || bvali (_A1, _A2) s e2
    | s, Imply (e1, e2) ->
        (if bvali (_A1, _A2) s e1 then bvali (_A1, _A2) s e2 else true)
    | s, Eq (i, x) -> eq _A1 (evali (_A1, _A2) s i) (evali (_A1, _A2) s x)
    | s, Lea (i, x) ->
        less_eq _A2.order_linorder.preorder_order.ord_preorder
          (evali (_A1, _A2) s i) (evali (_A1, _A2) s x)
    | s, Lta (i, x) ->
        less _A2.order_linorder.preorder_order.ord_preorder
          (evali (_A1, _A2) s i) (evali (_A1, _A2) s x)
    | s, Ge (i, x) ->
        less_eq _A2.order_linorder.preorder_order.ord_preorder
          (evali (_A1, _A2) s x) (evali (_A1, _A2) s i)
    | s, Gt (i, x) ->
        less _A2.order_linorder.preorder_order.ord_preorder
          (evali (_A1, _A2) s x) (evali (_A1, _A2) s i)
and evali (_A1, _A2)
  s x1 = match s, x1 with s, Const c -> c
    | s, Var x -> nth s x
    | s, If_then_else (b, e1, e2) ->
        (if bvali (_A1, _A2) s b then evali (_A1, _A2) s e1
          else evali (_A1, _A2) s e2)
    | s, Binop (f, e1, e2) -> f (evali (_A1, _A2) s e1) (evali (_A1, _A2) s e2)
    | s, Unop (f, e) -> f (evali (_A1, _A2) s e);;

let rec map_sexp
  uu uv uw x3 = match uu, uv, uw, x3 with uu, uv, uw, Truea -> Truea
    | f, g, h, Nota e -> Nota (map_sexp f g h e)
    | f, g, h, Anda (e1, e2) -> Anda (map_sexp f g h e1, map_sexp f g h e2)
    | f, g, h, Ora (e1, e2) -> Ora (map_sexp f g h e1, map_sexp f g h e2)
    | f, g, h, Implya (e1, e2) -> Implya (map_sexp f g h e1, map_sexp f g h e2)
    | f, g, h, Eqa (i, x) -> Eqa (g i, h x)
    | f, g, h, Ltb (i, x) -> Ltb (g i, h x)
    | f, g, h, Leb (i, x) -> Leb (g i, h x)
    | f, g, h, Gea (i, x) -> Gea (g i, h x)
    | f, g, h, Gta (i, x) -> Gta (g i, h x)
    | f, g, h, Loc (i, x) -> Loc (i, f i x);;

let rec check_diag_impl (_A1, _A2)
  n = (fun xi ->
        imp_for zero_nata (suc n) (fun sigma -> (fun () -> (not sigma)))
          (fun xc sigma ->
            (fun f_ () -> f_ ((mtx_get (heap_DBMEntry _A2) (suc n) xi (xc, xc))
              ()) ())
              (fun x ->
                (fun () ->
                  (less_DBMEntry
                     _A1.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.linordered_ab_semigroup_add_linordered_ab_monoid_add.linorder_linordered_ab_semigroup_add
                     x (Le (zero _A1.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.ordered_comm_monoid_add_linordered_ab_monoid_add.comm_monoid_add_ordered_comm_monoid_add.monoid_add_comm_monoid_add.zero_monoid_add)) ||
                    sigma))))
          false);;

let rec of_nat
  json =
    (match json with Object _ -> Error ["of_nat: expected natural number"]
      | Arrayb _ -> Error ["of_nat: expected natural number"]
      | Stringa _ -> Error ["of_nat: expected natural number"]
      | Int _ -> Error ["of_nat: expected natural number"] | Nata a -> Result a
      | Rat _ -> Error ["of_nat: expected natural number"]
      | Boolean _ -> Error ["of_nat: expected natural number"]
      | Null -> Error ["of_nat: expected natural number"]);;

let rec find_remove
  p = comp (map_option (fun (xs, (x, ys)) -> (x, xs @ ys))) (extract p);;

let rec merge_pairs _A
  x0 ys = match x0, ys with [], ys -> ys
    | (k, v) :: xs, ys ->
        (match find_remove (fun (ka, _) -> eq _A ka k) ys
          with None -> (k, v) :: merge_pairs _A xs ys
          | Some ((_, va), ysa) -> (k, v @ va) :: merge_pairs _A xs ysa);;

let rec conv_urge _C _J
  urge =
    (fun (committed, (urgent, (trans, inv))) ->
      (committed,
        ([], (map (fun (l, a) ->
                    (let (b, aa) = a in
                     let (g, ab) = aa in
                     let (ac, (f, (r, la))) = ab in
                      (l, (b, (g, (ac, (f, (urge :: r, la))))))))
                trans,
               merge_pairs _C (map (fun l -> (l, [LE (urge, zero _J)])) urgent)
                 inv))));;

let rec dbm_subset_impl (_A1, _A2, _A3)
  n = (fun ai bi ->
        imp_for zero_nata (suc n) (fun a -> (fun () -> a))
          (fun xb _ ->
            imp_for zero_nata (suc n) (fun a -> (fun () -> a))
              (fun xe _ ->
                (fun f_ () -> f_
                  ((mtx_get (heap_DBMEntry _A3) (suc n) ai (xb, xe)) ()) ())
                  (fun x_f ->
                    (fun f_ () -> f_
                      ((mtx_get (heap_DBMEntry _A3) (suc n) bi (xb, xe)) ()) ())
                      (fun x_g ->
                        (fun () ->
                          (less_eq_DBMEntry
                            (_A2, _A1.linordered_ab_monoid_add_linordered_cancel_ab_monoid_add.linordered_ab_semigroup_add_linordered_ab_monoid_add.linorder_linordered_ab_semigroup_add)
                            x_f x_g)))))
              true)
          true);;

let rec map_sexpa
  f1 f2 f3 f4 x4 = match f1, f2, f3, f4, x4 with f1, f2, f3, f4, Truea -> Truea
    | f1, f2, f3, f4, Nota x2 -> Nota (map_sexpa f1 f2 f3 f4 x2)
    | f1, f2, f3, f4, Anda (x31, x32) ->
        Anda (map_sexpa f1 f2 f3 f4 x31, map_sexpa f1 f2 f3 f4 x32)
    | f1, f2, f3, f4, Ora (x41, x42) ->
        Ora (map_sexpa f1 f2 f3 f4 x41, map_sexpa f1 f2 f3 f4 x42)
    | f1, f2, f3, f4, Implya (x51, x52) ->
        Implya (map_sexpa f1 f2 f3 f4 x51, map_sexpa f1 f2 f3 f4 x52)
    | f1, f2, f3, f4, Eqa (x61, x62) -> Eqa (f3 x61, f4 x62)
    | f1, f2, f3, f4, Leb (x71, x72) -> Leb (f3 x71, f4 x72)
    | f1, f2, f3, f4, Ltb (x81, x82) -> Ltb (f3 x81, f4 x82)
    | f1, f2, f3, f4, Gea (x91, x92) -> Gea (f3 x91, f4 x92)
    | f1, f2, f3, f4, Gta (x101, x102) -> Gta (f3 x101, f4 x102)
    | f1, f2, f3, f4, Loc (x111, x112) -> Loc (f1 x111, f2 x112);;

let rec map_formulaa
  f1 f2 f3 f4 x4 = match f1, f2, f3, f4, x4 with
    f1, f2, f3, f4, EX x1 -> EX (map_sexpa f1 f2 f3 f4 x1)
    | f1, f2, f3, f4, EG x2 -> EG (map_sexpa f1 f2 f3 f4 x2)
    | f1, f2, f3, f4, AX x3 -> AX (map_sexpa f1 f2 f3 f4 x3)
    | f1, f2, f3, f4, AG x4 -> AG (map_sexpa f1 f2 f3 f4 x4)
    | f1, f2, f3, f4, Leadsto (x51, x52) ->
        Leadsto (map_sexpa f1 f2 f3 f4 x51, map_sexpa f1 f2 f3 f4 x52);;

let rec rename_locs_sexp
  f x1 = match f, x1 with
    f, Nota a -> binda (rename_locs_sexp f a) (fun aa -> Result (Nota aa))
    | f, Implya (a, b) ->
        binda (rename_locs_sexp f a)
          (fun aa ->
            binda (rename_locs_sexp f b) (fun ba -> Result (Implya (aa, ba))))
    | f, Ora (a, b) ->
        binda (rename_locs_sexp f a)
          (fun aa ->
            binda (rename_locs_sexp f b) (fun ba -> Result (Ora (aa, ba))))
    | f, Anda (a, b) ->
        binda (rename_locs_sexp f a)
          (fun aa ->
            binda (rename_locs_sexp f b) (fun ba -> Result (Anda (aa, ba))))
    | f, Loc (n, x) -> binda (f n x) (fun xa -> Result (Loc (n, xa)))
    | f, Eqa (a, b) -> Result (Eqa (a, b))
    | f, Ltb (a, b) -> Result (Ltb (a, b))
    | f, Leb (a, b) -> Result (Leb (a, b))
    | f, Gea (a, b) -> Result (Gea (a, b))
    | f, Gta (a, b) -> Result (Gta (a, b));;

let rec rename_locs_formula
  f x1 = match f, x1 with
    f, EX phi ->
      binda (rename_locs_sexp f phi) (comp (fun a -> Result a) (fun a -> EX a))
    | f, EG phi ->
        binda (rename_locs_sexp f phi)
          (comp (fun a -> Result a) (fun a -> EG a))
    | f, AX phi ->
        binda (rename_locs_sexp f phi)
          (comp (fun a -> Result a) (fun a -> AX a))
    | f, AG phi ->
        binda (rename_locs_sexp f phi)
          (comp (fun a -> Result a) (fun a -> AG a))
    | f, Leadsto (phi, psi) ->
        binda (rename_locs_sexp f phi)
          (fun phia ->
            binda (rename_locs_sexp f psi)
              (fun psia -> Result (Leadsto (phia, psia))));;

let rec locs_of_sexp _A
  = function Nota e -> locs_of_sexp _A e
    | Anda (e1, e2) -> sup_set _A (locs_of_sexp _A e1) (locs_of_sexp _A e2)
    | Ora (e1, e2) -> sup_set _A (locs_of_sexp _A e1) (locs_of_sexp _A e2)
    | Implya (e1, e2) -> sup_set _A (locs_of_sexp _A e1) (locs_of_sexp _A e2)
    | Loc (i, x) -> insert _A i bot_set
    | Truea -> bot_set
    | Eqa (v, va) -> bot_set
    | Leb (v, va) -> bot_set
    | Ltb (v, va) -> bot_set
    | Gea (v, va) -> bot_set
    | Gta (v, va) -> bot_set;;

let rec locs_of_formula _A
  = function EX phi -> locs_of_sexp _A phi
    | EG phi -> locs_of_sexp _A phi
    | AX phi -> locs_of_sexp _A phi
    | AG phi -> locs_of_sexp _A phi
    | Leadsto (phi, psi) ->
        sup_set _A (locs_of_sexp _A phi) (locs_of_sexp _A psi);;

let rec sexp_to_acconstraint = function Ltb (a, b) -> LT (a, b)
                               | Leb (a, b) -> LE (a, b)
                               | Eqa (a, b) -> EQ (a, b)
                               | Gea (a, b) -> GE (a, b)
                               | Gta (a, b) -> GT (a, b);;

let rec sexp_to_bexp
  = function Ltb (a, b) -> Result (Lta (Var a, Const b))
    | Leb (a, b) -> Result (Lea (Var a, Const b))
    | Eqa (a, b) -> Result (Eq (Var a, Const b))
    | Gea (a, b) -> Result (Ge (Var a, Const b))
    | Gta (a, b) -> Result (Gt (Var a, Const b))
    | Anda (a, b) ->
        binda (sexp_to_bexp a)
          (fun aa -> binda (sexp_to_bexp b) (fun ba -> Result (And (aa, ba))))
    | Ora (a, b) ->
        binda (sexp_to_bexp a)
          (fun aa -> binda (sexp_to_bexp b) (fun ba -> Result (Or (aa, ba))))
    | Implya (a, b) ->
        binda (sexp_to_bexp a)
          (fun aa -> binda (sexp_to_bexp b) (fun ba -> Result (Imply (aa, ba))))
    | Truea -> Error ["Illegal construct in binary operation"]
    | Nota v -> Error ["Illegal construct in binary operation"]
    | Loc (v, va) -> Error ["Illegal construct in binary operation"];;

let rec chop_sexp _A
  clocks x1 x2 = match clocks, x1, x2 with
    clocks, Anda (a, b), (cs, es) ->
      chop_sexp _A clocks b (chop_sexp _A clocks a (cs, es))
    | clocks, Eqa (a, b), (cs, es) ->
        (if membera _A clocks a then (Eqa (a, b) :: cs, es)
          else (cs, Eqa (a, b) :: es))
    | clocks, Leb (a, b), (cs, es) ->
        (if membera _A clocks a then (Leb (a, b) :: cs, es)
          else (cs, Leb (a, b) :: es))
    | clocks, Ltb (a, b), (cs, es) ->
        (if membera _A clocks a then (Ltb (a, b) :: cs, es)
          else (cs, Ltb (a, b) :: es))
    | clocks, Gea (a, b), (cs, es) ->
        (if membera _A clocks a then (Gea (a, b) :: cs, es)
          else (cs, Gea (a, b) :: es))
    | clocks, Gta (a, b), (cs, es) ->
        (if membera _A clocks a then (Gta (a, b) :: cs, es)
          else (cs, Gta (a, b) :: es))
    | clocks, Truea, (cs, es) -> (cs, Truea :: es)
    | clocks, Nota v, (cs, es) -> (cs, Nota v :: es)
    | clocks, Ora (v, va), (cs, es) -> (cs, Ora (v, va) :: es)
    | clocks, Implya (v, va), (cs, es) -> (cs, Implya (v, va) :: es)
    | clocks, Loc (v, va), (cs, es) -> (cs, Loc (v, va) :: es);;

let rec compile_invariant
  clocks vars inv =
    (let (cs, es) = chop_sexp equal_literal clocks inv ([], []) in
     let g = map sexp_to_acconstraint cs in
      (if null es then Result (g, True)
        else (let e = fold (fun a b -> Anda (a, b)) (tl es) (hd es) in
               binda (sexp_to_bexp e)
                 (fun b ->
                   binda (asserta
                           (subset (card_UNIV_literal, equal_literal)
                             (set_bexp equal_literal b) (Set vars))
                           (implode
                             ([Chara (true, false, true, false, true, false,
                                       true, false);
                                Chara (false, true, true, true, false, true,
true, false);
                                Chara (true, true, false, true, false, true,
true, false);
                                Chara (false, true, true, true, false, true,
true, false);
                                Chara (true, true, true, true, false, true,
true, false);
                                Chara (true, true, true, false, true, true,
true, false);
                                Chara (false, true, true, true, false, true,
true, false);
                                Chara (false, false, false, false, false, true,
false, false);
                                Chara (false, true, true, false, true, true,
true, false);
                                Chara (true, false, false, false, false, true,
true, false);
                                Chara (false, true, false, false, true, true,
true, false);
                                Chara (true, false, false, true, false, true,
true, false);
                                Chara (true, false, false, false, false, true,
true, false);
                                Chara (false, true, false, false, false, true,
true, false);
                                Chara (false, false, true, true, false, true,
true, false);
                                Chara (true, false, true, false, false, true,
true, false);
                                Chara (false, false, false, false, false, true,
false, false);
                                Chara (true, false, false, true, false, true,
true, false);
                                Chara (false, true, true, true, false, true,
true, false);
                                Chara (false, false, false, false, false, true,
false, false);
                                Chara (false, true, false, false, false, true,
true, false);
                                Chara (true, false, true, false, false, true,
true, false);
                                Chara (false, false, false, true, true, true,
true, false);
                                Chara (false, false, false, false, true, true,
true, false);
                                Chara (false, true, false, true, true, true,
false, false);
                                Chara (false, false, false, false, false, true,
false, false)] @
                               shows_prec_bexp show_literal show_int zero_nata b
                                 [])))
                     (fun _ -> Result (g, b))))));;

let rec scan_acconstraint
  x = bindb (alt (bindb (gen_token lx_ws scan_var)
                   (fun xa ->
                     bindb (gen_token lx_ws
                             (exactly (equal_char, show_char)
                               [Chara (false, false, true, true, true, true,
false, false)]))
                       (fun _ ->
                         bindb (gen_token lx_ws lx_int)
                           (fun xaa -> return (Ltb (implode xa, xaa))))))
              (bindb
                (alt (bindb (gen_token lx_ws scan_var)
                       (fun xa ->
                         bindb (gen_token lx_ws
                                 (exactly (equal_char, show_char)
                                   [Chara (false, false, true, true, true, true,
    false, false);
                                     Chara (true, false, true, true, true, true,
     false, false)]))
                           (fun _ ->
                             bindb (gen_token lx_ws lx_int)
                               (fun xaa -> return (Leb (implode xa, xaa))))))
                  (bindb
                    (alt (bindb (gen_token lx_ws scan_var)
                           (fun xa ->
                             bindb (gen_token lx_ws
                                     (exactly (equal_char, show_char)
                                       [Chara
  (true, false, true, true, true, true, false, false);
 Chara (true, false, true, true, true, true, false, false)]))
                               (fun _ ->
                                 bindb (gen_token lx_ws lx_int)
                                   (fun xaa ->
                                     return (Eqa (implode xa, xaa))))))
                      (bindb
                        (alt (bindb (gen_token lx_ws scan_var)
                               (fun xa ->
                                 bindb (gen_token lx_ws
 (exactly (equal_char, show_char)
   [Chara (true, false, true, true, true, true, false, false)]))
                                   (fun _ ->
                                     bindb (gen_token lx_ws lx_int)
                                       (fun xaa ->
 return (Eqa (implode xa, xaa))))))
                          (bindb
                            (alt (bindb (gen_token lx_ws scan_var)
                                   (fun xa ->
                                     bindb (gen_token lx_ws
     (exactly (equal_char, show_char)
       [Chara (false, true, true, true, true, true, false, false);
         Chara (true, false, true, true, true, true, false, false)]))
                                       (fun _ ->
 bindb (gen_token lx_ws lx_int) (fun xaa -> return (Gea (implode xa, xaa))))))
                              (bindb (gen_token lx_ws scan_var)
                                (fun xa ->
                                  bindb (gen_token lx_ws
  (exactly (equal_char, show_char)
    [Chara (false, true, true, true, true, true, false, false)]))
                                    (fun _ ->
                                      bindb (gen_token lx_ws lx_int)
(fun xaa -> return (Gta (implode xa, xaa)))))))
                            (fun xa -> return (sum_join xa))))
                        (fun xa -> return (sum_join xa))))
                    (fun xa -> return (sum_join xa))))
                (fun xa -> return (sum_join xa))))
        (fun xa -> return (sum_join xa)) x;;

let rec scan_loc
  x = bindb (gen_token lx_ws scan_var)
        (fun xa ->
          bindb (exactly (equal_char, show_char)
                  [Chara (false, true, true, true, false, true, false, false)])
            (fun _ ->
              bindb scan_var
                (fun xaa -> return (Loc (implode xa, implode xaa)))))
        x;;

let rec scan_bexp_elem
  x = bindb (alt scan_acconstraint scan_loc) (fun xa -> return (sum_join xa))
        x;;

let rec scan_7a
  elem imply ora anda nota l =
    bindb (alt (bindb
                 (scan_infix_pair (scan_6a elem imply ora anda nota)
                   (scan_7a elem imply ora anda nota)
                   [Chara (true, false, true, true, false, true, false, false);
                     Chara (false, true, true, true, true, true, false, false)])
                 (fun x -> return (uncurry imply x)))
            (bindb
              (alt (bindb
                     (scan_infix_pair (scan_6a elem imply ora anda nota)
                       (scan_7a elem imply ora anda nota)
                       [Chara (false, false, true, true, true, true, true,
                                false);
                         Chara (false, false, true, true, true, true, true,
                                 false)])
                     (fun x -> return (uncurry ora x)))
                (scan_6a elem imply ora anda nota))
              (fun x -> return (sum_join x))))
      (fun x -> return (sum_join x)) l
and scan_0a
  elem imply ora anda nota l =
    bindb (alt (bindb
                 (gen_token lx_ws
                   (bindb
                     (alt (exactly (equal_char, show_char)
                            [Chara (false, true, true, true, true, true, true,
                                     false)])
                       (exactly (equal_char, show_char)
                         [Chara (true, false, false, false, false, true, false,
                                  false)]))
                     (fun x -> return (sum_join x))))
                 (fun _ ->
                   bindb (scan_parens
                           [Chara (false, false, false, true, false, true,
                                    false, false)]
                           [Chara (true, false, false, true, false, true, false,
                                    false)]
                           (scan_7a elem imply ora anda nota))
                     (fun x -> return (nota x))))
            (bindb
              (alt elem
                (scan_parens
                  [Chara (false, false, false, true, false, true, false, false)]
                  [Chara (true, false, false, true, false, true, false, false)]
                  (scan_7a elem imply ora anda nota)))
              (fun x -> return (sum_join x))))
      (fun x -> return (sum_join x)) l
and scan_6a
  elem imply ora anda nota l =
    bindb (alt (bindb
                 (scan_infix_pair (scan_0a elem imply ora anda nota)
                   (scan_6a elem imply ora anda nota)
                   [Chara (false, true, true, false, false, true, false, false);
                     Chara (false, true, true, false, false, true, false,
                             false)])
                 (fun x -> return (uncurry anda x)))
            (scan_0a elem imply ora anda nota))
      (fun x -> return (sum_join x)) l;;

let rec compile_invarianta
  clocks vars inv =
    (if ((inv : string) = "") then Result ([], True)
      else binda (err_msg ("Failed to parse guard in " ^ inv)
                   (parse
                     (scan_7a scan_bexp_elem (fun a b -> Implya (a, b))
                       (fun a b -> Ora (a, b)) (fun a b -> Anda (a, b))
                       (fun a -> Nota a))
                     inv))
             (compile_invariant clocks vars));;

let rec of_string
  json =
    (match json with Object _ -> Error ["of_array: expected sequence"]
      | Arrayb _ -> Error ["of_array: expected sequence"]
      | Stringa s -> Result (implode s)
      | Int _ -> Error ["of_array: expected sequence"]
      | Nata _ -> Error ["of_array: expected sequence"]
      | Rat _ -> Error ["of_array: expected sequence"]
      | Boolean _ -> Error ["of_array: expected sequence"]
      | Null -> Error ["of_array: expected sequence"]);;

let rec of_object
  json =
    (match json with Object asa -> Result (map_of (equal_list equal_char) asa)
      | Arrayb _ -> Error ["json_to_map: expected object"]
      | Stringa _ -> Error ["json_to_map: expected object"]
      | Int _ -> Error ["json_to_map: expected object"]
      | Nata _ -> Error ["json_to_map: expected object"]
      | Rat _ -> Error ["json_to_map: expected object"]
      | Boolean _ -> Error ["json_to_map: expected object"]
      | Null -> Error ["json_to_map: expected object"]);;

let rec convert_node
  clocks vars n =
    binda (of_object n)
      (fun na ->
        binda (binda
                (geta (show_list show_char) na
                  [Chara (true, false, false, true, false, true, true, false);
                    Chara (false, false, true, false, false, true, true,
                            false)])
                of_nat)
          (fun id ->
            binda (binda
                    (geta (show_list show_char) na
                      [Chara (false, true, true, true, false, true, true,
                               false);
                        Chara (true, false, false, false, false, true, true,
                                false);
                        Chara (true, false, true, true, false, true, true,
                                false);
                        Chara (true, false, true, false, false, true, true,
                                false)])
                    of_string)
              (fun name ->
                binda (binda
                        (geta (show_list show_char) na
                          [Chara (true, false, false, true, false, true, true,
                                   false);
                            Chara (false, true, true, true, false, true, true,
                                    false);
                            Chara (false, true, true, false, true, true, true,
                                    false);
                            Chara (true, false, false, false, false, true, true,
                                    false);
                            Chara (false, true, false, false, true, true, true,
                                    false);
                            Chara (true, false, false, true, false, true, true,
                                    false);
                            Chara (true, false, false, false, false, true, true,
                                    false);
                            Chara (false, true, true, true, false, true, true,
                                    false);
                            Chara (false, false, true, false, true, true, true,
                                    false)])
                        of_string)
                  (fun inv ->
                    binda (err_msg "Failed to parse invariant!"
                            (compile_invarianta clocks vars inv))
                      (fun (inva, inv_vars) ->
                        binda (asserta
                                (match inv_vars with True -> true
                                  | Not _ -> false | And (_, _) -> false
                                  | Or (_, _) -> false | Imply (_, _) -> false
                                  | Eq (_, _) -> false | Lea (_, _) -> false
                                  | Lta (_, _) -> false | Ge (_, _) -> false
                                  | Gt (_, _) -> false)
                                "State invariants on nodes are not supported")
                          (fun _ -> Result ((name, id), inva)))))));;

let rec scan_update
  x = bindb (gen_token lx_ws scan_var)
        (fun xa ->
          bindb (gen_token lx_ws
                  (bindb
                    (alt (exactly (equal_char, show_char)
                           [Chara (true, false, true, true, true, true, false,
                                    false)])
                      (exactly (equal_char, show_char)
                        [Chara (false, true, false, true, true, true, false,
                                 false);
                          Chara (true, false, true, true, true, true, false,
                                  false)]))
                    (fun xb -> return (sum_join xb))))
            (fun _ -> bindb scan_exp (fun xaa -> return (implode xa, xaa))))
        x;;

let rec scan_action
  x = bindb (alt (bindb scan_var
                   (fun xa ->
                     bindb (gen_token lx_ws
                             (exactly (equal_char, show_char)
                               [Chara (true, true, true, true, true, true,
false, false)]))
                       (fun _ -> return (comp (fun a -> In a) implode xa))))
              (bindb
                (alt (bindb scan_var
                       (fun xa ->
                         bindb (gen_token lx_ws
                                 (exactly (equal_char, show_char)
                                   [Chara (true, false, false, false, false,
    true, false, false)]))
                           (fun _ ->
                             return (comp (fun a -> Out a) implode xa))))
                  (bindb scan_var
                    (fun xa -> return (comp (fun a -> Sil a) implode xa))))
                (fun xa -> return (sum_join xa))))
        (fun xa -> return (sum_join xa)) x;;

let rec convert_edge
  clocks vars e =
    binda (of_object e)
      (fun ea ->
        binda (binda
                (geta (show_list show_char) ea
                  [Chara (true, true, false, false, true, true, true, false);
                    Chara (true, true, true, true, false, true, true, false);
                    Chara (true, false, true, false, true, true, true, false);
                    Chara (false, true, false, false, true, true, true, false);
                    Chara (true, true, false, false, false, true, true, false);
                    Chara (true, false, true, false, false, true, true, false)])
                of_nat)
          (fun source ->
            binda (binda
                    (geta (show_list show_char) ea
                      [Chara (false, false, true, false, true, true, true,
                               false);
                        Chara (true, false, false, false, false, true, true,
                                false);
                        Chara (false, true, false, false, true, true, true,
                                false);
                        Chara (true, true, true, false, false, true, true,
                                false);
                        Chara (true, false, true, false, false, true, true,
                                false);
                        Chara (false, false, true, false, true, true, true,
                                false)])
                    of_nat)
              (fun target ->
                binda (binda
                        (geta (show_list show_char) ea
                          [Chara (true, true, true, false, false, true, true,
                                   false);
                            Chara (true, false, true, false, true, true, true,
                                    false);
                            Chara (true, false, false, false, false, true, true,
                                    false);
                            Chara (false, true, false, false, true, true, true,
                                    false);
                            Chara (false, false, true, false, false, true, true,
                                    false)])
                        of_string)
                  (fun guard ->
                    binda (binda
                            (geta (show_list show_char) ea
                              [Chara (false, false, true, true, false, true,
                                       true, false);
                                Chara (true, false, false, false, false, true,
true, false);
                                Chara (false, true, false, false, false, true,
true, false);
                                Chara (true, false, true, false, false, true,
true, false);
                                Chara (false, false, true, true, false, true,
true, false)])
                            of_string)
                      (fun label ->
                        binda (binda
                                (geta (show_list show_char) ea
                                  [Chara (true, false, true, false, true, true,
   true, false);
                                    Chara (false, false, false, false, true,
    true, true, false);
                                    Chara (false, false, true, false, false,
    true, true, false);
                                    Chara (true, false, false, false, false,
    true, true, false);
                                    Chara (false, false, true, false, true,
    true, true, false);
                                    Chara (true, false, true, false, false,
    true, true, false)])
                                of_string)
                          (fun update ->
                            binda (if ((label : string) = "")
                                    then Result (Sil "")
                                    else err_msg
   ("Failed to parse label in " ^ label) (parse scan_action label))
                              (fun labela ->
                                binda (err_msg "Failed to parse guard!"
(compile_invarianta clocks vars guard))
                                  (fun (g, check) ->
                                    binda (if ((update : string) = "")
    then Result []
    else err_msg ("Failed to parse update in " ^ update)
           (parse (parse_list scan_update) update))
                                      (fun upd ->
(let resets = filter (fun x -> membera equal_literal clocks (fst x)) upd in
  binda (asserta (list_all (fun (_, Const x) -> equal_inta x zero_inta) resets)
          "Clock resets to values different from zero are not supported")
    (fun _ ->
      (let resetsa = map fst resets in
       let upds =
         filter (fun x -> not (membera equal_literal clocks (fst x))) upd in
        binda (asserta
                (list_all (fun (x, _) -> membera equal_literal vars x) upds)
                ("Unknown variable in update: " ^ update))
          (fun _ ->
            Result
              (source,
                (check,
                  (g, (labela, (upds, (resetsa, target)))))))))))))))))));;

let rec of_array
  json =
    (match json with Object _ -> Error ["of_array: expected sequence"]
      | Arrayb a -> Result a
      | Stringa _ -> Error ["of_array: expected sequence"]
      | Int _ -> Error ["of_array: expected sequence"]
      | Nata _ -> Error ["of_array: expected sequence"]
      | Rat _ -> Error ["of_array: expected sequence"]
      | Boolean _ -> Error ["of_array: expected sequence"]
      | Null -> Error ["of_array: expected sequence"]);;

let rec default def x = (match x with Result s -> s | Error _ -> def);;

let rec convert_automaton
  clocks vars a =
    binda (binda
            (geta (show_list show_char) a
              [Chara (false, true, true, true, false, true, true, false);
                Chara (true, true, true, true, false, true, true, false);
                Chara (false, false, true, false, false, true, true, false);
                Chara (true, false, true, false, false, true, true, false);
                Chara (true, true, false, false, true, true, true, false)])
            of_array)
      (fun nodes ->
        binda (binda
                (geta (show_list show_char) a
                  [Chara (true, false, true, false, false, true, true, false);
                    Chara (false, false, true, false, false, true, true, false);
                    Chara (true, true, true, false, false, true, true, false);
                    Chara (true, false, true, false, false, true, true, false);
                    Chara (true, true, false, false, true, true, true, false)])
                of_array)
          (fun edges ->
            binda (combine_map (convert_node clocks vars) nodes)
              (fun nodesa ->
                (let invs =
                   map (fun (aa, b) ->
                         (let (_, ab) = aa in (fun ba -> (ab, ba))) b)
                     nodesa
                   in
                 let names_to_ids = map fst nodesa in
                  binda (asserta
                          (distinct equal_literal
                            (filter (fun s -> not ((s : string) = ""))
                              (map fst names_to_ids)))
                          ("Node names are ambiguous" ^
                            implode
                              (shows_prec_list show_literal zero_nata
                                (map fst names_to_ids) [])))
                    (fun _ ->
                      binda (asserta (distinct equal_nat (map snd names_to_ids))
                              "Duplicate node id")
                        (fun _ ->
                          (let ids_to_names =
                             map_of equal_nat (map swap names_to_ids) in
                           let names_to_idsa = map_of equal_literal names_to_ids
                             in
                           let committed =
                             default []
                               (binda
                                 (geta (show_list show_char) a
                                   [Chara (true, true, false, false, false,
    true, true, false);
                                     Chara (true, true, true, true, false, true,
     true, false);
                                     Chara (true, false, true, true, false,
     true, true, false);
                                     Chara (true, false, true, true, false,
     true, true, false);
                                     Chara (true, false, false, true, false,
     true, true, false);
                                     Chara (false, false, true, false, true,
     true, true, false);
                                     Chara (false, false, true, false, true,
     true, true, false);
                                     Chara (true, false, true, false, false,
     true, true, false);
                                     Chara (false, false, true, false, false,
     true, true, false)])
                                 of_array)
                             in
                            binda (combine_map of_nat committed)
                              (fun committeda ->
                                (let urgent =
                                   default []
                                     (binda
                                       (geta (show_list show_char) a
 [Chara (true, false, true, false, true, true, true, false);
   Chara (false, true, false, false, true, true, true, false);
   Chara (true, true, true, false, false, true, true, false);
   Chara (true, false, true, false, false, true, true, false);
   Chara (false, true, true, true, false, true, true, false);
   Chara (false, false, true, false, true, true, true, false)])
                                       of_array)
                                   in
                                  binda (combine_map of_nat urgent)
                                    (fun urgenta ->
                                      binda
(combine_map (convert_edge clocks vars) edges)
(fun edgesa ->
  Result
    (names_to_idsa,
      (ids_to_names, (committeda, (urgenta, (edgesa, invs))))))))))))))));;

let rec trace_level i f = ();;

let rec scan_prefix
  p head =
    bindb (gen_token lx_ws (exactly (equal_char, show_char) head))
      (fun _ -> p);;

let rec scan_formula
  x = bindb (alt (bindb
                   (scan_prefix
                     (scan_7a scan_bexp_elem (fun a b -> Implya (a, b))
                       (fun a b -> Ora (a, b)) (fun a b -> Anda (a, b))
                       (fun a -> Nota a))
                     [Chara (true, false, true, false, false, false, true,
                              false);
                       Chara (false, false, true, true, true, true, false,
                               false);
                       Chara (false, true, true, true, true, true, false,
                               false)])
                   (fun xa -> return (EX xa)))
              (bindb
                (alt (bindb
                       (scan_prefix
                         (scan_7a scan_bexp_elem (fun a b -> Implya (a, b))
                           (fun a b -> Ora (a, b)) (fun a b -> Anda (a, b))
                           (fun a -> Nota a))
                         [Chara (true, false, true, false, false, false, true,
                                  false);
                           Chara (true, true, false, true, true, false, true,
                                   false);
                           Chara (true, false, true, true, true, false, true,
                                   false)])
                       (fun xa -> return (EG xa)))
                  (bindb
                    (alt (bindb
                           (scan_prefix
                             (scan_7a scan_bexp_elem (fun a b -> Implya (a, b))
                               (fun a b -> Ora (a, b)) (fun a b -> Anda (a, b))
                               (fun a -> Nota a))
                             [Chara (true, false, false, false, false, false,
                                      true, false);
                               Chara (false, false, true, true, true, true,
                                       false, false);
                               Chara (false, true, true, true, true, true,
                                       false, false)])
                           (fun xa -> return (AX xa)))
                      (bindb
                        (alt (bindb
                               (scan_prefix
                                 (scan_7a scan_bexp_elem
                                   (fun a b -> Implya (a, b))
                                   (fun a b -> Ora (a, b))
                                   (fun a b -> Anda (a, b)) (fun a -> Nota a))
                                 [Chara (true, false, false, false, false,
  false, true, false);
                                   Chara (true, true, false, true, true, false,
   true, false);
                                   Chara (true, false, true, true, true, false,
   true, false)])
                               (fun xa -> return (AG xa)))
                          (bindb
                            (scan_infix_pair
                              (scan_7a scan_bexp_elem (fun a b -> Implya (a, b))
                                (fun a b -> Ora (a, b)) (fun a b -> Anda (a, b))
                                (fun a -> Nota a))
                              (scan_7a scan_bexp_elem (fun a b -> Implya (a, b))
                                (fun a b -> Ora (a, b)) (fun a b -> Anda (a, b))
                                (fun a -> Nota a))
                              [Chara (true, false, true, true, false, true,
                                       false, false);
                                Chara (true, false, true, true, false, true,
false, false);
                                Chara (false, true, true, true, true, true,
false, false)])
                            (fun xa ->
                              return (uncurry (fun a b -> Leadsto (a, b)) xa))))
                        (fun xa -> return (sum_join xa))))
                    (fun xa -> return (sum_join xa))))
                (fun xa -> return (sum_join xa))))
        (fun xa -> return (sum_join xa)) x;;

let rec parse_bound
  x = bindb ta_var_ident
        (fun a ->
          bindb (exactly (equal_char, show_char)
                  [Chara (true, true, false, true, true, false, true, false)])
            (fun _ ->
              bindb lx_int
                (fun xa ->
                  bindb (exactly (equal_char, show_char)
                          [Chara (false, true, false, true, true, true, false,
                                   false)])
                    (fun _ ->
                      bindb lx_int
                        (fun xaa ->
                          bindb (exactly (equal_char, show_char)
                                  [Chara (true, false, true, true, true, false,
   true, false)])
                            (fun _ -> return (a, (xa, xaa))))))))
        x;;

let rec parse_bounds
  x = bindb (alt (parse_list
                   (bindb lx_ws
                     (fun _ ->
                       bindb parse_bound
                         (fun xa ->
                           return (let (s, a) = xa in (implode s, a))))))
              (bindb lx_ws (fun _ -> return [])))
        (fun xa -> return (sum_join xa)) x;;

let rec convert
  json =
    binda (of_object json)
      (fun all ->
        binda (geta (show_list show_char) all
                [Chara (true, false, false, false, false, true, true, false);
                  Chara (true, false, true, false, true, true, true, false);
                  Chara (false, false, true, false, true, true, true, false);
                  Chara (true, true, true, true, false, true, true, false);
                  Chara (true, false, true, true, false, true, true, false);
                  Chara (true, false, false, false, false, true, true, false);
                  Chara (false, false, true, false, true, true, true, false);
                  Chara (true, false, false, false, false, true, true, false)])
          (fun automata ->
            binda (of_array automata)
              (fun automataa ->
                (let broadcast =
                   default []
                     (binda
                       (geta (show_list show_char) all
                         [Chara (false, true, false, false, false, true, true,
                                  false);
                           Chara (false, true, false, false, true, true, true,
                                   false);
                           Chara (true, true, true, true, false, true, true,
                                   false);
                           Chara (true, false, false, false, false, true, true,
                                   false);
                           Chara (false, false, true, false, false, true, true,
                                   false);
                           Chara (true, true, false, false, false, true, true,
                                   false);
                           Chara (true, false, false, false, false, true, true,
                                   false);
                           Chara (true, true, false, false, true, true, true,
                                   false);
                           Chara (false, false, true, false, true, true, true,
                                   false)])
                       of_array)
                   in
                  binda (combine_map of_string broadcast)
                    (fun broadcasta ->
                      (let _ =
                         trace_level (Int_of_integer (Z.Int.of_int 3))
                           (fun _ ->
                             (fun () ->
                               ("Broadcast channels " ^
                                 implode
                                   (shows_prec_list show_literal zero_nata
                                     broadcasta []))))
                         in
                       let bounds =
                         default ""
                           (binda
                             (geta (show_list show_char) all
                               [Chara (false, true, true, false, true, true,
true, false);
                                 Chara (true, false, false, false, false, true,
 true, false);
                                 Chara (false, true, false, false, true, true,
 true, false);
                                 Chara (true, true, false, false, true, true,
 true, false)])
                             of_string)
                         in
                        binda (err_msg "Failed to parse bounds"
                                (parse parse_bounds bounds))
                          (fun boundsa ->
                            binda (geta (show_list show_char) all
                                    [Chara (true, true, false, false, false,
     true, true, false);
                                      Chara
(false, false, true, true, false, true, true, false);
                                      Chara
(true, true, true, true, false, true, true, false);
                                      Chara
(true, true, false, false, false, true, true, false);
                                      Chara
(true, true, false, true, false, true, true, false);
                                      Chara
(true, true, false, false, true, true, true, false)])
                              (fun clocks ->
                                binda (of_string clocks)
                                  (fun clocksa ->
                                    binda (err_msg "Failed to parse clocks"
    (parse
      (parse_list
        (bindb lx_ws
          (fun _ -> bindb ta_var_ident (fun x -> return (implode x)))))
      clocksa))
                                      (fun clocksb ->
binda (geta (show_list show_char) all
        [Chara (false, true, true, false, false, true, true, false);
          Chara (true, true, true, true, false, true, true, false);
          Chara (false, true, false, false, true, true, true, false);
          Chara (true, false, true, true, false, true, true, false);
          Chara (true, false, true, false, true, true, true, false);
          Chara (false, false, true, true, false, true, true, false);
          Chara (true, false, false, false, false, true, true, false)])
  (fun formula ->
    binda (of_string formula)
      (fun formulaa ->
        binda (err_msg "Failed to parse formula" (parse scan_formula formulaa))
          (fun formulab ->
            binda (combine_map of_object automataa)
              (fun automatab ->
                binda (combine_map
                        (fun a ->
                          binda (geta (show_list show_char) a
                                  [Chara (false, true, true, true, false, true,
   true, false);
                                    Chara (true, false, false, false, false,
    true, true, false);
                                    Chara (true, false, true, true, false, true,
    true, false);
                                    Chara (true, false, true, false, false,
    true, true, false)])
                            of_string)
                        automatab)
                  (fun process_names ->
                    binda (asserta (distinct equal_literal process_names)
                            "Process names are ambiguous")
                      (fun _ ->
                        binda (asserta
                                (subset (card_UNIV_literal, equal_literal)
                                  (locs_of_formula equal_literal formulab)
                                  (Set process_names))
                                "Unknown process name in formula")
                          (fun _ ->
                            (let process_names_to_index =
                               index equal_literal process_names in
                              binda (combine_map
                                      (fun a ->
binda (geta (show_list show_char) a
        [Chara (true, false, false, true, false, true, true, false);
          Chara (false, true, true, true, false, true, true, false);
          Chara (true, false, false, true, false, true, true, false);
          Chara (false, false, true, false, true, true, true, false);
          Chara (true, false, false, true, false, true, true, false);
          Chara (true, false, false, false, false, true, true, false);
          Chara (false, false, true, true, false, true, true, false)])
  (fun x -> binda (of_nat x) (fun aa -> Result aa)))
                                      automatab)
                                (fun init_locs ->
                                  (let formulac =
                                     map_formulaa process_names_to_index id id
                                       id formulab
                                     in
                                   let vars = map fst boundsa in
                                   let init_vars =
                                     map (fun x -> (x, zero_inta)) vars in
                                    binda (combine_map
    (convert_automaton clocksb vars) automatab)
                                      (fun names_automata ->
(let automatac = map (comp snd snd) names_automata in
 let names = map fst names_automata in
 let ids_to_names = map (comp fst snd) names_automata in
 let ids_to_namesa =
   (fun p i ->
     (match nth ids_to_names p i
       with None -> implode (shows_prec_nat zero_nata i []) | Some n -> n))
   in
  binda (rename_locs_formula (fun i -> geta show_literal (nth names i))
          formulac)
    (fun formulad ->
      Result
        (ids_to_namesa,
          (process_names_to_index,
            (broadcasta,
              (automatac,
                (boundsa,
                  (formulad,
                    (init_locs, init_vars))))))))))))))))))))))))))))));;

let rec mk_updsi
  s upds =
    fold (fun (x, upd) sa ->
           list_update sa x (evali (equal_int, linorder_int) s upd))
      upds s;;

let rec map_formula
  f g h x3 = match f, g, h, x3 with f, g, h, EX phi -> EX (map_sexp f g h phi)
    | f, g, h, EG phi -> EG (map_sexp f g h phi)
    | f, g, h, AX phi -> AX (map_sexp f g h phi)
    | f, g, h, AG phi -> AG (map_sexp f g h phi)
    | f, g, h, Leadsto (phi, psi) ->
        Leadsto (map_sexp f g h phi, map_sexp f g h psi);;

let rec map_acconstraint
  f1 f2 x2 = match f1, f2, x2 with f1, f2, LT (x11, x12) -> LT (f1 x11, f2 x12)
    | f1, f2, LE (x21, x22) -> LE (f1 x21, f2 x22)
    | f1, f2, EQ (x31, x32) -> EQ (f1 x31, f2 x32)
    | f1, f2, GT (x41, x42) -> GT (f1 x41, f2 x42)
    | f1, f2, GE (x51, x52) -> GE (f1 x51, f2 x52);;

let rec mem_assoc _A x = list_ex (fun (y, _) -> eq _A x y);;

let rec n_vs bounds = size_list bounds;;

let rec vars_of_sexp _C
  = function Nota e -> vars_of_sexp _C e
    | Anda (e1, e2) -> sup_set _C (vars_of_sexp _C e1) (vars_of_sexp _C e2)
    | Ora (e1, e2) -> sup_set _C (vars_of_sexp _C e1) (vars_of_sexp _C e2)
    | Implya (e1, e2) -> sup_set _C (vars_of_sexp _C e1) (vars_of_sexp _C e2)
    | Eqa (i, x) -> insert _C i bot_set
    | Ltb (i, x) -> insert _C i bot_set
    | Leb (i, x) -> insert _C i bot_set
    | Gea (i, x) -> insert _C i bot_set
    | Gta (i, x) -> insert _C i bot_set
    | Truea -> bot_set
    | Loc (v, va) -> bot_set;;

let rec vars_of_formula _C
  = function EX phi -> vars_of_sexp _C phi
    | EG phi -> vars_of_sexp _C phi
    | AX phi -> vars_of_sexp _C phi
    | AG phi -> vars_of_sexp _C phi
    | Leadsto (phi, psi) ->
        sup_set _C (vars_of_sexp _C phi) (vars_of_sexp _C psi);;

let rec simple_Network_Impl_nat_ceiling_start_state_axioms
  broadcast bounds automata m num_states k l_0 s_0 formula =
    all_interval_nat
      (fun i ->
        list_all
          (fun (l, g) ->
            ball (collect_clock_pairs g)
              (fun (x, ma) ->
                less_eq_int ma (int_of_nat (nth (nth (nth k i) l) x))))
          (comp (comp snd snd) snd (nth automata i)))
      zero_nata (size_list automata) &&
      (all_interval_nat
         (fun i ->
           list_all
             (fun (l, (_, (g, _))) ->
               ball (collect_clock_pairs g)
                 (fun (x, ma) ->
                   less_eq_int ma (int_of_nat (nth (nth (nth k i) l) x))))
             (comp (comp fst snd) snd (nth automata i)))
         zero_nata (size_list automata) &&
        all_interval_nat
          (fun i ->
            list_all
              (fun (l, (_, (_, (_, (_, (r, la)))))) ->
                ball (minus_set equal_nat
                       (Set (upt zero_nata (plus_nata m one_nata))) (Set r))
                  (fun c ->
                    less_eq_nat (nth (nth (nth k i) la) c)
                      (nth (nth (nth k i) l) c)))
              (comp (comp fst snd) snd (nth automata i)))
          zero_nata (size_list automata)) &&
      (equal_nata (size_list k) (size_list automata) &&
        (all_interval_nat
           (fun i -> equal_nata (size_list (nth k i)) (num_states i)) zero_nata
           (size_list automata) &&
          list_all
            (list_all
              (fun xxs -> equal_nata (size_list xxs) (plus_nata m one_nata)))
            k)) &&
      (all_interval_nat
         (fun i ->
           all_interval_nat
             (fun l -> equal_nata (nth (nth (nth k i) l) zero_nata) zero_nata)
             zero_nata (num_states i))
         zero_nata (size_list automata) &&
         (list_all (fun (_, (_, (_, inv))) -> distinct equal_nat (map fst inv))
            automata &&
           (eq_set (card_UNIV_nat, equal_nat) (image fst (Set s_0))
              (image fst (Set bounds)) &&
             ball (image fst (Set s_0))
               (fun x ->
                 less_eq_int (fst (the (map_of equal_nat bounds x)))
                   (the (map_of equal_nat s_0 x)) &&
                   less_eq_int (the (map_of equal_nat s_0 x))
                     (snd (the (map_of equal_nat bounds x)))))) &&
        (equal_nata (size_list l_0) (size_list automata) &&
          (all_interval_nat
             (fun i ->
               member equal_nat (nth l_0 i)
                 (image fst (Set (comp (comp fst snd) snd (nth automata i)))))
             zero_nata (size_list automata) &&
            subset (card_UNIV_nat, equal_nat)
              (vars_of_formula equal_nat formula)
              (Set (upt zero_nata (n_vs bounds))))));;

let rec simple_Network_Impl_nat_urge_axioms
  automata = list_all (fun (_, (u, (_, _))) -> null u) automata;;

let rec simple_Network_Impl_nat
  broadcast bounds automata m num_states num_actions =
    less_nat zero_nata m &&
      (less_nat zero_nata (size_list automata) &&
        all_interval_nat
          (fun i ->
            (let (_, (_, (trans, _))) = nth automata i in
              list_all
                (fun (l, (_, (_, (_, (_, (_, la)))))) ->
                  less_nat l (num_states i) && less_nat la (num_states i))
                trans))
          zero_nata (size_list automata)) &&
      (all_interval_nat
         (fun i ->
           (let a = nth automata i in
            let (_, aa) = a in
            let (_, ab) = aa in
            let (_, ac) = ab in
             list_all (fun (x, _) -> less_nat x (num_states i)) ac))
         zero_nata (size_list automata) &&
        (list_all
           (fun (_, (_, (trans, _))) ->
             list_all
               (fun (_, (_, (_, (_, (f, (_, _)))))) ->
                 list_all
                   (fun (x, upd) ->
                     less_nat x (n_vs bounds) &&
                       ball (vars_of_exp equal_nat upd)
                         (fun i -> less_nat i (n_vs bounds)))
                   f)
               trans)
           automata &&
          list_all
            (fun (_, (_, (trans, _))) ->
              list_all
                (fun (_, (b, (_, (_, (_, (_, _)))))) ->
                  ball (vars_of_bexp equal_nat b)
                    (fun i -> less_nat i (n_vs bounds)))
                trans)
            automata)) &&
      (all_interval_nat (fun i -> equal_nata (fst (nth bounds i)) i) zero_nata
         (n_vs bounds) &&
         (list_all (fun a -> less_nat a num_actions) broadcast &&
           list_all
             (fun (_, (_, (trans, _))) ->
               list_all
                 (fun (_, a) ->
                   (let (_, aa) = a in
                    let (_, ab) = aa in
                    let (ac, (_, (_, _))) = ab in
                     pred_act equal_nat (fun ad -> less_nat ad num_actions) ac))
                 trans)
             automata) &&
        (list_all
           (fun (_, (_, (trans, _))) ->
             list_all
               (fun (_, (_, (g, (_, (_, (r, _)))))) ->
                 list_all (fun c -> less_nat zero_nata c && less_eq_nat c m)
                   r &&
                   ball (collect_clock_pairs g)
                     (fun (c, x) ->
                       less_nat zero_nata c &&
                         (less_eq_nat c m && less_eq_int zero_inta x)))
               trans)
           automata &&
          (list_all
             (fun (_, a) ->
               (let (_, aa) = a in
                let (_, ab) = aa in
                 list_all
                   (fun (_, g) ->
                     ball (collect_clock_pairs g)
                       (fun (c, x) ->
                         less_nat zero_nata c &&
                           (less_eq_nat c m && less_eq_int zero_inta x)))
                   ab))
             automata &&
            list_all
              (fun (_, (_, (trans, _))) ->
                list_all
                  (fun (_, a) ->
                    (let (_, aa) = a in
                     let (g, ab) = aa in
                     let (ac, (_, (_, _))) = ab in
                      (match ac
                        with In ad ->
                          (if membera equal_nat broadcast ad then null g
                            else true)
                        | Out _ -> true | Sil _ -> true)))
                  trans)
              automata)));;

let rec simple_Network_Impl_nat_urge
  broadcast bounds automata m num_states num_actions =
    simple_Network_Impl_nat broadcast bounds automata m num_states
      num_actions &&
      simple_Network_Impl_nat_urge_axioms automata;;

let rec simple_Network_Impl_nat_ceiling_start_state
  broadcast bounds automata m num_states num_actions k l_0 s_0 formula =
    simple_Network_Impl_nat_urge broadcast bounds automata m num_states
      num_actions &&
      simple_Network_Impl_nat_ceiling_start_state_axioms broadcast bounds
        automata m num_states k l_0 s_0 formula;;

let rec bounds_map bounds = comp the (map_of equal_nat bounds);;

let rec check_boundedi
  bounds s =
    all_interval_nat
      (fun x ->
        less_eq_int (fst (bounds_map bounds x)) (nth s x) &&
          less_eq_int (nth s x) (snd (bounds_map bounds x)))
      zero_nata (size_list s);;

let rec pairs_by_action_impl
  bounds l s out ina =
    maps (fun (p, (b1, (g1, (a1, (f1, (r1, l1)))))) ->
           map_filter
             (fun (q, (b2, (g2, (_, (f2, (r2, l2)))))) ->
               (if equal_nata p q then None
                 else (let sa = mk_updsi (mk_updsi s f1) f2 in
                        (if bvali (equal_int, linorder_int) s b1 &&
                              (bvali (equal_int, linorder_int) s b2 &&
                                check_boundedi bounds sa)
                          then Some (g1 @ g2,
                                      (Bin a1,
(r1 @ r2, (list_update (list_update l p l1) q l2, sa))))
                          else None))))
             out)
      ina;;

let rec actions_by_state
  i = fold (fun t acc ->
             list_update acc (fst (snd (snd t)))
               ((i, t) :: nth acc (fst (snd (snd t)))));;

let rec all_actions_from_vec
  num_actions t vec =
    fold (fun (p, l) -> actions_by_state p (t p l)) vec
      (map (fun _ -> []) (upt zero_nata num_actions));;

let rec all_actions_by_state
  broadcast bounds automata num_actions t l =
    fold (fun i -> actions_by_state i (t i (nth l i)))
      (upt zero_nata (size_list automata))
      (map (fun _ -> []) (upt zero_nata num_actions));;

let rec compute_upds_impl
  bounds init =
    map_filter
      (fun comb ->
        (let a =
           fold (fun (q, (_, (g2, (_, (f2, (r2, l2)))))) (g1, a) ->
                  (let (aa, (r1, (l, s))) = a in
                    (g1 @ g2,
                      (aa, (r1 @ r2, (list_update l q l2, mk_updsi s f2))))))
             comb init
           in
         let (g, aa) = a in
         let (ab, (r, (l, s))) = aa in
          (if check_boundedi bounds s then Some (g, (ab, (r, (l, s))))
            else None)));;

let rec actions_by_statea
  num_actions xs =
    fold (fun t acc ->
           list_update acc (fst (snd (snd t)))
             (t :: nth acc (fst (snd (snd t)))))
      xs (map (fun _ -> []) (upt zero_nata num_actions));;

let rec get_committed
  broadcast bounds automata l =
    map_filter
      (fun p ->
        (let la = nth l p in
          (if membera equal_nat (fst (nth automata p)) la then Some (p, la)
            else None)))
      (upt zero_nata (size_list automata));;

let rec bin_actions
  broadcast num_actions =
    filter (fun a -> not (membera equal_nat broadcast a))
      (upt zero_nata num_actions);;

let rec make_combs
  broadcast bounds automata p a xs =
    (let ys =
       map_filter
         (fun i ->
           (if equal_nata i p then None
             else (if null (nth (nth xs i) a) then None
                    else Some (map (fun aa -> (i, aa)) (nth (nth xs i) a)))))
         (upt zero_nata (size_list automata))
       in
      (if null ys then [] else product_lists ys));;

let rec union_map_of _A
  xs = fold (fun (x, y) m ->
              (match m x with None -> fun_upd _A m x (Some [y])
                | Some ys -> fun_upd _A m x (Some (y :: ys))))
         xs (fun _ -> None);;

let rec trans_map
  automata i =
    (let m = union_map_of equal_nat (fst (snd (snd (nth automata i)))) in
      (fun j -> (match m j with None -> [] | Some xs -> xs)));;

let rec tracei (_B1, _B2, _B3, _B4)
  n show_state show_clock typea =
    (fun (l, m) ->
      (let _ =
         trace_level (Int_of_integer (Z.Int.of_int 5))
           (fun _ ->
             (let st = show_state l in
               (fun f_ () -> f_
                 ((show_dbm_impl (_B1, _B2, _B3) n show_clock
                    (fun x -> shows_prec _B4 zero_nata x []) m)
                 ()) ())
                 (fun ma ->
                   (let s =
                      typea @
                        [Chara (false, true, false, true, true, true, false,
                                 false);
                          Chara (false, false, false, false, false, true, false,
                                  false);
                          Chara (false, false, false, true, false, true, false,
                                  false)] @
                          st @ [Chara (false, false, true, true, false, true,
false, false);
                                 Chara (false, false, false, false, false, true,
 false, false);
                                 Chara (false, false, true, true, true, true,
 false, false)] @
                                 ma @ [Chara
 (false, true, true, true, true, true, false, false);
Chara (true, false, false, true, false, true, false, false)]
                      in
                    let a = implode s in
                     (fun () -> a)))))
         in
        (fun () -> ())));;

let rec reset_canonical_upd_impl (_A1, _A2, _A3)
  n = (fun ai bib bia bi ->
        (fun f_ () -> f_
          ((mtx_set (heap_DBMEntry _A3) (suc n) ai (bia, zero_nata) (Le bi)) ())
          ())
          (fun x ->
            (fun f_ () -> f_
              ((mtx_set (heap_DBMEntry _A3) (suc n) x (zero_nata, bia)
                 (Le (uminus _A2 bi)))
              ()) ())
              (imp_fora one_nata (plus_nata bib one_nata)
                (fun xb sigma ->
                  (if equal_nata xb bia then (fun () -> sigma)
                    else (fun f_ () -> f_
                           ((mtx_get (heap_DBMEntry _A3) (suc n) sigma
                              (zero_nata, xb))
                           ()) ())
                           (fun x_d ->
                             (fun f_ () -> f_
                               ((mtx_get (heap_DBMEntry _A3) (suc n) sigma
                                  (xb, zero_nata))
                               ()) ())
                               (fun x_e ->
                                 (fun f_ () -> f_
                                   ((mtx_set (heap_DBMEntry _A3) (suc n) sigma
                                      (bia, xb)
                                      (plus_DBMEntrya _A1 (Le bi) x_d))
                                   ()) ())
                                   (fun x_f ->
                                     mtx_set (heap_DBMEntry _A3) (suc n) x_f
                                       (xb, bia)
                                       (plus_DBMEntrya _A1 (Le (uminus _A2 bi))
 x_e)))))))));;

let rec up_canonical_upd_impl (_A1, _A2)
  n = (fun ai bi ->
        imp_fora one_nata (plus_nata bi one_nata)
          (fun xa sigma ->
            mtx_set (heap_DBMEntry _A2) (suc n) sigma (xa, zero_nata) INF)
          ai);;

let rec dbm_add_int x0 uu = match x0, uu with INF, uu -> INF
                      | Le v, INF -> INF
                      | Lt v, INF -> INF
                      | Le a, Le b -> Le (plus_inta a b)
                      | Le a, Lt b -> Lt (plus_inta a b)
                      | Lt a, Le b -> Lt (plus_inta a b)
                      | Lt a, Lt b -> Lt (plus_inta a b);;

let rec fw_upd_impl_int
  n = (fun ai bib bia bi ->
        (fun f_ () -> f_
          ((mtx_get (heap_DBMEntry heap_int) (suc n) ai (bia, bib)) ()) ())
          (fun xa ->
            (fun f_ () -> f_
              ((mtx_get (heap_DBMEntry heap_int) (suc n) ai (bib, bi)) ()) ())
              (fun xb ->
                (fun f_ () -> f_
                  ((mtx_get (heap_DBMEntry heap_int) (suc n) ai (bia, bi)) ())
                  ())
                  (fun x ->
                    (let e = dbm_add_int xa xb in
                      (if less_DBMEntry linorder_int e x
                        then mtx_set (heap_DBMEntry heap_int) (suc n) ai
                               (bia, bi) e
                        else (fun () -> ai)))))));;

let rec fw_impl_int
  n = imp_fora zero_nata (plus_nata n one_nata)
        (fun xb ->
          imp_fora zero_nata (plus_nata n one_nata)
            (fun xd ->
              imp_fora zero_nata (plus_nata n one_nata)
                (fun xf sigma -> fw_upd_impl_int n sigma xb xd xf)));;

let rec deadlock_checker
  broadcast bounds automata m num_states num_actions k l_0 s_0 show_clock
    show_state =
    (let n_ps = size_list automata in
     let k_i =
       IArray
         (map (comp (fun a -> IArray a)
                (map (comp (fun a -> IArray a) (map int_of_nat))))
           k)
       in
     let invs =
       IArray
         (map (fun i ->
                (let ma =
                   default_map_of equal_nat []
                     (snd (snd (snd (nth automata i))))
                   in
                 let mb = IArray (map ma (upt zero_nata (num_states i))) in
                  mb))
           (upt zero_nata n_ps))
       in
     let inv_fun =
       (fun (l, _) ->
         maps (fun i -> sub (sub invs i) (nth l i)) (upt zero_nata n_ps))
       in
     let trans_mapa = trans_map automata in
     let trans_i_map =
       (fun i j ->
         map_filter
           (fun (b, a) ->
             (let (g, aa) = a in
              let (ab, (ma, l)) = aa in
               (match ab with In _ -> None | Out _ -> None
                 | Sil ac -> Some (b, (g, (ac, (ma, l)))))))
           (trans_mapa i j))
       in
     let int_trans_from_loc_impl =
       (fun p l la s ->
         (let a = trans_i_map p l in
           map_filter
             (fun (b, aa) ->
               (let (g, ab) = aa in
                let (ac, (f, (r, lb))) = ab in
                let sa = mk_updsi s f in
                 (if bvali (equal_int, linorder_int) s b &&
                       check_boundedi bounds sa
                   then Some (g, (Internal ac, (r, (list_update la p lb, sa))))
                   else None)))
             a))
       in
     let int_trans_from_vec_impl =
       (fun pairs l s ->
         maps (fun (p, la) -> int_trans_from_loc_impl p la l s) pairs)
       in
     let int_trans_from_all_impl =
       (fun l s ->
         maps (fun p -> int_trans_from_loc_impl p (nth l p) l s)
           (upt zero_nata n_ps))
       in
     let trans_out_map =
       (fun i j ->
         map_filter
           (fun (b, a) ->
             (let (g, aa) = a in
              let (ab, (ma, l)) = aa in
               (match ab with In _ -> None
                 | Out ac -> Some (b, (g, (ac, (ma, l)))) | Sil _ -> None)))
           (trans_mapa i j))
       in
     let trans_in_map =
       (fun i j ->
         map_filter
           (fun (b, a) ->
             (let (g, aa) = a in
              let (ab, (ma, l)) = aa in
               (match ab with In ac -> Some (b, (g, (ac, (ma, l))))
                 | Out _ -> None | Sil _ -> None)))
           (trans_mapa i j))
       in
     let trans_out_broad_grouped =
       (fun i j ->
         actions_by_statea num_actions
           (map_filter
             (fun (b, a) ->
               (let (g, aa) = a in
                let (ab, (ma, l)) = aa in
                 (match ab with In _ -> None
                   | Out ac ->
                     (if membera equal_nat broadcast ac
                       then Some (b, (g, (ac, (ma, l)))) else None)
                   | Sil _ -> None)))
             (trans_mapa i j)))
       in
     let trans_in_broad_grouped =
       (fun i j ->
         actions_by_statea num_actions
           (map_filter
             (fun (b, a) ->
               (let (g, aa) = a in
                let (ab, (ma, l)) = aa in
                 (match ab
                   with In ac ->
                     (if membera equal_nat broadcast ac
                       then Some (b, (g, (ac, (ma, l)))) else None)
                   | Out _ -> None | Sil _ -> None)))
             (trans_mapa i j)))
       in
     let broad_trans_impl =
       (fun (l, s) ->
         (let pairs = get_committed broadcast bounds automata l in
          let ina =
            map (fun p -> trans_in_broad_grouped p (nth l p))
              (upt zero_nata n_ps)
            in
          let out =
            map (fun p -> trans_out_broad_grouped p (nth l p))
              (upt zero_nata n_ps)
            in
          let inb =
            map (map (filter
                       (fun (b, _) -> bvali (equal_int, linorder_int) s b)))
              ina
            in
          let outa =
            map (map (filter
                       (fun (b, _) -> bvali (equal_int, linorder_int) s b)))
              out
            in
           (if null pairs
             then maps (fun a ->
                         maps (fun p ->
                                (let outs = nth (nth outa p) a in
                                  (if null outs then []
                                    else (let combs =
    make_combs broadcast bounds automata p a inb in
  let outsa = map (fun aa -> (p, aa)) outs in
  let combsa =
    (if null combs then map (fun x -> [x]) outsa
      else maps (fun x -> map (fun aa -> x :: aa) combs) outsa)
    in
  let init = ([], (Broad a, ([], (l, s)))) in
   compute_upds_impl bounds init combsa))))
                           (upt zero_nata n_ps))
                    (upt zero_nata num_actions)
             else maps (fun a ->
                         (let ins_committed =
                            map_filter
                              (fun (p, _) ->
                                (if not (null (nth (nth inb p) a)) then Some p
                                  else None))
                              pairs
                            in
                          let always_committed =
                            less_nat one_nata (size_list ins_committed) in
                           maps (fun p ->
                                  (let outs = nth (nth outa p) a in
                                    (if null outs then []
                                      else (if not always_committed &&
         ((equal_lista equal_nat ins_committed [p] || null ins_committed) &&
           not (list_ex (fun (q, _) -> equal_nata q p) pairs))
     then []
     else (let combs = make_combs broadcast bounds automata p a inb in
           let outsa = map (fun aa -> (p, aa)) outs in
           let combsa =
             (if null combs then map (fun x -> [x]) outsa
               else maps (fun x -> map (fun aa -> x :: aa) combs) outsa)
             in
           let init = ([], (Broad a, ([], (l, s)))) in
            compute_upds_impl bounds init combsa)))))
                             (upt zero_nata n_ps)))
                    (upt zero_nata num_actions))))
       in
     let bin_trans_impl =
       (fun (l, s) ->
         (let pairs = get_committed broadcast bounds automata l in
          let ina =
            all_actions_by_state broadcast bounds automata num_actions
              trans_in_map l
            in
          let out =
            all_actions_by_state broadcast bounds automata num_actions
              trans_out_map l
            in
           (if null pairs
             then maps (fun a ->
                         pairs_by_action_impl bounds l s (nth out a)
                           (nth ina a))
                    (bin_actions broadcast num_actions)
             else (let in2 = all_actions_from_vec num_actions trans_in_map pairs
                     in
                   let out2 =
                     all_actions_from_vec num_actions trans_out_map pairs in
                    maps (fun a ->
                           pairs_by_action_impl bounds l s (nth out a)
                             (nth in2 a))
                      (bin_actions broadcast num_actions) @
                      maps (fun a ->
                             pairs_by_action_impl bounds l s (nth out2 a)
                               (nth ina a))
                        (bin_actions broadcast num_actions)))))
       in
     let int_trans_impl =
       (fun (l, s) ->
         (let pairs = get_committed broadcast bounds automata l in
           (if null pairs then int_trans_from_all_impl l s
             else int_trans_from_vec_impl pairs l s)))
       in
     let trans_impl =
       (fun st -> int_trans_impl st @ bin_trans_impl st @ broad_trans_impl st)
       in
     let e_op_impl =
       (fun ai bic bib bia bi ->
         (fun f_ () -> f_
           ((up_canonical_upd_impl
              (linordered_cancel_ab_monoid_add_int, heap_int) m bi m)
           ()) ())
           (fun x ->
             (fun f_ () -> f_
               ((imp_nfoldli (inv_fun ai) (fun _ -> (fun () -> true))
                  (fun aia bid ->
                    (fun f_ () -> f_
                      ((abstra_upd_impl
                         (linordered_cancel_ab_monoid_add_int, uminus_int,
                           equal_int, heap_int)
                         m aia bid)
                      ()) ())
                      (fun xa ->
                        repair_pair_impl
                          ((linordered_ab_monoid_add_DBMEntry
                             (linordered_cancel_ab_monoid_add_int, equal_int)),
                            (heap_DBMEntry heap_int))
                          m xa zero_nata (constraint_clk aia)))
                  x)
               ()) ())
               (fun xa ->
                 (fun f_ () -> f_
                   ((check_diag_impla
                      (linordered_cancel_ab_monoid_add_int, heap_int) m m xa)
                   ()) ())
                   (fun xaa ->
                     (fun f_ () -> f_
                       ((if xaa
                          then mtx_set (heap_DBMEntry heap_int) (suc m) xa
                                 (zero_nata, zero_nata) (Lt zero_inta)
                          else imp_nfoldli bib (fun _ -> (fun () -> true))
                                 (fun aia bid ->
                                   (fun f_ () -> f_
                                     ((abstra_upd_impl
(linordered_cancel_ab_monoid_add_int, uminus_int, equal_int, heap_int) m aia
bid)
                                     ()) ())
                                     (fun xb ->
                                       repair_pair_impl
 ((linordered_ab_monoid_add_DBMEntry
    (linordered_cancel_ab_monoid_add_int, equal_int)),
   (heap_DBMEntry heap_int))
 m xb zero_nata (constraint_clk aia)))
                                 xa)
                       ()) ())
                       (fun x_a ->
                         (fun f_ () -> f_
                           ((check_diag_impla
                              (linordered_cancel_ab_monoid_add_int, heap_int) m
                              m x_a)
                           ()) ())
                           (fun xb ->
                             (fun f_ () -> f_
                               ((if xb
                                  then mtx_set (heap_DBMEntry heap_int) (suc m)
 x_a (zero_nata, zero_nata) (Lt zero_inta)
                                  else (fun f_ () -> f_
 ((imp_nfoldli bic (fun _ -> (fun () -> true))
    (fun xc sigma ->
      reset_canonical_upd_impl
        (linordered_cancel_ab_monoid_add_int, uminus_int, heap_int) m sigma m xc
        zero_inta)
    x_a)
 ()) ())
 (imp_nfoldli (inv_fun bia) (fun _ -> (fun () -> true))
   (fun aia bid ->
     (fun f_ () -> f_
       ((abstra_upd_impl
          (linordered_cancel_ab_monoid_add_int, uminus_int, equal_int, heap_int)
          m aia bid)
       ()) ())
       (fun xc ->
         repair_pair_impl
           ((linordered_ab_monoid_add_DBMEntry
              (linordered_cancel_ab_monoid_add_int, equal_int)),
             (heap_DBMEntry heap_int))
           m xc zero_nata (constraint_clk aia)))))
                               ()) ())
                               (fun x_b ->
                                 (fun f_ () -> f_
                                   ((check_diag_impla
                                      (linordered_cancel_ab_monoid_add_int,
heap_int)
                                      m m x_b)
                                   ()) ())
                                   (fun x_c ->
                                     (if x_c
                                       then mtx_set (heap_DBMEntry heap_int)
      (suc m) x_b (zero_nata, zero_nata) (Lt zero_inta)
                                       else (fun f_ () -> f_
      ((norm_upd_impl (linordered_ab_group_add_int, heap_int) m x_b
         (let (l, _) = bia in
           IArray
             (map (fun c ->
                    maxa linorder_int
                      (image (fun i -> sub (sub (sub k_i i) (nth l i)) c)
                        (Set (upt zero_nata n_ps))))
               (upt zero_nata (plus_nata m one_nata))))
         m)
      ()) ())
      (fw_impl_int m))))))))))
       in
     let is_start =
       (fun () ->
         (not (op_list_is_empty
                (trans_impl
                  (l_0, map (comp the (map_of equal_nat s_0))
                          (upt zero_nata (n_vs bounds)))))))
       in
     let key = comp (fun a -> (fun () -> a)) fst in
     let suba =
       (fun ai bi ->
         (let (a1, a2) = ai in
          let (a1a, a2a) = bi in
           (if equal_proda (equal_list equal_nat) (equal_list equal_int) a1 a1a
             then dbm_subset_impl
                    (linordered_cancel_ab_monoid_add_int, equal_int, heap_int) m
                    a2 a2a
             else (fun () -> false))))
       in
     let copy =
       (fun (a1, a2) ->
         (fun f_ () -> f_ ((amtx_copy (heap_DBMEntry heap_int) a2) ()) ())
           (fun x -> (fun () -> (a1, x))))
       in
     let start =
       (fun f_ () -> f_
         ((amtx_dflt (heap_DBMEntry heap_int) (suc m) (suc m) (Le zero_inta))
         ()) ())
         (fun x_a ->
           (fun () ->
             ((l_0, map (comp the (map_of equal_nat s_0))
                      (upt zero_nata (n_vs bounds))),
               x_a)))
       in
     let final = (fun _ -> (fun () -> false)) in
     let succs =
       (fun (a1, a2) ->
         imp_nfoldli (trans_impl a1) (fun _ -> (fun () -> true))
           (fun xc sigma ->
             (let (a1a, (_, (a1c, a2c))) = xc in
               (fun f_ () -> f_ ((amtx_copy (heap_DBMEntry heap_int) a2) ()) ())
                 (fun x ->
                   (fun f_ () -> f_ ((e_op_impl a1 a1c a1a a2c x) ()) ())
                     (fun xa ->
                       (fun () -> (op_list_prepend (a2c, xa) sigma))))))
           [])
       in
     let empty =
       (fun (_, a) ->
         check_diag_impl (linordered_cancel_ab_monoid_add_int, heap_int) m a)
       in
     let p =
       (fun (a1, a2) ->
         (fun f_ () -> f_
           (((fun f_ () -> f_
               ((imp_nfoldli (trans_impl a1) (fun _ -> (fun () -> true))
                  (fun xb sigma ->
                    (fun f_ () -> f_
                      ((v_dbm_impl
                         (linordered_cancel_ab_monoid_add_int, heap_int) m)
                      ()) ())
                      (fun x ->
                        (fun f_ () -> f_
                          ((abstr_FW_impl
                             (linordered_cancel_ab_monoid_add_int, uminus_int,
                               equal_int, heap_int)
                             m (inv_fun (snd (snd (snd xb)))) x)
                          ()) ())
                          (fun xa ->
                            (fun f_ () -> f_
                              ((pre_reset_list_impl m xa (fst (snd (snd xb))))
                              ()) ())
                              (fun xc ->
                                (fun f_ () -> f_
                                  ((abstr_FW_impl
                                     (linordered_cancel_ab_monoid_add_int,
                                       uminus_int, equal_int, heap_int)
                                     m (fst xb) xc)
                                  ()) ())
                                  (fun xd ->
                                    (fun f_ () -> f_
                                      ((abstr_FW_impl
 (linordered_cancel_ab_monoid_add_int, uminus_int, equal_int, heap_int) m
 (inv_fun a1) xd)
                                      ()) ())
                                      (fun xe ->
(fun f_ () -> f_
  ((down_impl (linordered_cancel_ab_monoid_add_int, equal_int, heap_int) m xe)
  ()) ())
  (fun x_c -> (fun () -> (x_c :: sigma)))))))))
                  [])
               ()) ())
              (fun x -> dbm_subset_fed_impl m a2 (op_list_rev x)))
           ()) ())
           (fun x -> (fun () -> (not x))))
       in
     let trace =
       tracei (linordered_ab_group_add_int, equal_int, heap_int, show_int) m
         show_state show_clock
       in
      (fun f_ () -> f_ (is_start ()) ())
        (fun r1 ->
          (if r1
            then (fun f_ () -> f_
                   ((check_passed_impl
                      (heap_prod
                        (heap_prod (heap_list heap_nat) (heap_list heap_int))
                        (heap_array (typerep_DBMEntry typerep_int)))
                      ((equal_prod (equal_list equal_nat)
                         (equal_list equal_int)),
                        (hashable_prod (hashable_list hashable_nat)
                          (hashable_list hashable_int)),
                        (heap_prod (heap_list heap_nat) (heap_list heap_int)))
                      succs start final suba empty key copy trace p)
                   ()) ())
                   (fun a -> (fun () -> a))
            else (fun () -> true))));;

let rec precond_dc
  show_clock show_state broadcast bounds automata m num_states num_actions k l_0
    s_0 formula =
    (if simple_Network_Impl_nat_ceiling_start_state broadcast bounds automata m
          num_states num_actions k l_0 s_0 formula
      then (fun f_ () -> f_
             ((deadlock_checker broadcast bounds automata m num_states
                num_actions k l_0 s_0 show_clock show_state)
             ()) ())
             (fun x -> (fun () -> (Some x)))
      else (fun () -> None));;

let rec check_sexpi _A
  x0 uu uv = match x0, uu, uv with Truea, uu, uv -> true
    | Nota e, l, s -> not (check_sexpi _A e l s)
    | Anda (e1, e2), l, s -> check_sexpi _A e1 l s && check_sexpi _A e2 l s
    | Ora (e1, e2), l, s -> check_sexpi _A e1 l s || check_sexpi _A e2 l s
    | Implya (e1, e2), l, s ->
        (if check_sexpi _A e1 l s then check_sexpi _A e2 l s else true)
    | Eqa (i, x), l, s -> equal_inta (nth s i) x
    | Leb (i, x), l, s -> less_eq_int (nth s i) x
    | Ltb (i, x), l, s -> less_int (nth s i) x
    | Gea (i, x), l, s -> less_eq_int x (nth s i)
    | Gta (i, x), l, s -> less_int x (nth s i)
    | Loc (i, x), l, s -> eq _A (nth l i) x;;

let rec hd_of_formulai _A
  x0 l s = match x0, l, s with EX phi, l, s -> check_sexpi _A phi l s
    | EG phi, l, s -> check_sexpi _A phi l s
    | AX phi, l, s -> not (check_sexpi _A phi l s)
    | AG phi, l, s -> not (check_sexpi _A phi l s)
    | Leadsto (phi, uu), l, s -> check_sexpi _A phi l s;;

let rec reachability_checker
  broadcast bounds automata m num_states num_actions k l_0 s_0 formula
    show_clock show_state =
    (fun f_ () -> f_
      ((let key = comp (fun a -> (fun () -> a)) fst in
        let suba =
          (fun ai bi ->
            (let (a1, a2) = ai in
             let (a1a, a2a) = bi in
              (if equal_proda (equal_list equal_nat) (equal_list equal_int) a1
                    a1a
                then dbm_subset_impl
                       (linordered_cancel_ab_monoid_add_int, equal_int,
                         heap_int)
                       m a2 a2a
                else (fun () -> false))))
          in
        let copy =
          (fun (a1, a2) ->
            (fun f_ () -> f_ ((amtx_copy (heap_DBMEntry heap_int) a2) ()) ())
              (fun x -> (fun () -> (a1, x))))
          in
        let start =
          (fun f_ () -> f_
            ((amtx_dflt (heap_DBMEntry heap_int) (suc m) (suc m) (Le zero_inta))
            ()) ())
            (fun x_a ->
              (fun () ->
                ((l_0, map (comp the (map_of equal_nat s_0))
                         (upt zero_nata (n_vs bounds))),
                  x_a)))
          in
        let final =
          (fun xi ->
            (fun () ->
              (let ((a, b), _) = xi in hd_of_formulai equal_nat formula a b)))
          in
        let succs =
          (let n_ps = size_list automata in
           let k_i =
             IArray
               (map (comp (fun a -> IArray a)
                      (map (comp (fun a -> IArray a) (map int_of_nat))))
                 k)
             in
           let invs =
             IArray
               (map (fun i ->
                      (let ma =
                         default_map_of equal_nat []
                           (snd (snd (snd (nth automata i))))
                         in
                       let mb = IArray (map ma (upt zero_nata (num_states i)))
                         in
                        mb))
                 (upt zero_nata n_ps))
             in
           let inv_fun =
             (fun (l, _) ->
               maps (fun i -> sub (sub invs i) (nth l i)) (upt zero_nata n_ps))
             in
           let trans_mapa = trans_map automata in
           let trans_i_map =
             (fun i j ->
               map_filter
                 (fun (b, a) ->
                   (let (g, aa) = a in
                    let (ab, (ma, l)) = aa in
                     (match ab with In _ -> None | Out _ -> None
                       | Sil ac -> Some (b, (g, (ac, (ma, l)))))))
                 (trans_mapa i j))
             in
           let int_trans_from_loc_impl =
             (fun p l la s ->
               (let a = trans_i_map p l in
                 map_filter
                   (fun (b, aa) ->
                     (let (g, ab) = aa in
                      let (ac, (f, (r, lb))) = ab in
                      let sa = mk_updsi s f in
                       (if bvali (equal_int, linorder_int) s b &&
                             check_boundedi bounds sa
                         then Some (g, (Internal ac,
 (r, (list_update la p lb, sa))))
                         else None)))
                   a))
             in
           let int_trans_from_vec_impl =
             (fun pairs l s ->
               maps (fun (p, la) -> int_trans_from_loc_impl p la l s) pairs)
             in
           let int_trans_from_all_impl =
             (fun l s ->
               maps (fun p -> int_trans_from_loc_impl p (nth l p) l s)
                 (upt zero_nata n_ps))
             in
           let trans_out_map =
             (fun i j ->
               map_filter
                 (fun (b, a) ->
                   (let (g, aa) = a in
                    let (ab, (ma, l)) = aa in
                     (match ab with In _ -> None
                       | Out ac -> Some (b, (g, (ac, (ma, l))))
                       | Sil _ -> None)))
                 (trans_mapa i j))
             in
           let trans_in_map =
             (fun i j ->
               map_filter
                 (fun (b, a) ->
                   (let (g, aa) = a in
                    let (ab, (ma, l)) = aa in
                     (match ab with In ac -> Some (b, (g, (ac, (ma, l))))
                       | Out _ -> None | Sil _ -> None)))
                 (trans_mapa i j))
             in
           let trans_out_broad_grouped =
             (fun i j ->
               actions_by_statea num_actions
                 (map_filter
                   (fun (b, a) ->
                     (let (g, aa) = a in
                      let (ab, (ma, l)) = aa in
                       (match ab with In _ -> None
                         | Out ac ->
                           (if membera equal_nat broadcast ac
                             then Some (b, (g, (ac, (ma, l)))) else None)
                         | Sil _ -> None)))
                   (trans_mapa i j)))
             in
           let trans_in_broad_grouped =
             (fun i j ->
               actions_by_statea num_actions
                 (map_filter
                   (fun (b, a) ->
                     (let (g, aa) = a in
                      let (ab, (ma, l)) = aa in
                       (match ab
                         with In ac ->
                           (if membera equal_nat broadcast ac
                             then Some (b, (g, (ac, (ma, l)))) else None)
                         | Out _ -> None | Sil _ -> None)))
                   (trans_mapa i j)))
             in
           let broad_trans_impl =
             (fun (l, s) ->
               (let pairs = get_committed broadcast bounds automata l in
                let ina =
                  map (fun p -> trans_in_broad_grouped p (nth l p))
                    (upt zero_nata n_ps)
                  in
                let out =
                  map (fun p -> trans_out_broad_grouped p (nth l p))
                    (upt zero_nata n_ps)
                  in
                let inb =
                  map (map (filter
                             (fun (b, _) ->
                               bvali (equal_int, linorder_int) s b)))
                    ina
                  in
                let outa =
                  map (map (filter
                             (fun (b, _) ->
                               bvali (equal_int, linorder_int) s b)))
                    out
                  in
                 (if null pairs
                   then maps (fun a ->
                               maps (fun p ->
                                      (let outs = nth (nth outa p) a in
(if null outs then []
  else (let combs = make_combs broadcast bounds automata p a inb in
        let outsa = map (fun aa -> (p, aa)) outs in
        let combsa =
          (if null combs then map (fun x -> [x]) outsa
            else maps (fun x -> map (fun aa -> x :: aa) combs) outsa)
          in
        let init = ([], (Broad a, ([], (l, s)))) in
         compute_upds_impl bounds init combsa))))
                                 (upt zero_nata n_ps))
                          (upt zero_nata num_actions)
                   else maps (fun a ->
                               (let ins_committed =
                                  map_filter
                                    (fun (p, _) ->
                                      (if not (null (nth (nth inb p) a))
then Some p else None))
                                    pairs
                                  in
                                let always_committed =
                                  less_nat one_nata (size_list ins_committed) in
                                 maps (fun p ->
(let outs = nth (nth outa p) a in
  (if null outs then []
    else (if not always_committed &&
               ((equal_lista equal_nat ins_committed [p] ||
                  null ins_committed) &&
                 not (list_ex (fun (q, _) -> equal_nata q p) pairs))
           then []
           else (let combs = make_combs broadcast bounds automata p a inb in
                 let outsa = map (fun aa -> (p, aa)) outs in
                 let combsa =
                   (if null combs then map (fun x -> [x]) outsa
                     else maps (fun x -> map (fun aa -> x :: aa) combs) outsa)
                   in
                 let init = ([], (Broad a, ([], (l, s)))) in
                  compute_upds_impl bounds init combsa)))))
                                   (upt zero_nata n_ps)))
                          (upt zero_nata num_actions))))
             in
           let bin_trans_impl =
             (fun (l, s) ->
               (let pairs = get_committed broadcast bounds automata l in
                let ina =
                  all_actions_by_state broadcast bounds automata num_actions
                    trans_in_map l
                  in
                let out =
                  all_actions_by_state broadcast bounds automata num_actions
                    trans_out_map l
                  in
                 (if null pairs
                   then maps (fun a ->
                               pairs_by_action_impl bounds l s (nth out a)
                                 (nth ina a))
                          (bin_actions broadcast num_actions)
                   else (let in2 =
                           all_actions_from_vec num_actions trans_in_map pairs
                           in
                         let out2 =
                           all_actions_from_vec num_actions trans_out_map pairs
                           in
                          maps (fun a ->
                                 pairs_by_action_impl bounds l s (nth out a)
                                   (nth in2 a))
                            (bin_actions broadcast num_actions) @
                            maps (fun a ->
                                   pairs_by_action_impl bounds l s (nth out2 a)
                                     (nth ina a))
                              (bin_actions broadcast num_actions)))))
             in
           let int_trans_impl =
             (fun (l, s) ->
               (let pairs = get_committed broadcast bounds automata l in
                 (if null pairs then int_trans_from_all_impl l s
                   else int_trans_from_vec_impl pairs l s)))
             in
           let trans_impl =
             (fun st ->
               int_trans_impl st @ bin_trans_impl st @ broad_trans_impl st)
             in
           let e_op_impl =
             (fun ai bic bib bia bi ->
               (fun f_ () -> f_
                 ((up_canonical_upd_impl
                    (linordered_cancel_ab_monoid_add_int, heap_int) m bi m)
                 ()) ())
                 (fun x ->
                   (fun f_ () -> f_
                     ((imp_nfoldli (inv_fun ai) (fun _ -> (fun () -> true))
                        (fun aia bid ->
                          (fun f_ () -> f_
                            ((abstra_upd_impl
                               (linordered_cancel_ab_monoid_add_int, uminus_int,
                                 equal_int, heap_int)
                               m aia bid)
                            ()) ())
                            (fun xa ->
                              repair_pair_impl
                                ((linordered_ab_monoid_add_DBMEntry
                                   (linordered_cancel_ab_monoid_add_int,
                                     equal_int)),
                                  (heap_DBMEntry heap_int))
                                m xa zero_nata (constraint_clk aia)))
                        x)
                     ()) ())
                     (fun xa ->
                       (fun f_ () -> f_
                         ((check_diag_impla
                            (linordered_cancel_ab_monoid_add_int, heap_int) m m
                            xa)
                         ()) ())
                         (fun xaa ->
                           (fun f_ () -> f_
                             ((if xaa
                                then mtx_set (heap_DBMEntry heap_int) (suc m) xa
                                       (zero_nata, zero_nata) (Lt zero_inta)
                                else imp_nfoldli bib (fun _ -> (fun () -> true))
                                       (fun aia bid ->
 (fun f_ () -> f_
   ((abstra_upd_impl
      (linordered_cancel_ab_monoid_add_int, uminus_int, equal_int, heap_int) m
      aia bid)
   ()) ())
   (fun xb ->
     repair_pair_impl
       ((linordered_ab_monoid_add_DBMEntry
          (linordered_cancel_ab_monoid_add_int, equal_int)),
         (heap_DBMEntry heap_int))
       m xb zero_nata (constraint_clk aia)))
                                       xa)
                             ()) ())
                             (fun x_a ->
                               (fun f_ () -> f_
                                 ((check_diag_impla
                                    (linordered_cancel_ab_monoid_add_int,
                                      heap_int)
                                    m m x_a)
                                 ()) ())
                                 (fun xb ->
                                   (fun f_ () -> f_
                                     ((if xb
then mtx_set (heap_DBMEntry heap_int) (suc m) x_a (zero_nata, zero_nata)
       (Lt zero_inta)
else (fun f_ () -> f_
       ((imp_nfoldli bic (fun _ -> (fun () -> true))
          (fun xc sigma ->
            reset_canonical_upd_impl
              (linordered_cancel_ab_monoid_add_int, uminus_int, heap_int) m
              sigma m xc zero_inta)
          x_a)
       ()) ())
       (imp_nfoldli (inv_fun bia) (fun _ -> (fun () -> true))
         (fun aia bid ->
           (fun f_ () -> f_
             ((abstra_upd_impl
                (linordered_cancel_ab_monoid_add_int, uminus_int, equal_int,
                  heap_int)
                m aia bid)
             ()) ())
             (fun xc ->
               repair_pair_impl
                 ((linordered_ab_monoid_add_DBMEntry
                    (linordered_cancel_ab_monoid_add_int, equal_int)),
                   (heap_DBMEntry heap_int))
                 m xc zero_nata (constraint_clk aia)))))
                                     ()) ())
                                     (fun x_b ->
                                       (fun f_ () -> f_
 ((check_diag_impla (linordered_cancel_ab_monoid_add_int, heap_int) m m x_b) ())
 ())
 (fun x_c ->
   (if x_c
     then mtx_set (heap_DBMEntry heap_int) (suc m) x_b (zero_nata, zero_nata)
            (Lt zero_inta)
     else (fun f_ () -> f_
            ((norm_upd_impl (linordered_ab_group_add_int, heap_int) m x_b
               (let (l, _) = bia in
                 IArray
                   (map (fun c ->
                          maxa linorder_int
                            (image (fun i -> sub (sub (sub k_i i) (nth l i)) c)
                              (Set (upt zero_nata n_ps))))
                     (upt zero_nata (plus_nata m one_nata))))
               m)
            ()) ())
            (fw_impl_int m))))))))))
             in
            (fun (a1, a2) ->
              imp_nfoldli (trans_impl a1) (fun _ -> (fun () -> true))
                (fun xc sigma ->
                  (let (a1a, (_, (a1c, a2c))) = xc in
                    (fun f_ () -> f_ ((amtx_copy (heap_DBMEntry heap_int) a2)
                      ()) ())
                      (fun x ->
                        (fun f_ () -> f_ ((e_op_impl a1 a1c a1a a2c x) ()) ())
                          (fun xa ->
                            (fun () -> (op_list_prepend (a2c, xa) sigma))))))
                []))
          in
        let empty =
          (fun (_, a) ->
            check_diag_impl (linordered_cancel_ab_monoid_add_int, heap_int) m a)
          in
        let trace =
          tracei (linordered_ab_group_add_int, equal_int, heap_int, show_int) m
            show_state show_clock
          in
         pw_impl
           (heap_prod (heap_prod (heap_list heap_nat) (heap_list heap_int))
             (heap_array (typerep_DBMEntry typerep_int)))
           ((equal_prod (equal_list equal_nat) (equal_list equal_int)),
             (hashable_prod (hashable_list hashable_nat)
               (hashable_list hashable_int)),
             (heap_prod (heap_list heap_nat) (heap_list heap_int)))
           key copy trace suba start final succs empty)
      ()) ())
      (fun x ->
        (fun f_ () -> f_ ((fun () -> ()) ()) ()) (fun _ -> (fun () -> x)));;

let rec leadsto_checker
  broadcast bounds automata m num_states num_actions k l_0 s_0 formula psi
    show_clock show_state =
    (fun f_ () -> f_
      ((let key = comp (fun a -> (fun () -> a)) fst in
        let suba =
          (fun ai bi ->
            (let (a1, a2) = ai in
             let (a1a, a2a) = bi in
              (if equal_proda (equal_list equal_nat) (equal_list equal_int) a1
                    a1a
                then dbm_subset_impl
                       (linordered_cancel_ab_monoid_add_int, equal_int,
                         heap_int)
                       m a2 a2a
                else (fun () -> false))))
          in
        let copy =
          (fun (a1, a2) ->
            (fun f_ () -> f_ ((amtx_copy (heap_DBMEntry heap_int) a2) ()) ())
              (fun x -> (fun () -> (a1, x))))
          in
        let start =
          (fun f_ () -> f_
            ((amtx_dflt (heap_DBMEntry heap_int) (suc m) (suc m) (Le zero_inta))
            ()) ())
            (fun x_a ->
              (fun () ->
                ((l_0, map (comp the (map_of equal_nat s_0))
                         (upt zero_nata (n_vs bounds))),
                  x_a)))
          in
        let final =
          (fun xi ->
            (fun () ->
              (let ((a, b), _) = xi in hd_of_formulai equal_nat formula a b)))
          in
        let finala =
          (fun xi ->
            (fun () ->
              (let ((l, s), _) = xi in not (check_sexpi equal_nat psi l s))))
          in
        let succs =
          (fun xi ->
            (fun f_ () -> f_
              (((let n_ps = size_list automata in
                 let k_i =
                   IArray
                     (map (comp (fun a -> IArray a)
                            (map (comp (fun a -> IArray a) (map int_of_nat))))
                       k)
                   in
                 let invs =
                   IArray
                     (map (fun i ->
                            (let ma =
                               default_map_of equal_nat []
                                 (snd (snd (snd (nth automata i))))
                               in
                             let mb =
                               IArray (map ma (upt zero_nata (num_states i))) in
                              mb))
                       (upt zero_nata n_ps))
                   in
                 let inv_fun =
                   (fun (l, _) ->
                     maps (fun i -> sub (sub invs i) (nth l i))
                       (upt zero_nata n_ps))
                   in
                 let trans_mapa = trans_map automata in
                 let trans_i_map =
                   (fun i j ->
                     map_filter
                       (fun (b, a) ->
                         (let (g, aa) = a in
                          let (ab, (ma, l)) = aa in
                           (match ab with In _ -> None | Out _ -> None
                             | Sil ac -> Some (b, (g, (ac, (ma, l)))))))
                       (trans_mapa i j))
                   in
                 let int_trans_from_loc_impl =
                   (fun p l la s ->
                     (let a = trans_i_map p l in
                       map_filter
                         (fun (b, aa) ->
                           (let (g, ab) = aa in
                            let (ac, (f, (r, lb))) = ab in
                            let sa = mk_updsi s f in
                             (if bvali (equal_int, linorder_int) s b &&
                                   check_boundedi bounds sa
                               then Some (g,
   (Internal ac, (r, (list_update la p lb, sa))))
                               else None)))
                         a))
                   in
                 let int_trans_from_vec_impl =
                   (fun pairs l s ->
                     maps (fun (p, la) -> int_trans_from_loc_impl p la l s)
                       pairs)
                   in
                 let int_trans_from_all_impl =
                   (fun l s ->
                     maps (fun p -> int_trans_from_loc_impl p (nth l p) l s)
                       (upt zero_nata n_ps))
                   in
                 let trans_out_map =
                   (fun i j ->
                     map_filter
                       (fun (b, a) ->
                         (let (g, aa) = a in
                          let (ab, (ma, l)) = aa in
                           (match ab with In _ -> None
                             | Out ac -> Some (b, (g, (ac, (ma, l))))
                             | Sil _ -> None)))
                       (trans_mapa i j))
                   in
                 let trans_in_map =
                   (fun i j ->
                     map_filter
                       (fun (b, a) ->
                         (let (g, aa) = a in
                          let (ab, (ma, l)) = aa in
                           (match ab with In ac -> Some (b, (g, (ac, (ma, l))))
                             | Out _ -> None | Sil _ -> None)))
                       (trans_mapa i j))
                   in
                 let trans_out_broad_grouped =
                   (fun i j ->
                     actions_by_statea num_actions
                       (map_filter
                         (fun (b, a) ->
                           (let (g, aa) = a in
                            let (ab, (ma, l)) = aa in
                             (match ab with In _ -> None
                               | Out ac ->
                                 (if membera equal_nat broadcast ac
                                   then Some (b, (g, (ac, (ma, l)))) else None)
                               | Sil _ -> None)))
                         (trans_mapa i j)))
                   in
                 let trans_in_broad_grouped =
                   (fun i j ->
                     actions_by_statea num_actions
                       (map_filter
                         (fun (b, a) ->
                           (let (g, aa) = a in
                            let (ab, (ma, l)) = aa in
                             (match ab
                               with In ac ->
                                 (if membera equal_nat broadcast ac
                                   then Some (b, (g, (ac, (ma, l)))) else None)
                               | Out _ -> None | Sil _ -> None)))
                         (trans_mapa i j)))
                   in
                 let broad_trans_impl =
                   (fun (l, s) ->
                     (let pairs = get_committed broadcast bounds automata l in
                      let ina =
                        map (fun p -> trans_in_broad_grouped p (nth l p))
                          (upt zero_nata n_ps)
                        in
                      let out =
                        map (fun p -> trans_out_broad_grouped p (nth l p))
                          (upt zero_nata n_ps)
                        in
                      let inb =
                        map (map (filter
                                   (fun (b, _) ->
                                     bvali (equal_int, linorder_int) s b)))
                          ina
                        in
                      let outa =
                        map (map (filter
                                   (fun (b, _) ->
                                     bvali (equal_int, linorder_int) s b)))
                          out
                        in
                       (if null pairs
                         then maps (fun a ->
                                     maps (fun p ->
    (let outs = nth (nth outa p) a in
      (if null outs then []
        else (let combs = make_combs broadcast bounds automata p a inb in
              let outsa = map (fun aa -> (p, aa)) outs in
              let combsa =
                (if null combs then map (fun x -> [x]) outsa
                  else maps (fun x -> map (fun aa -> x :: aa) combs) outsa)
                in
              let init = ([], (Broad a, ([], (l, s)))) in
               compute_upds_impl bounds init combsa))))
                                       (upt zero_nata n_ps))
                                (upt zero_nata num_actions)
                         else maps (fun a ->
                                     (let ins_committed =
map_filter
  (fun (p, _) -> (if not (null (nth (nth inb p) a)) then Some p else None))
  pairs
in
                                      let always_committed =
less_nat one_nata (size_list ins_committed) in
                                       maps
 (fun p ->
   (let outs = nth (nth outa p) a in
     (if null outs then []
       else (if not always_committed &&
                  ((equal_lista equal_nat ins_committed [p] ||
                     null ins_committed) &&
                    not (list_ex (fun (q, _) -> equal_nata q p) pairs))
              then []
              else (let combs = make_combs broadcast bounds automata p a inb in
                    let outsa = map (fun aa -> (p, aa)) outs in
                    let combsa =
                      (if null combs then map (fun x -> [x]) outsa
                        else maps (fun x -> map (fun aa -> x :: aa) combs)
                               outsa)
                      in
                    let init = ([], (Broad a, ([], (l, s)))) in
                     compute_upds_impl bounds init combsa)))))
 (upt zero_nata n_ps)))
                                (upt zero_nata num_actions))))
                   in
                 let bin_trans_impl =
                   (fun (l, s) ->
                     (let pairs = get_committed broadcast bounds automata l in
                      let ina =
                        all_actions_by_state broadcast bounds automata
                          num_actions trans_in_map l
                        in
                      let out =
                        all_actions_by_state broadcast bounds automata
                          num_actions trans_out_map l
                        in
                       (if null pairs
                         then maps (fun a ->
                                     pairs_by_action_impl bounds l s (nth out a)
                                       (nth ina a))
                                (bin_actions broadcast num_actions)
                         else (let in2 =
                                 all_actions_from_vec num_actions trans_in_map
                                   pairs
                                 in
                               let out2 =
                                 all_actions_from_vec num_actions trans_out_map
                                   pairs
                                 in
                                maps (fun a ->
                                       pairs_by_action_impl bounds l s
 (nth out a) (nth in2 a))
                                  (bin_actions broadcast num_actions) @
                                  maps (fun a ->
 pairs_by_action_impl bounds l s (nth out2 a) (nth ina a))
                                    (bin_actions broadcast num_actions)))))
                   in
                 let int_trans_impl =
                   (fun (l, s) ->
                     (let pairs = get_committed broadcast bounds automata l in
                       (if null pairs then int_trans_from_all_impl l s
                         else int_trans_from_vec_impl pairs l s)))
                   in
                 let trans_impl =
                   (fun st ->
                     int_trans_impl st @
                       bin_trans_impl st @ broad_trans_impl st)
                   in
                 let e_op_impl =
                   (fun ai bic bib bia bi ->
                     (fun f_ () -> f_
                       ((up_canonical_upd_impl
                          (linordered_cancel_ab_monoid_add_int, heap_int) m bi
                          m)
                       ()) ())
                       (fun x ->
                         (fun f_ () -> f_
                           ((imp_nfoldli (inv_fun ai)
                              (fun _ -> (fun () -> true))
                              (fun aia bid ->
                                (fun f_ () -> f_
                                  ((abstra_upd_impl
                                     (linordered_cancel_ab_monoid_add_int,
                                       uminus_int, equal_int, heap_int)
                                     m aia bid)
                                  ()) ())
                                  (fun xa ->
                                    repair_pair_impl
                                      ((linordered_ab_monoid_add_DBMEntry
 (linordered_cancel_ab_monoid_add_int, equal_int)),
(heap_DBMEntry heap_int))
                                      m xa zero_nata (constraint_clk aia)))
                              x)
                           ()) ())
                           (fun xa ->
                             (fun f_ () -> f_
                               ((check_diag_impla
                                  (linordered_cancel_ab_monoid_add_int,
                                    heap_int)
                                  m m xa)
                               ()) ())
                               (fun xaa ->
                                 (fun f_ () -> f_
                                   ((if xaa
                                      then mtx_set (heap_DBMEntry heap_int)
     (suc m) xa (zero_nata, zero_nata) (Lt zero_inta)
                                      else imp_nfoldli bib
     (fun _ -> (fun () -> true))
     (fun aia bid ->
       (fun f_ () -> f_
         ((abstra_upd_impl
            (linordered_cancel_ab_monoid_add_int, uminus_int, equal_int,
              heap_int)
            m aia bid)
         ()) ())
         (fun xb ->
           repair_pair_impl
             ((linordered_ab_monoid_add_DBMEntry
                (linordered_cancel_ab_monoid_add_int, equal_int)),
               (heap_DBMEntry heap_int))
             m xb zero_nata (constraint_clk aia)))
     xa)
                                   ()) ())
                                   (fun x_a ->
                                     (fun f_ () -> f_
                                       ((check_diag_impla
  (linordered_cancel_ab_monoid_add_int, heap_int) m m x_a)
                                       ()) ())
                                       (fun xb ->
 (fun f_ () -> f_
   ((if xb
      then mtx_set (heap_DBMEntry heap_int) (suc m) x_a (zero_nata, zero_nata)
             (Lt zero_inta)
      else (fun f_ () -> f_
             ((imp_nfoldli bic (fun _ -> (fun () -> true))
                (fun xc sigma ->
                  reset_canonical_upd_impl
                    (linordered_cancel_ab_monoid_add_int, uminus_int, heap_int)
                    m sigma m xc zero_inta)
                x_a)
             ()) ())
             (imp_nfoldli (inv_fun bia) (fun _ -> (fun () -> true))
               (fun aia bid ->
                 (fun f_ () -> f_
                   ((abstra_upd_impl
                      (linordered_cancel_ab_monoid_add_int, uminus_int,
                        equal_int, heap_int)
                      m aia bid)
                   ()) ())
                   (fun xc ->
                     repair_pair_impl
                       ((linordered_ab_monoid_add_DBMEntry
                          (linordered_cancel_ab_monoid_add_int, equal_int)),
                         (heap_DBMEntry heap_int))
                       m xc zero_nata (constraint_clk aia)))))
   ()) ())
   (fun x_b ->
     (fun f_ () -> f_
       ((check_diag_impla (linordered_cancel_ab_monoid_add_int, heap_int) m m
          x_b)
       ()) ())
       (fun x_c ->
         (if x_c
           then mtx_set (heap_DBMEntry heap_int) (suc m) x_b
                  (zero_nata, zero_nata) (Lt zero_inta)
           else (fun f_ () -> f_
                  ((norm_upd_impl (linordered_ab_group_add_int, heap_int) m x_b
                     (let (l, _) = bia in
                       IArray
                         (map (fun c ->
                                maxa linorder_int
                                  (image
                                    (fun i -> sub (sub (sub k_i i) (nth l i)) c)
                                    (Set (upt zero_nata n_ps))))
                           (upt zero_nata (plus_nata m one_nata))))
                     m)
                  ()) ())
                  (fw_impl_int m))))))))))
                   in
                  (fun (a1, a2) ->
                    imp_nfoldli (trans_impl a1) (fun _ -> (fun () -> true))
                      (fun xc sigma ->
                        (let (a1a, (_, (a1c, a2c))) = xc in
                          (if (let (l, s) = a2c in
                                not (check_sexpi equal_nat psi l s))
                            then (fun f_ () -> f_
                                   ((amtx_copy (heap_DBMEntry heap_int) a2) ())
                                   ())
                                   (fun x ->
                                     (fun f_ () -> f_
                                       ((e_op_impl a1 a1c a1a a2c x) ()) ())
                                       (fun xa ->
 (fun () -> (op_list_prepend (a2c, xa) sigma))))
                            else (fun () -> sigma))))
                      []))
                 xi)
              ()) ())
              (fun x ->
                (fun f_ () -> f_
                  ((imp_nfoldli x (fun _ -> (fun () -> true))
                     (fun xc sigma ->
                       (fun f_ () -> f_
                         ((let (_, a2) = xc in
                            (fun f_ () -> f_
                              ((check_diag_impl
                                 (linordered_cancel_ab_monoid_add_int, heap_int)
                                 m a2)
                              ()) ())
                              (fun x_c -> (fun () -> (not x_c))))
                         ()) ())
                         (fun x_c ->
                           (fun () ->
                             (if x_c then op_list_prepend xc sigma
                               else sigma))))
                     [])
                  ()) ())
                  (fun xa -> (fun () -> (op_list_rev xa)))))
          in
        let succsa =
          (fun xi ->
            (fun f_ () -> f_
              (((let n_ps = size_list automata in
                 let k_i =
                   IArray
                     (map (comp (fun a -> IArray a)
                            (map (comp (fun a -> IArray a) (map int_of_nat))))
                       k)
                   in
                 let invs =
                   IArray
                     (map (fun i ->
                            (let ma =
                               default_map_of equal_nat []
                                 (snd (snd (snd (nth automata i))))
                               in
                             let mb =
                               IArray (map ma (upt zero_nata (num_states i))) in
                              mb))
                       (upt zero_nata n_ps))
                   in
                 let inv_fun =
                   (fun (l, _) ->
                     maps (fun i -> sub (sub invs i) (nth l i))
                       (upt zero_nata n_ps))
                   in
                 let trans_mapa = trans_map automata in
                 let trans_i_map =
                   (fun i j ->
                     map_filter
                       (fun (b, a) ->
                         (let (g, aa) = a in
                          let (ab, (ma, l)) = aa in
                           (match ab with In _ -> None | Out _ -> None
                             | Sil ac -> Some (b, (g, (ac, (ma, l)))))))
                       (trans_mapa i j))
                   in
                 let int_trans_from_loc_impl =
                   (fun p l la s ->
                     (let a = trans_i_map p l in
                       map_filter
                         (fun (b, aa) ->
                           (let (g, ab) = aa in
                            let (ac, (f, (r, lb))) = ab in
                            let sa = mk_updsi s f in
                             (if bvali (equal_int, linorder_int) s b &&
                                   check_boundedi bounds sa
                               then Some (g,
   (Internal ac, (r, (list_update la p lb, sa))))
                               else None)))
                         a))
                   in
                 let int_trans_from_vec_impl =
                   (fun pairs l s ->
                     maps (fun (p, la) -> int_trans_from_loc_impl p la l s)
                       pairs)
                   in
                 let int_trans_from_all_impl =
                   (fun l s ->
                     maps (fun p -> int_trans_from_loc_impl p (nth l p) l s)
                       (upt zero_nata n_ps))
                   in
                 let trans_out_map =
                   (fun i j ->
                     map_filter
                       (fun (b, a) ->
                         (let (g, aa) = a in
                          let (ab, (ma, l)) = aa in
                           (match ab with In _ -> None
                             | Out ac -> Some (b, (g, (ac, (ma, l))))
                             | Sil _ -> None)))
                       (trans_mapa i j))
                   in
                 let trans_in_map =
                   (fun i j ->
                     map_filter
                       (fun (b, a) ->
                         (let (g, aa) = a in
                          let (ab, (ma, l)) = aa in
                           (match ab with In ac -> Some (b, (g, (ac, (ma, l))))
                             | Out _ -> None | Sil _ -> None)))
                       (trans_mapa i j))
                   in
                 let trans_out_broad_grouped =
                   (fun i j ->
                     actions_by_statea num_actions
                       (map_filter
                         (fun (b, a) ->
                           (let (g, aa) = a in
                            let (ab, (ma, l)) = aa in
                             (match ab with In _ -> None
                               | Out ac ->
                                 (if membera equal_nat broadcast ac
                                   then Some (b, (g, (ac, (ma, l)))) else None)
                               | Sil _ -> None)))
                         (trans_mapa i j)))
                   in
                 let trans_in_broad_grouped =
                   (fun i j ->
                     actions_by_statea num_actions
                       (map_filter
                         (fun (b, a) ->
                           (let (g, aa) = a in
                            let (ab, (ma, l)) = aa in
                             (match ab
                               with In ac ->
                                 (if membera equal_nat broadcast ac
                                   then Some (b, (g, (ac, (ma, l)))) else None)
                               | Out _ -> None | Sil _ -> None)))
                         (trans_mapa i j)))
                   in
                 let broad_trans_impl =
                   (fun (l, s) ->
                     (let pairs = get_committed broadcast bounds automata l in
                      let ina =
                        map (fun p -> trans_in_broad_grouped p (nth l p))
                          (upt zero_nata n_ps)
                        in
                      let out =
                        map (fun p -> trans_out_broad_grouped p (nth l p))
                          (upt zero_nata n_ps)
                        in
                      let inb =
                        map (map (filter
                                   (fun (b, _) ->
                                     bvali (equal_int, linorder_int) s b)))
                          ina
                        in
                      let outa =
                        map (map (filter
                                   (fun (b, _) ->
                                     bvali (equal_int, linorder_int) s b)))
                          out
                        in
                       (if null pairs
                         then maps (fun a ->
                                     maps (fun p ->
    (let outs = nth (nth outa p) a in
      (if null outs then []
        else (let combs = make_combs broadcast bounds automata p a inb in
              let outsa = map (fun aa -> (p, aa)) outs in
              let combsa =
                (if null combs then map (fun x -> [x]) outsa
                  else maps (fun x -> map (fun aa -> x :: aa) combs) outsa)
                in
              let init = ([], (Broad a, ([], (l, s)))) in
               compute_upds_impl bounds init combsa))))
                                       (upt zero_nata n_ps))
                                (upt zero_nata num_actions)
                         else maps (fun a ->
                                     (let ins_committed =
map_filter
  (fun (p, _) -> (if not (null (nth (nth inb p) a)) then Some p else None))
  pairs
in
                                      let always_committed =
less_nat one_nata (size_list ins_committed) in
                                       maps
 (fun p ->
   (let outs = nth (nth outa p) a in
     (if null outs then []
       else (if not always_committed &&
                  ((equal_lista equal_nat ins_committed [p] ||
                     null ins_committed) &&
                    not (list_ex (fun (q, _) -> equal_nata q p) pairs))
              then []
              else (let combs = make_combs broadcast bounds automata p a inb in
                    let outsa = map (fun aa -> (p, aa)) outs in
                    let combsa =
                      (if null combs then map (fun x -> [x]) outsa
                        else maps (fun x -> map (fun aa -> x :: aa) combs)
                               outsa)
                      in
                    let init = ([], (Broad a, ([], (l, s)))) in
                     compute_upds_impl bounds init combsa)))))
 (upt zero_nata n_ps)))
                                (upt zero_nata num_actions))))
                   in
                 let bin_trans_impl =
                   (fun (l, s) ->
                     (let pairs = get_committed broadcast bounds automata l in
                      let ina =
                        all_actions_by_state broadcast bounds automata
                          num_actions trans_in_map l
                        in
                      let out =
                        all_actions_by_state broadcast bounds automata
                          num_actions trans_out_map l
                        in
                       (if null pairs
                         then maps (fun a ->
                                     pairs_by_action_impl bounds l s (nth out a)
                                       (nth ina a))
                                (bin_actions broadcast num_actions)
                         else (let in2 =
                                 all_actions_from_vec num_actions trans_in_map
                                   pairs
                                 in
                               let out2 =
                                 all_actions_from_vec num_actions trans_out_map
                                   pairs
                                 in
                                maps (fun a ->
                                       pairs_by_action_impl bounds l s
 (nth out a) (nth in2 a))
                                  (bin_actions broadcast num_actions) @
                                  maps (fun a ->
 pairs_by_action_impl bounds l s (nth out2 a) (nth ina a))
                                    (bin_actions broadcast num_actions)))))
                   in
                 let int_trans_impl =
                   (fun (l, s) ->
                     (let pairs = get_committed broadcast bounds automata l in
                       (if null pairs then int_trans_from_all_impl l s
                         else int_trans_from_vec_impl pairs l s)))
                   in
                 let trans_impl =
                   (fun st ->
                     int_trans_impl st @
                       bin_trans_impl st @ broad_trans_impl st)
                   in
                 let e_op_impl =
                   (fun ai bic bib bia bi ->
                     (fun f_ () -> f_
                       ((up_canonical_upd_impl
                          (linordered_cancel_ab_monoid_add_int, heap_int) m bi
                          m)
                       ()) ())
                       (fun x ->
                         (fun f_ () -> f_
                           ((imp_nfoldli (inv_fun ai)
                              (fun _ -> (fun () -> true))
                              (fun aia bid ->
                                (fun f_ () -> f_
                                  ((abstra_upd_impl
                                     (linordered_cancel_ab_monoid_add_int,
                                       uminus_int, equal_int, heap_int)
                                     m aia bid)
                                  ()) ())
                                  (fun xa ->
                                    repair_pair_impl
                                      ((linordered_ab_monoid_add_DBMEntry
 (linordered_cancel_ab_monoid_add_int, equal_int)),
(heap_DBMEntry heap_int))
                                      m xa zero_nata (constraint_clk aia)))
                              x)
                           ()) ())
                           (fun xa ->
                             (fun f_ () -> f_
                               ((check_diag_impla
                                  (linordered_cancel_ab_monoid_add_int,
                                    heap_int)
                                  m m xa)
                               ()) ())
                               (fun xaa ->
                                 (fun f_ () -> f_
                                   ((if xaa
                                      then mtx_set (heap_DBMEntry heap_int)
     (suc m) xa (zero_nata, zero_nata) (Lt zero_inta)
                                      else imp_nfoldli bib
     (fun _ -> (fun () -> true))
     (fun aia bid ->
       (fun f_ () -> f_
         ((abstra_upd_impl
            (linordered_cancel_ab_monoid_add_int, uminus_int, equal_int,
              heap_int)
            m aia bid)
         ()) ())
         (fun xb ->
           repair_pair_impl
             ((linordered_ab_monoid_add_DBMEntry
                (linordered_cancel_ab_monoid_add_int, equal_int)),
               (heap_DBMEntry heap_int))
             m xb zero_nata (constraint_clk aia)))
     xa)
                                   ()) ())
                                   (fun x_a ->
                                     (fun f_ () -> f_
                                       ((check_diag_impla
  (linordered_cancel_ab_monoid_add_int, heap_int) m m x_a)
                                       ()) ())
                                       (fun xb ->
 (fun f_ () -> f_
   ((if xb
      then mtx_set (heap_DBMEntry heap_int) (suc m) x_a (zero_nata, zero_nata)
             (Lt zero_inta)
      else (fun f_ () -> f_
             ((imp_nfoldli bic (fun _ -> (fun () -> true))
                (fun xc sigma ->
                  reset_canonical_upd_impl
                    (linordered_cancel_ab_monoid_add_int, uminus_int, heap_int)
                    m sigma m xc zero_inta)
                x_a)
             ()) ())
             (imp_nfoldli (inv_fun bia) (fun _ -> (fun () -> true))
               (fun aia bid ->
                 (fun f_ () -> f_
                   ((abstra_upd_impl
                      (linordered_cancel_ab_monoid_add_int, uminus_int,
                        equal_int, heap_int)
                      m aia bid)
                   ()) ())
                   (fun xc ->
                     repair_pair_impl
                       ((linordered_ab_monoid_add_DBMEntry
                          (linordered_cancel_ab_monoid_add_int, equal_int)),
                         (heap_DBMEntry heap_int))
                       m xc zero_nata (constraint_clk aia)))))
   ()) ())
   (fun x_b ->
     (fun f_ () -> f_
       ((check_diag_impla (linordered_cancel_ab_monoid_add_int, heap_int) m m
          x_b)
       ()) ())
       (fun x_c ->
         (if x_c
           then mtx_set (heap_DBMEntry heap_int) (suc m) x_b
                  (zero_nata, zero_nata) (Lt zero_inta)
           else (fun f_ () -> f_
                  ((norm_upd_impl (linordered_ab_group_add_int, heap_int) m x_b
                     (let (l, _) = bia in
                       IArray
                         (map (fun c ->
                                maxa linorder_int
                                  (image
                                    (fun i -> sub (sub (sub k_i i) (nth l i)) c)
                                    (Set (upt zero_nata n_ps))))
                           (upt zero_nata (plus_nata m one_nata))))
                     m)
                  ()) ())
                  (fw_impl_int m))))))))))
                   in
                  (fun (a1, a2) ->
                    imp_nfoldli (trans_impl a1) (fun _ -> (fun () -> true))
                      (fun xc sigma ->
                        (let (a1a, (_, (a1c, a2c))) = xc in
                          (fun f_ () -> f_
                            ((amtx_copy (heap_DBMEntry heap_int) a2) ()) ())
                            (fun x ->
                              (fun f_ () -> f_ ((e_op_impl a1 a1c a1a a2c x) ())
                                ())
                                (fun xa ->
                                  (fun () ->
                                    (op_list_prepend (a2c, xa) sigma))))))
                      []))
                 xi)
              ()) ())
              (fun x ->
                (fun f_ () -> f_
                  ((imp_nfoldli x (fun _ -> (fun () -> true))
                     (fun xc sigma ->
                       (fun f_ () -> f_
                         ((let (_, a2) = xc in
                            (fun f_ () -> f_
                              ((check_diag_impl
                                 (linordered_cancel_ab_monoid_add_int, heap_int)
                                 m a2)
                              ()) ())
                              (fun x_c -> (fun () -> (not x_c))))
                         ()) ())
                         (fun x_c ->
                           (fun () ->
                             (if x_c then op_list_prepend xc sigma
                               else sigma))))
                     [])
                  ()) ())
                  (fun xa -> (fun () -> (op_list_rev xa)))))
          in
        let empty =
          (fun (_, a) ->
            check_diag_impl (linordered_cancel_ab_monoid_add_int, heap_int) m a)
          in
        let a =
          tracei (linordered_ab_group_add_int, equal_int, heap_int, show_int) m
            show_state show_clock
          in
         leadsto_impl
           (heap_prod (heap_prod (heap_list heap_nat) (heap_list heap_int))
             (heap_array (typerep_DBMEntry typerep_int)))
           ((equal_prod (equal_list equal_nat) (equal_list equal_int)),
             (hashable_prod (hashable_list hashable_nat)
               (hashable_list hashable_int)),
             (heap_prod (heap_list heap_nat) (heap_list heap_int)))
           copy succs start suba key succsa empty final finala a)
      ()) ())
      (fun r -> (fun () -> (not r)));;

let rec alw_ev_checker
  broadcast bounds automata m num_states num_actions k l_0 s_0 formula =
    (fun f_ () -> f_
      ((let key = comp (fun a -> (fun () -> a)) fst in
        let suba =
          (fun ai bi ->
            (let (a1, a2) = ai in
             let (a1a, a2a) = bi in
              (if equal_proda (equal_list equal_nat) (equal_list equal_int) a1
                    a1a
                then dbm_subset_impl
                       (linordered_cancel_ab_monoid_add_int, equal_int,
                         heap_int)
                       m a2 a2a
                else (fun () -> false))))
          in
        let copy =
          (fun (a1, a2) ->
            (fun f_ () -> f_ ((amtx_copy (heap_DBMEntry heap_int) a2) ()) ())
              (fun x -> (fun () -> (a1, x))))
          in
        let start =
          (fun f_ () -> f_
            ((amtx_dflt (heap_DBMEntry heap_int) (suc m) (suc m) (Le zero_inta))
            ()) ())
            (fun x_a ->
              (fun () ->
                ((l_0, map (comp the (map_of equal_nat s_0))
                         (upt zero_nata (n_vs bounds))),
                  x_a)))
          in
        let succs =
          (fun xi ->
            (fun f_ () -> f_
              (((let n_ps = size_list automata in
                 let k_i =
                   IArray
                     (map (comp (fun a -> IArray a)
                            (map (comp (fun a -> IArray a) (map int_of_nat))))
                       k)
                   in
                 let invs =
                   IArray
                     (map (fun i ->
                            (let ma =
                               default_map_of equal_nat []
                                 (snd (snd (snd (nth automata i))))
                               in
                             let mb =
                               IArray (map ma (upt zero_nata (num_states i))) in
                              mb))
                       (upt zero_nata n_ps))
                   in
                 let inv_fun =
                   (fun (l, _) ->
                     maps (fun i -> sub (sub invs i) (nth l i))
                       (upt zero_nata n_ps))
                   in
                 let trans_mapa = trans_map automata in
                 let trans_i_map =
                   (fun i j ->
                     map_filter
                       (fun (b, a) ->
                         (let (g, aa) = a in
                          let (ab, (ma, l)) = aa in
                           (match ab with In _ -> None | Out _ -> None
                             | Sil ac -> Some (b, (g, (ac, (ma, l)))))))
                       (trans_mapa i j))
                   in
                 let int_trans_from_loc_impl =
                   (fun p l la s ->
                     (let a = trans_i_map p l in
                       map_filter
                         (fun (b, aa) ->
                           (let (g, ab) = aa in
                            let (ac, (f, (r, lb))) = ab in
                            let sa = mk_updsi s f in
                             (if bvali (equal_int, linorder_int) s b &&
                                   check_boundedi bounds sa
                               then Some (g,
   (Internal ac, (r, (list_update la p lb, sa))))
                               else None)))
                         a))
                   in
                 let int_trans_from_vec_impl =
                   (fun pairs l s ->
                     maps (fun (p, la) -> int_trans_from_loc_impl p la l s)
                       pairs)
                   in
                 let int_trans_from_all_impl =
                   (fun l s ->
                     maps (fun p -> int_trans_from_loc_impl p (nth l p) l s)
                       (upt zero_nata n_ps))
                   in
                 let trans_out_map =
                   (fun i j ->
                     map_filter
                       (fun (b, a) ->
                         (let (g, aa) = a in
                          let (ab, (ma, l)) = aa in
                           (match ab with In _ -> None
                             | Out ac -> Some (b, (g, (ac, (ma, l))))
                             | Sil _ -> None)))
                       (trans_mapa i j))
                   in
                 let trans_in_map =
                   (fun i j ->
                     map_filter
                       (fun (b, a) ->
                         (let (g, aa) = a in
                          let (ab, (ma, l)) = aa in
                           (match ab with In ac -> Some (b, (g, (ac, (ma, l))))
                             | Out _ -> None | Sil _ -> None)))
                       (trans_mapa i j))
                   in
                 let trans_out_broad_grouped =
                   (fun i j ->
                     actions_by_statea num_actions
                       (map_filter
                         (fun (b, a) ->
                           (let (g, aa) = a in
                            let (ab, (ma, l)) = aa in
                             (match ab with In _ -> None
                               | Out ac ->
                                 (if membera equal_nat broadcast ac
                                   then Some (b, (g, (ac, (ma, l)))) else None)
                               | Sil _ -> None)))
                         (trans_mapa i j)))
                   in
                 let trans_in_broad_grouped =
                   (fun i j ->
                     actions_by_statea num_actions
                       (map_filter
                         (fun (b, a) ->
                           (let (g, aa) = a in
                            let (ab, (ma, l)) = aa in
                             (match ab
                               with In ac ->
                                 (if membera equal_nat broadcast ac
                                   then Some (b, (g, (ac, (ma, l)))) else None)
                               | Out _ -> None | Sil _ -> None)))
                         (trans_mapa i j)))
                   in
                 let broad_trans_impl =
                   (fun (l, s) ->
                     (let pairs = get_committed broadcast bounds automata l in
                      let ina =
                        map (fun p -> trans_in_broad_grouped p (nth l p))
                          (upt zero_nata n_ps)
                        in
                      let out =
                        map (fun p -> trans_out_broad_grouped p (nth l p))
                          (upt zero_nata n_ps)
                        in
                      let inb =
                        map (map (filter
                                   (fun (b, _) ->
                                     bvali (equal_int, linorder_int) s b)))
                          ina
                        in
                      let outa =
                        map (map (filter
                                   (fun (b, _) ->
                                     bvali (equal_int, linorder_int) s b)))
                          out
                        in
                       (if null pairs
                         then maps (fun a ->
                                     maps (fun p ->
    (let outs = nth (nth outa p) a in
      (if null outs then []
        else (let combs = make_combs broadcast bounds automata p a inb in
              let outsa = map (fun aa -> (p, aa)) outs in
              let combsa =
                (if null combs then map (fun x -> [x]) outsa
                  else maps (fun x -> map (fun aa -> x :: aa) combs) outsa)
                in
              let init = ([], (Broad a, ([], (l, s)))) in
               compute_upds_impl bounds init combsa))))
                                       (upt zero_nata n_ps))
                                (upt zero_nata num_actions)
                         else maps (fun a ->
                                     (let ins_committed =
map_filter
  (fun (p, _) -> (if not (null (nth (nth inb p) a)) then Some p else None))
  pairs
in
                                      let always_committed =
less_nat one_nata (size_list ins_committed) in
                                       maps
 (fun p ->
   (let outs = nth (nth outa p) a in
     (if null outs then []
       else (if not always_committed &&
                  ((equal_lista equal_nat ins_committed [p] ||
                     null ins_committed) &&
                    not (list_ex (fun (q, _) -> equal_nata q p) pairs))
              then []
              else (let combs = make_combs broadcast bounds automata p a inb in
                    let outsa = map (fun aa -> (p, aa)) outs in
                    let combsa =
                      (if null combs then map (fun x -> [x]) outsa
                        else maps (fun x -> map (fun aa -> x :: aa) combs)
                               outsa)
                      in
                    let init = ([], (Broad a, ([], (l, s)))) in
                     compute_upds_impl bounds init combsa)))))
 (upt zero_nata n_ps)))
                                (upt zero_nata num_actions))))
                   in
                 let bin_trans_impl =
                   (fun (l, s) ->
                     (let pairs = get_committed broadcast bounds automata l in
                      let ina =
                        all_actions_by_state broadcast bounds automata
                          num_actions trans_in_map l
                        in
                      let out =
                        all_actions_by_state broadcast bounds automata
                          num_actions trans_out_map l
                        in
                       (if null pairs
                         then maps (fun a ->
                                     pairs_by_action_impl bounds l s (nth out a)
                                       (nth ina a))
                                (bin_actions broadcast num_actions)
                         else (let in2 =
                                 all_actions_from_vec num_actions trans_in_map
                                   pairs
                                 in
                               let out2 =
                                 all_actions_from_vec num_actions trans_out_map
                                   pairs
                                 in
                                maps (fun a ->
                                       pairs_by_action_impl bounds l s
 (nth out a) (nth in2 a))
                                  (bin_actions broadcast num_actions) @
                                  maps (fun a ->
 pairs_by_action_impl bounds l s (nth out2 a) (nth ina a))
                                    (bin_actions broadcast num_actions)))))
                   in
                 let int_trans_impl =
                   (fun (l, s) ->
                     (let pairs = get_committed broadcast bounds automata l in
                       (if null pairs then int_trans_from_all_impl l s
                         else int_trans_from_vec_impl pairs l s)))
                   in
                 let trans_impl =
                   (fun st ->
                     int_trans_impl st @
                       bin_trans_impl st @ broad_trans_impl st)
                   in
                 let e_op_impl =
                   (fun ai bic bib bia bi ->
                     (fun f_ () -> f_
                       ((up_canonical_upd_impl
                          (linordered_cancel_ab_monoid_add_int, heap_int) m bi
                          m)
                       ()) ())
                       (fun x ->
                         (fun f_ () -> f_
                           ((imp_nfoldli (inv_fun ai)
                              (fun _ -> (fun () -> true))
                              (fun aia bid ->
                                (fun f_ () -> f_
                                  ((abstra_upd_impl
                                     (linordered_cancel_ab_monoid_add_int,
                                       uminus_int, equal_int, heap_int)
                                     m aia bid)
                                  ()) ())
                                  (fun xa ->
                                    repair_pair_impl
                                      ((linordered_ab_monoid_add_DBMEntry
 (linordered_cancel_ab_monoid_add_int, equal_int)),
(heap_DBMEntry heap_int))
                                      m xa zero_nata (constraint_clk aia)))
                              x)
                           ()) ())
                           (fun xa ->
                             (fun f_ () -> f_
                               ((check_diag_impla
                                  (linordered_cancel_ab_monoid_add_int,
                                    heap_int)
                                  m m xa)
                               ()) ())
                               (fun xaa ->
                                 (fun f_ () -> f_
                                   ((if xaa
                                      then mtx_set (heap_DBMEntry heap_int)
     (suc m) xa (zero_nata, zero_nata) (Lt zero_inta)
                                      else imp_nfoldli bib
     (fun _ -> (fun () -> true))
     (fun aia bid ->
       (fun f_ () -> f_
         ((abstra_upd_impl
            (linordered_cancel_ab_monoid_add_int, uminus_int, equal_int,
              heap_int)
            m aia bid)
         ()) ())
         (fun xb ->
           repair_pair_impl
             ((linordered_ab_monoid_add_DBMEntry
                (linordered_cancel_ab_monoid_add_int, equal_int)),
               (heap_DBMEntry heap_int))
             m xb zero_nata (constraint_clk aia)))
     xa)
                                   ()) ())
                                   (fun x_a ->
                                     (fun f_ () -> f_
                                       ((check_diag_impla
  (linordered_cancel_ab_monoid_add_int, heap_int) m m x_a)
                                       ()) ())
                                       (fun xb ->
 (fun f_ () -> f_
   ((if xb
      then mtx_set (heap_DBMEntry heap_int) (suc m) x_a (zero_nata, zero_nata)
             (Lt zero_inta)
      else (fun f_ () -> f_
             ((imp_nfoldli bic (fun _ -> (fun () -> true))
                (fun xc sigma ->
                  reset_canonical_upd_impl
                    (linordered_cancel_ab_monoid_add_int, uminus_int, heap_int)
                    m sigma m xc zero_inta)
                x_a)
             ()) ())
             (imp_nfoldli (inv_fun bia) (fun _ -> (fun () -> true))
               (fun aia bid ->
                 (fun f_ () -> f_
                   ((abstra_upd_impl
                      (linordered_cancel_ab_monoid_add_int, uminus_int,
                        equal_int, heap_int)
                      m aia bid)
                   ()) ())
                   (fun xc ->
                     repair_pair_impl
                       ((linordered_ab_monoid_add_DBMEntry
                          (linordered_cancel_ab_monoid_add_int, equal_int)),
                         (heap_DBMEntry heap_int))
                       m xc zero_nata (constraint_clk aia)))))
   ()) ())
   (fun x_b ->
     (fun f_ () -> f_
       ((check_diag_impla (linordered_cancel_ab_monoid_add_int, heap_int) m m
          x_b)
       ()) ())
       (fun x_c ->
         (if x_c
           then mtx_set (heap_DBMEntry heap_int) (suc m) x_b
                  (zero_nata, zero_nata) (Lt zero_inta)
           else (fun f_ () -> f_
                  ((norm_upd_impl (linordered_ab_group_add_int, heap_int) m x_b
                     (let (l, _) = bia in
                       IArray
                         (map (fun c ->
                                maxa linorder_int
                                  (image
                                    (fun i -> sub (sub (sub k_i i) (nth l i)) c)
                                    (Set (upt zero_nata n_ps))))
                           (upt zero_nata (plus_nata m one_nata))))
                     m)
                  ()) ())
                  (fw_impl_int m))))))))))
                   in
                  (fun (a1, a2) ->
                    imp_nfoldli (trans_impl a1) (fun _ -> (fun () -> true))
                      (fun xc sigma ->
                        (let (a1a, (_, (a1c, a2c))) = xc in
                          (if (let (a, b) = a2c in
                                hd_of_formulai equal_nat formula a b)
                            then (fun f_ () -> f_
                                   ((amtx_copy (heap_DBMEntry heap_int) a2) ())
                                   ())
                                   (fun x ->
                                     (fun f_ () -> f_
                                       ((e_op_impl a1 a1c a1a a2c x) ()) ())
                                       (fun xa ->
 (fun () -> (op_list_prepend (a2c, xa) sigma))))
                            else (fun () -> sigma))))
                      []))
                 xi)
              ()) ())
              (fun x ->
                (fun f_ () -> f_
                  ((imp_nfoldli x (fun _ -> (fun () -> true))
                     (fun xc sigma ->
                       (fun f_ () -> f_
                         ((let (_, a2) = xc in
                            (fun f_ () -> f_
                              ((check_diag_impl
                                 (linordered_cancel_ab_monoid_add_int, heap_int)
                                 m a2)
                              ()) ())
                              (fun x_c -> (fun () -> (not x_c))))
                         ()) ())
                         (fun x_c ->
                           (fun () ->
                             (if x_c then op_list_prepend xc sigma
                               else sigma))))
                     [])
                  ()) ())
                  (fun xa -> (fun () -> (op_list_rev xa)))))
          in
         dfs_map_impl
           (heap_prod (heap_prod (heap_list heap_nat) (heap_list heap_int))
             (heap_array (typerep_DBMEntry typerep_int)))
           ((equal_prod (equal_list equal_nat) (equal_list equal_int)),
             (hashable_prod (hashable_list hashable_nat)
               (hashable_list hashable_int)),
             (heap_prod (heap_list heap_nat) (heap_list heap_int)))
           succs start suba key copy)
      ()) ())
      (fun x ->
        (fun f_ () -> f_ ((fun () -> ()) ()) ()) (fun _ -> (fun () -> x)));;

let rec check_sexp _A (_C1, _C2)
  x0 uu uv = match x0, uu, uv with Truea, uu, uv -> true
    | Nota e, l, s -> not (check_sexp _A (_C1, _C2) e l s)
    | Anda (e1, e2), l, s ->
        check_sexp _A (_C1, _C2) e1 l s && check_sexp _A (_C1, _C2) e2 l s
    | Ora (e1, e2), l, s ->
        check_sexp _A (_C1, _C2) e1 l s || check_sexp _A (_C1, _C2) e2 l s
    | Implya (e1, e2), l, s ->
        (if check_sexp _A (_C1, _C2) e1 l s then check_sexp _A (_C1, _C2) e2 l s
          else true)
    | Eqa (i, x), l, s -> eq _C1 (s i) x
    | Leb (i, x), l, s ->
        less_eq _C2.order_linorder.preorder_order.ord_preorder (s i) x
    | Ltb (i, x), l, s ->
        less _C2.order_linorder.preorder_order.ord_preorder (s i) x
    | Gea (i, x), l, s ->
        less_eq _C2.order_linorder.preorder_order.ord_preorder x (s i)
    | Gta (i, x), l, s ->
        less _C2.order_linorder.preorder_order.ord_preorder x (s i)
    | Loc (i, x), l, s -> eq _A (nth l i) x;;

let rec hd_of_formula _A (_C1, _C2)
  x0 l s = match x0, l, s with EX phi, l, s -> check_sexp _A (_C1, _C2) phi l s
    | EG phi, l, s -> check_sexp _A (_C1, _C2) phi l s
    | AX phi, l, s -> not (check_sexp _A (_C1, _C2) phi l s)
    | AG phi, l, s -> not (check_sexp _A (_C1, _C2) phi l s)
    | Leadsto (phi, uu), l, s -> check_sexp _A (_C1, _C2) phi l s;;

let rec model_checker
  broadcast bounds automata m num_states num_actions k l_0 s_0 formula
    show_clock show_state =
    (match formula
      with EX _ ->
        reachability_checker broadcast bounds automata m num_states num_actions
          k l_0 s_0 formula show_clock show_state
      | EG _ ->
        (if hd_of_formula equal_nat (equal_int, linorder_int) formula l_0
              (comp the (map_of equal_nat s_0))
          then alw_ev_checker broadcast bounds automata m num_states num_actions
                 k l_0 s_0 formula
          else (fun () -> false))
      | AX _ ->
        (fun f_ () -> f_
          ((if hd_of_formula equal_nat (equal_int, linorder_int) formula l_0
                 (comp the (map_of equal_nat s_0))
             then alw_ev_checker broadcast bounds automata m num_states
                    num_actions k l_0 s_0 formula
             else (fun () -> false))
          ()) ())
          (fun r -> (fun () -> (not r)))
      | AG _ ->
        (fun f_ () -> f_
          ((reachability_checker broadcast bounds automata m num_states
             num_actions k l_0 s_0 formula show_clock show_state)
          ()) ())
          (fun r -> (fun () -> (not r)))
      | Leadsto (_, psi) ->
        leadsto_checker broadcast bounds automata m num_states num_actions k l_0
          s_0 formula psi show_clock show_state);;

let rec precond_mc
  show_clock show_state broadcast bounds automata m num_states num_actions k l_0
    s_0 formula =
    (if simple_Network_Impl_nat_ceiling_start_state broadcast bounds automata m
          num_states num_actions k l_0 s_0 formula
      then (fun f_ () -> f_
             ((model_checker broadcast bounds automata m num_states num_actions
                k l_0 s_0 formula show_clock show_state)
             ()) ())
             (fun x -> (fun () -> (Some x)))
      else (fun () -> None));;

let rec set2_sexp _B
  = function Truea -> bot_set
    | Nota x2 -> set2_sexp _B x2
    | Anda (x31, x32) -> sup_set _B (set2_sexp _B x31) (set2_sexp _B x32)
    | Ora (x41, x42) -> sup_set _B (set2_sexp _B x41) (set2_sexp _B x42)
    | Implya (x51, x52) -> sup_set _B (set2_sexp _B x51) (set2_sexp _B x52)
    | Eqa (x61, x62) -> bot_set
    | Leb (x71, x72) -> bot_set
    | Ltb (x81, x82) -> bot_set
    | Gea (x91, x92) -> bot_set
    | Gta (x101, x102) -> bot_set
    | Loc (x111, x112) -> insert _B x112 bot_set;;

let rec set2_formula _B
  = function EX x1 -> set2_sexp _B x1
    | EG x2 -> set2_sexp _B x2
    | AX x3 -> set2_sexp _B x3
    | AG x4 -> set2_sexp _B x4
    | Leadsto (x51, x52) -> sup_set _B (set2_sexp _B x51) (set2_sexp _B x52);;

let rec clkp_set _C
  automata =
    sup_set (equal_prod _C equal_int)
      (sup_seta (equal_prod _C equal_int)
        (image
          (fun a ->
            sup_seta (equal_prod _C equal_int)
              (image (comp collect_clock_pairs snd) (Set (snd (snd (snd a))))))
          (Set automata)))
      (sup_seta (equal_prod _C equal_int)
        (image
          (fun a ->
            sup_seta (equal_prod _C equal_int)
              (image (fun (_, (_, (g, _))) -> collect_clock_pairs g)
                (Set (fst (snd (snd a))))))
          (Set automata)));;

let rec clk_set _C
  automata =
    sup_set _C (image fst (clkp_set _C automata))
      (sup_seta _C
        (image
          (fun a ->
            sup_seta _C
              (image (fun (_, (_, (_, (_, (_, (r, _)))))) -> Set r)
                (Set (fst (snd (snd a))))))
          (Set automata)));;

let rec check_renaming
  broadcast bounds renum_acts renum_vars renum_clocks renum_states automata urge
    phi l_0 s_0 =
    combine
      [asserta
         (all_interval_nat
           (fun i ->
             ball (sup_seta equal_nat
                    (image
                      (fun (_, (_, (t, _))) ->
                        sup_seta equal_nat
                          (image
                            (fun (l, (_, (_, (_, (_, (_, la)))))) ->
                              insert equal_nat l (insert equal_nat la bot_set))
                            (Set t)))
                      (Set automata)))
               (fun x ->
                 ball (sup_seta equal_nat
                        (image
                          (fun (_, (_, (t, _))) ->
                            sup_seta equal_nat
                              (image
                                (fun (l, (_, (_, (_, (_, (_, la)))))) ->
                                  insert equal_nat l
                                    (insert equal_nat la bot_set))
                                (Set t)))
                          (Set automata)))
                   (fun y ->
                     (if equal_nata (renum_states i x) (renum_states i y)
                       then equal_nata x y else true))))
           zero_nata (size_list automata))
         "Location renamings are injective";
        asserta
          (inj_on equal_literal equal_nat renum_clocks
            (insert equal_literal urge (clk_set equal_literal automata)))
          "Clock renaming is injective";
        asserta
          (inj_on equal_literal equal_nat renum_vars
            (sup_set equal_literal
              (sup_seta equal_literal
                (image
                  (fun s ->
                    sup_seta equal_literal
                      (image (vars_of_bexp equal_literal) s))
                  (image (fun t -> image (comp fst snd) (Set t))
                    (image (fun (_, (_, (t, _))) -> t) (Set automata)))))
              (sup_seta equal_literal
                (image
                  (fun s ->
                    sup_seta equal_literal
                      (image
                        (fun f ->
                          sup_seta equal_literal
                            (image
                              (fun (x, e) ->
                                sup_set equal_literal
                                  (insert equal_literal x bot_set)
                                  (vars_of_exp equal_literal e))
                              (Set f)))
                        s))
                  (image
                    (fun t ->
                      image (comp (comp (comp (comp fst snd) snd) snd) snd)
                        (Set t))
                    (image (fun (_, (_, (t, _))) -> t) (Set automata)))))))
          "Variable renaming is injective";
        asserta
          (inj_on equal_literal equal_nat renum_acts
            (sup_set equal_literal
              (sup_seta equal_literal
                (image
                  (fun (_, (_, (t, _))) ->
                    sup_seta equal_literal
                      (image
                        (fun (_, a) ->
                          (let (_, aa) = a in
                           let (_, ab) = aa in
                           let (ac, _) = ab in
                            set_act equal_literal ac))
                        (Set t)))
                  (Set automata)))
              (Set broadcast)))
          "Action renaming is injective";
        asserta
          (eq_set (card_UNIV_literal, equal_literal) (image fst (Set bounds))
            (sup_set equal_literal
              (sup_seta equal_literal
                (image
                  (fun s ->
                    sup_seta equal_literal
                      (image (vars_of_bexp equal_literal) s))
                  (image (fun t -> image (comp fst snd) (Set t))
                    (image (fun (_, (_, (t, _))) -> t) (Set automata)))))
              (sup_seta equal_literal
                (image
                  (fun s ->
                    sup_seta equal_literal
                      (image
                        (fun f ->
                          sup_seta equal_literal
                            (image
                              (fun (x, e) ->
                                sup_set equal_literal
                                  (insert equal_literal x bot_set)
                                  (vars_of_exp equal_literal e))
                              (Set f)))
                        s))
                  (image
                    (fun t ->
                      image (comp (comp (comp (comp fst snd) snd) snd) snd)
                        (Set t))
                    (image (fun (_, (_, (t, _))) -> t) (Set automata)))))))
          "Bound set is exactly the variable set";
        asserta
          (subset (card_UNIV_nat, equal_nat)
            (sup_seta equal_nat
              (image (fun g -> image fst (Set g))
                (Set (map (comp (comp snd snd) snd) automata))))
            (sup_seta equal_nat
              (image
                (fun (_, (_, (t, _))) ->
                  sup_seta equal_nat
                    (image
                      (fun (l, (_, (_, (_, (_, (_, la)))))) ->
                        insert equal_nat l (insert equal_nat la bot_set))
                      (Set t)))
                (Set automata))))
          "Invariant locations are contained in the location set";
        asserta
          (subset (card_UNIV_nat, equal_nat)
            (sup_seta equal_nat
              (image (comp (fun a -> Set a) fst) (Set automata)))
            (sup_seta equal_nat
              (image
                (fun (_, (_, (t, _))) ->
                  sup_seta equal_nat
                    (image
                      (fun (l, (_, (_, (_, (_, (_, la)))))) ->
                        insert equal_nat l (insert equal_nat la bot_set))
                      (Set t)))
                (Set automata))))
          "Broadcast locations are containted in the location set";
        asserta
          (subset (card_UNIV_nat, equal_nat)
            (sup_seta equal_nat
              (image (fun x -> Set (fst (snd x))) (Set automata)))
            (sup_seta equal_nat
              (image
                (fun (_, (_, (t, _))) ->
                  sup_seta equal_nat
                    (image
                      (fun (l, (_, (_, (_, (_, (_, la)))))) ->
                        insert equal_nat l (insert equal_nat la bot_set))
                      (Set t)))
                (Set automata))))
          "Urgent locations are containted in the location set";
        asserta
          (not (member equal_literal urge (clk_set equal_literal automata)))
          "Urge not in clock set";
        asserta
          (equal_nata (size_list l_0) (size_list automata) &&
            all_interval_nat
              (fun i ->
                bex (fst (snd (snd (nth (fst
  (snd (Set broadcast,
         (map (automaton_of equal_nat) automata, map_of equal_literal bounds))))
                                     i))))
                  (fun (l, (_, (_, (_, (_, (_, la)))))) ->
                    equal_nata (nth l_0 i) l || equal_nata (nth l_0 i) la))
              zero_nata (size_list automata))
          "Initial location is in the state set";
        asserta
          (eq_set (card_UNIV_literal, equal_literal) (image fst (Set s_0))
            (sup_set equal_literal
              (sup_seta equal_literal
                (image
                  (fun s ->
                    sup_seta equal_literal
                      (image (vars_of_bexp equal_literal) s))
                  (image (fun t -> image (comp fst snd) (Set t))
                    (image (fun (_, (_, (t, _))) -> t) (Set automata)))))
              (sup_seta equal_literal
                (image
                  (fun s ->
                    sup_seta equal_literal
                      (image
                        (fun f ->
                          sup_seta equal_literal
                            (image
                              (fun (x, e) ->
                                sup_set equal_literal
                                  (insert equal_literal x bot_set)
                                  (vars_of_exp equal_literal e))
                              (Set f)))
                        s))
                  (image
                    (fun t ->
                      image (comp (comp (comp (comp fst snd) snd) snd) snd)
                        (Set t))
                    (image (fun (_, (_, (t, _))) -> t) (Set automata)))))))
          "Initial state has the correct domain";
        asserta (distinct equal_literal (map fst s_0))
          "Initial state is unambiguous";
        asserta
          (subset (card_UNIV_nat, equal_nat) (set2_formula equal_nat phi)
            (sup_seta equal_nat
              (image
                (fun (_, (_, (t, _))) ->
                  sup_seta equal_nat
                    (image
                      (fun (l, (_, (_, (_, (_, (_, la)))))) ->
                        insert equal_nat l (insert equal_nat la bot_set))
                      (Set t)))
                (Set automata))))
          "Formula locations are contained in the location set";
        asserta
          (subset (card_UNIV_nat, equal_nat) (locs_of_formula equal_nat phi)
            (Set (upt zero_nata (size_list automata))))
          "Formula automata are contained in the automata set";
        asserta
          (subset (card_UNIV_literal, equal_literal)
            (vars_of_formula equal_literal phi)
            (sup_set equal_literal
              (sup_seta equal_literal
                (image
                  (fun s ->
                    sup_seta equal_literal
                      (image (vars_of_bexp equal_literal) s))
                  (image (fun t -> image (comp fst snd) (Set t))
                    (image (fun (_, (_, (t, _))) -> t) (Set automata)))))
              (sup_seta equal_literal
                (image
                  (fun s ->
                    sup_seta equal_literal
                      (image
                        (fun f ->
                          sup_seta equal_literal
                            (image
                              (fun (x, e) ->
                                sup_set equal_literal
                                  (insert equal_literal x bot_set)
                                  (vars_of_exp equal_literal e))
                              (Set f)))
                        s))
                  (image
                    (fun t ->
                      image (comp (comp (comp (comp fst snd) snd) snd) snd)
                        (Set t))
                    (image (fun (_, (_, (t, _))) -> t) (Set automata)))))))
          "Variables of the formula are contained in the variable set"];;

let rec check_precond2
  broadcast bounds automata m num_states k l_0 s_0 formula =
    combine
      [asserta
         (all_interval_nat
           (fun i ->
             list_all
               (fun (l, g) ->
                 ball (collect_clock_pairs g)
                   (fun (x, ma) ->
                     less_eq_int ma (int_of_nat (nth (nth (nth k i) l) x))))
               (comp (comp snd snd) snd (nth automata i)))
           zero_nata (size_list automata))
         "Ceiling invariants";
        asserta
          (all_interval_nat
            (fun i ->
              list_all
                (fun (l, (_, (g, _))) ->
                  ball (collect_clock_pairs g)
                    (fun (x, ma) ->
                      less_eq_int ma (int_of_nat (nth (nth (nth k i) l) x))))
                (comp (comp fst snd) snd (nth automata i)))
            zero_nata (size_list automata))
          "Ceiling transitions";
        asserta
          (all_interval_nat
            (fun i ->
              list_all
                (fun (l, (_, (_, (_, (_, (r, la)))))) ->
                  ball (minus_set equal_nat
                         (Set (upt zero_nata (plus_nata m one_nata))) (Set r))
                    (fun c ->
                      less_eq_nat (nth (nth (nth k i) la) c)
                        (nth (nth (nth k i) l) c)))
                (comp (comp fst snd) snd (nth automata i)))
            zero_nata (size_list automata))
          "Ceiling resets";
        asserta (equal_nata (size_list k) (size_list automata))
          "Ceiling length";
        asserta
          (all_interval_nat
            (fun i -> equal_nata (size_list (nth k i)) (num_states i)) zero_nata
            (size_list automata))
          "Ceiling length automata)";
        asserta
          (list_all
            (list_all
              (fun xxs -> equal_nata (size_list xxs) (plus_nata m one_nata)))
            k)
          "Ceiling length clocks";
        asserta
          (all_interval_nat
            (fun i ->
              all_interval_nat
                (fun l ->
                  equal_nata (nth (nth (nth k i) l) zero_nata) zero_nata)
                zero_nata (num_states i))
            zero_nata (size_list automata))
          "Ceiling zero clock";
        asserta
          (list_all (fun (_, (_, (_, inv))) -> distinct equal_nat (map fst inv))
            automata)
          "Unambiguous invariants";
        asserta
          (eq_set (card_UNIV_nat, equal_nat) (image fst (Set s_0))
             (image fst (Set bounds)) &&
            ball (image fst (Set s_0))
              (fun x ->
                less_eq_int (fst (the (map_of equal_nat bounds x)))
                  (the (map_of equal_nat s_0 x)) &&
                  less_eq_int (the (map_of equal_nat s_0 x))
                    (snd (the (map_of equal_nat bounds x)))))
          "Initial state bounded";
        asserta (equal_nata (size_list l_0) (size_list automata))
          "Length of initial state";
        asserta
          (all_interval_nat
            (fun i ->
              member equal_nat (nth l_0 i)
                (image fst (Set (comp (comp fst snd) snd (nth automata i)))))
            zero_nata (size_list automata))
          "Initial state has outgoing transitions";
        asserta
          (subset (card_UNIV_nat, equal_nat) (vars_of_formula equal_nat formula)
            (Set (upt zero_nata (n_vs bounds))))
          "Variable set of formula"];;

let rec check_precond1
  broadcast bounds automata m num_states num_actions =
    combine
      [asserta (less_nat zero_nata m) "At least one clock";
        asserta (less_nat zero_nata (size_list automata))
          "At least one automaton";
        asserta
          (all_interval_nat
            (fun i ->
              (let (_, (_, (trans, _))) = nth automata i in
                list_all
                  (fun (l, (_, (_, (_, (_, (_, la)))))) ->
                    less_nat l (num_states i) && less_nat la (num_states i))
                  trans))
            zero_nata (size_list automata))
          "Number of states is correct (transitions)";
        asserta
          (all_interval_nat
            (fun i ->
              (let a = nth automata i in
               let (_, aa) = a in
               let (_, ab) = aa in
               let (_, ac) = ab in
                list_all (fun (x, _) -> less_nat x (num_states i)) ac))
            zero_nata (size_list automata))
          "Number of states is correct (invariants)";
        asserta
          (list_all
            (fun (_, (_, (trans, _))) ->
              list_all
                (fun (_, (_, (_, (_, (f, (_, _)))))) ->
                  list_all
                    (fun (x, upd) ->
                      less_nat x (n_vs bounds) &&
                        ball (vars_of_exp equal_nat upd)
                          (fun i -> less_nat i (n_vs bounds)))
                    f)
                trans)
            automata)
          "Variable set bounded (updates)";
        asserta
          (list_all
            (fun (_, (_, (trans, _))) ->
              list_all
                (fun (_, (b, (_, (_, (_, (_, _)))))) ->
                  ball (vars_of_bexp equal_nat b)
                    (fun i -> less_nat i (n_vs bounds)))
                trans)
            automata)
          "Variable set bounded (guards)";
        asserta
          (all_interval_nat (fun i -> equal_nata (fst (nth bounds i)) i)
            zero_nata (n_vs bounds))
          "Bounds first index";
        asserta (list_all (fun a -> less_nat a num_actions) broadcast)
          "Broadcast actions bounded";
        asserta
          (list_all
            (fun (_, (_, (trans, _))) ->
              list_all
                (fun (_, a) ->
                  (let (_, aa) = a in
                   let (_, ab) = aa in
                   let (ac, (_, (_, _))) = ab in
                    pred_act equal_nat (fun ad -> less_nat ad num_actions) ac))
                trans)
            automata)
          "Actions bounded (transitions)";
        asserta
          (list_all
            (fun (_, (_, (trans, _))) ->
              list_all
                (fun (_, (_, (g, (_, (_, (r, _)))))) ->
                  list_all (fun c -> less_nat zero_nata c && less_eq_nat c m)
                    r &&
                    ball (collect_clock_pairs g)
                      (fun (c, x) ->
                        less_nat zero_nata c &&
                          (less_eq_nat c m && less_eq_int zero_inta x)))
                trans)
            automata)
          "Clock set bounded (transitions)";
        asserta
          (list_all
            (fun (_, a) ->
              (let (_, aa) = a in
               let (_, ab) = aa in
                list_all
                  (fun (_, g) ->
                    ball (collect_clock_pairs g)
                      (fun (c, x) ->
                        less_nat zero_nata c &&
                          (less_eq_nat c m && less_eq_int zero_inta x)))
                  ab))
            automata)
          "Clock set bounded (invariants)";
        asserta
          (list_all
            (fun (_, (_, (trans, _))) ->
              list_all
                (fun (_, a) ->
                  (let (_, aa) = a in
                   let (g, ab) = aa in
                   let (ac, (_, (_, _))) = ab in
                    (match ac
                      with In ad ->
                        (if membera equal_nat broadcast ad then null g
                          else true)
                      | Out _ -> true | Sil _ -> true)))
                trans)
            automata)
          "Broadcast receivers are unguarded";
        asserta (list_all (fun (_, (u, (_, _))) -> null u) automata)
          "Urgency was removed"];;

let rec check_precond
  broadcast bounds automata m num_states num_actions k l_0 s_0 formula =
    combine2 (check_precond1 broadcast bounds automata m num_states num_actions)
      (check_precond2 broadcast bounds automata m num_states k l_0 s_0
        formula);;

let rec map_cconstraint f g xs = map (map_acconstraint f g) xs;;

let rec renum_cconstraint _A renum_clocks = map_cconstraint renum_clocks id;;

let rec renum_reset _A renum_clocks = map renum_clocks;;

let rec renum_bexp _A renum_vars = map_bexp renum_vars;;

let rec renum_exp _A renum_vars = map_exp renum_vars;;

let rec renum_upd _A
  renum_vars =
    map (fun (x, upd) -> (renum_vars x, renum_exp _A renum_vars upd));;

let rec renum_act _A renum_acts = map_act renum_acts;;

let rec renum_automaton _A _B _C _D
  renum_acts renum_vars renum_clocks renum_states i =
    (fun (committed, (urgent, (trans, inv))) ->
      (let committeda = map (renum_states i) committed in
       let urgenta = map (renum_states i) urgent in
       let transa =
         map (fun (l, a) ->
               (let (b, aa) = a in
                let (g, ab) = aa in
                let (ac, (upd, (r, la))) = ab in
                 (renum_states i l,
                   (renum_bexp _B renum_vars b,
                     (renum_cconstraint _C renum_clocks g,
                       (renum_act _A renum_acts ac,
                         (renum_upd _B renum_vars upd,
                           (renum_reset _C renum_clocks r,
                             renum_states i la))))))))
           trans
         in
       let inva =
         map (fun (l, g) ->
               (renum_states i l, renum_cconstraint _C renum_clocks g))
           inv
         in
        (committeda, (urgenta, (transa, inva)))));;

let rec rename_network _A _B _E _G
  broadcast bounds automata renum_acts renum_vars renum_clocks renum_states =
    (let automataa =
       map_index zero_nata
         (renum_automaton _A _B _G _E renum_acts renum_vars renum_clocks
           renum_states)
         automata
       in
     let broadcasta = map renum_acts broadcast in
     let boundsa =
       map (fun (a, b) -> (let (ba, c) = b in (renum_vars a, (ba, c)))) bounds
       in
      (broadcasta, (automataa, boundsa)));;

let rec show_vars _A _B
  inv_renum_vars =
    comp (fun x -> shows_prec_list (show_list show_char) zero_nata x [])
      (map_index zero_nata
        (fun i v ->
          shows_prec _A zero_nata (inv_renum_vars i) [] @
            [Chara (true, false, true, true, true, true, false, false)] @
              shows_prec _B zero_nata v []));;

let rec show_locs _B
  inv_renum_states =
    comp (fun x -> shows_prec_list _B zero_nata x [])
      (map_index zero_nata inv_renum_states);;

let rec show_state _B _C _D
  inv_renum_states inv_renum_vars =
    (fun (l, vs) ->
      (let la = show_locs _B inv_renum_states l in
       let vsa = show_vars _C _D inv_renum_vars vs in
        [Chara (false, false, true, true, true, true, false, false)] @
          la @ [Chara (false, true, true, true, true, true, false, false);
                 Chara (false, false, true, true, false, true, false, false);
                 Chara (false, false, false, false, false, true, false, false);
                 Chara (false, false, true, true, true, true, false, false)] @
                 vsa @ [Chara (false, true, true, true, true, true, false,
                                false)]));;

let rec do_rename_mc _C _E _F _G
  f dc broadcast bounds automata k urge l_0 s_0 formula m num_states num_actions
    renum_acts renum_vars renum_clocks renum_states inv_renum_states
    inv_renum_vars inv_renum_clocks =
    (let _ = print_string "Checking renaming" in
     let formulaa = (if dc then EX (Nota Truea) else formula) in
     let renaming_valid =
       check_renaming broadcast bounds renum_acts renum_vars renum_clocks
         renum_states automata urge formulaa l_0 s_0
       in
     let _ = print_string "Renaming network" in
     let (broadcasta, (automataa, boundsa)) =
       rename_network countable_literal countable_literal countable_nat
         countable_literal broadcast bounds
         (map (conv_urge equal_nat zero_int urge) automata) renum_acts
         renum_vars renum_clocks renum_states
       in
     let _ =
       trace_level (Int_of_integer (Z.Int.of_int 4))
         (fun _ -> (fun () -> "Automata after renaming"))
       in
     let _ =
       map (fun a ->
             trace_level (Int_of_integer (Z.Int.of_int 4))
               (fun _ ->
                 (fun () ->
                   (implode
                     (shows_prec_prod (show_list show_nat)
                       (show_prod (show_list show_nat)
                         (show_prod
                           (show_list
                             (show_prod show_nat
                               (show_prod (show_bexp show_nat show_int)
                                 (show_prod
                                   (show_list
                                     (show_acconstraint show_nat show_int))
                                   (show_prod (show_act show_nat)
                                     (show_prod
                                       (show_list
 (show_prod show_nat (show_exp show_nat show_int)))
                                       (show_prod (show_list show_nat)
 show_nat)))))))
                           (show_list
                             (show_prod show_nat
                               (show_list
                                 (show_acconstraint show_nat show_int))))))
                       zero_nata a [])))))
         automataa
       in
     let _ = print_string "Renaming formula" in
     let formulab =
       (if dc then EX (Nota Truea)
         else map_formula renum_states renum_vars id formulaa)
       in
     let _ = print_string "Renaming state" in
     let l_0a = map_index zero_nata renum_states l_0 in
     let s_0a = map (fun (x, a) -> (renum_vars x, a)) s_0 in
     let show_clock =
       comp (fun x -> shows_prec _G zero_nata x []) inv_renum_clocks in
     let show_statea = show_state _E _F _C inv_renum_states inv_renum_vars in
      (if is_result renaming_valid
        then (let _ = print_string "Checking preconditions" in
              let r =
                check_precond broadcasta boundsa automataa m num_states
                  num_actions k l_0a s_0a formulab
                in
              let _ =
                (match r with Result _ -> ()
                  | Error es ->
                    (let _ = print_string "" in
                     let _ =
                       print_string "The following pre-conditions were not satisified:"
                       in
                     let _ = map (fun a -> print_string a) es in
                      print_string ""))
                in
              let _ = print_string "Running precond_mc" in
              let a =
                f show_clock show_statea broadcasta boundsa automataa m
                  num_states
                  num_actions
                  k
                  l_0a
                  s_0a
                  formulab
                in
               Some a)
        else (let _ =
                print_string "The following conditions on the renaming were not satisfied:"
                in
              let _ = map (fun a -> print_string a) (the_errors renaming_valid)
                in
               None)));;

let rec rename_mc _A _B _C
  dc broadcast bounds automata k urge l_0 s_0 formula m num_states num_actions
    renum_acts renum_vars renum_clocks renum_states inv_renum_states
    inv_renum_vars inv_renum_clocks =
    (match
      do_rename_mc show_int _A _B _C (if dc then precond_dc else precond_mc) dc
        broadcast bounds automata k urge l_0 s_0 formula m num_states
        num_actions renum_acts renum_vars renum_clocks renum_states
        inv_renum_states inv_renum_vars inv_renum_clocks
      with None -> (fun () -> Renaming_Failed)
      | Some r ->
        (fun f_ () -> f_ (r ()) ())
          (fun a ->
            (match a with None -> (fun () -> Preconds_Unsat)
              | Some true -> (fun () -> Sat)
              | Some false -> (fun () -> Unsat))));;

let rec shows_rat
  r = (let Rata (s, f, b) = r in
        (if s then []
          else [Chara (true, false, true, true, false, true, false, false)]) @
          shows_prec_int zero_nata f [] @
            (if not (equal_inta b zero_inta)
              then [Chara (false, true, true, true, false, true, false,
                            false)] @
                     shows_prec_int zero_nata b []
              else []));;

let rec concat_str x = comp (comp implode concat) (map explode) x;;

let rec n num_states q = num_states q;;

let rec clkp_inv
  automata i l =
    sup_seta (equal_prod equal_nat equal_int)
      (image (comp collect_clock_pairs snd)
        (Set (filter (fun (a, _) -> equal_nata a l)
               (snd (snd (snd (nth automata i)))))));;

let rec bound_inv
  automata q c l =
    maxa linorder_int
      (sup_set equal_int (insert equal_int zero_inta bot_set)
        (sup_seta equal_int
          (image
            (fun (x, d) ->
              (if equal_nata x c then insert equal_int d bot_set else bot_set))
            (clkp_inv automata q l))));;

let rec clkp_seta
  automata i l =
    sup_set (equal_prod equal_nat equal_int) (clkp_inv automata i l)
      (sup_seta (equal_prod equal_nat equal_int)
        (image
          (fun (la, (_, (g, _))) ->
            (if equal_nata la l then collect_clock_pairs g else bot_set))
          (Set (fst (snd (snd (nth automata i)))))));;

let rec bound_g
  automata q c l =
    maxa linorder_int
      (sup_set equal_int (insert equal_int zero_inta bot_set)
        (sup_seta equal_int
          (image
            (fun (x, d) ->
              (if equal_nata x c then insert equal_int d bot_set else bot_set))
            (clkp_seta automata q l))));;

let rec bound
  automata q c l =
    max ord_int (bound_g automata q c l) (bound_inv automata q c l);;

let rec w
  automata num_states q c la l =
    (if equal_nata la (n num_states q) then uminus_inta (bound automata q c l)
      else zero_inta);;

let rec v num_states q = (fun v -> less_eq_nat v (n num_states q));;

let rec resets
  automata q c l =
    fold (fun (l1, (_, (_, (_, (_, (r, la)))))) xs ->
           (if not (equal_nata l1 l) ||
                 (membera equal_nat xs la || membera equal_nat r c)
             then xs else la :: xs))
      (fst (snd (snd (nth automata q)))) [];;

let rec ea automata q c l = resets automata q c l;;

let rec e
  automata num_states q c l =
    (if equal_nata l (n num_states q) then upt zero_nata (n num_states q)
      else filter (fun la -> membera equal_nat (ea automata q c la) l)
             (upt zero_nata (n num_states q)));;

let rec g
  automata num_states q c =
    Gen_g_impl_ext
      (v num_states q, e automata num_states q c, [n num_states q],
        w automata num_states q c);;

let rec calc_shortest_scc_paths (_A1, _A2, _A3)
  g n = (let sccs = compute_SCC_tr (equal_nat, hashable_nat) g in
         let d = map (fun _ -> None) (upt zero_nata n) @ [Some (zero _A2)] in
         let da =
           fold (fold (fun u ->
                        fold (fun v da ->
                               (match nth da u with None -> da
                                 | Some du ->
                                   (match nth da v
                                     with None ->
                                       list_update da v
 (Some (plus _A1 du (more g u v)))
                                     | Some dv ->
                                       (if less _A3 (plus _A1 du (more g u v))
     dv
 then list_update da v (Some (plus _A1 du (more g u v))) else da))))
                          (gi_E g u)))
             sccs d
           in
         let db =
           fold (fun vs db ->
                  (let dscc =
                     fold (fun v dscc ->
                            (match dscc with None -> nth db v
                              | Some daa ->
                                (match nth db v with None -> dscc
                                  | Some dv -> Some (min _A3 dv daa))))
                       vs None
                     in
                    fold (fun v dc -> list_update dc v dscc) vs db))
             sccs da
           in
          db);;

let rec local_ceiling_single
  automata num_states q c =
    (let a =
       calc_shortest_scc_paths (plus_int, zero_int, ord_int)
         (g automata num_states q c) (n num_states q)
       in
      map (fun aa ->
            (match aa with None -> zero_nata | Some x -> nat (uminus_inta x)))
        a);;

let rec local_ceiling
  broadcast bounds automata m num_states =
    app rev
      (fold (fun q xs ->
              app (fun x -> rev x :: xs)
                (fold (fun l xsa ->
                        app (fun x -> (zero_nata :: rev x) :: xsa)
                          (fold (fun c ->
                                  (fun a ->
                                    nth (local_ceiling_single automata
  num_states q c)
                                      l ::
                                      a))
                            (upt one_nata (suc m)) []))
                  (upt zero_nata (n num_states q)) []))
        (upt zero_nata (size_list automata)) []);;

let rec action_set _D
  automata broadcast =
    sup_set _D
      (sup_seta _D
        (image
          (fun (_, (_, (trans, _))) ->
            sup_seta _D
              (image
                (fun (_, a) -> (let (_, aa) = a in
                                let (_, ab) = aa in
                                let (ac, (_, (_, _))) = ab in
                                 set_act _D ac))
                (Set trans)))
          (Set automata)))
      (Set broadcast);;

let rec loc_set _A
  automata p =
    sup_seta _A
      (image
        (fun (l, (_, (_, (_, (_, (_, la)))))) ->
          insert _A l (insert _A la bot_set))
        (Set (fst (snd (snd (nth automata p))))));;

let rec extend_domain _A (_B1, _B2)
  m d n =
    (let (_, xs) =
       fold (fun x (i, xs) ->
              (if membera _A d x
                then (plus _B2 i (one _B1), (x, plus _B2 i (one _B1)) :: xs)
                else (i, xs)))
         d (n, [])
       in
     let ma = map_of _A xs in
      (fun x -> (if membera _A d x then the (ma x) else m x)));;

let rec mk_renaming _A
  str xs =
    binda (fold_error
            (fun x m ->
              (if mem_assoc _A x m then Error ["Duplicate name: " ^ str x]
                else Result ((x, size_list m) :: m)))
            xs [])
      (fun mapping ->
        Result
          (let m = map_of _A mapping in
           let f =
             (fun x ->
               (match m x with None -> failwith "empty case" | Some v -> v))
             in
           let ma = map_of equal_nat (map swap mapping) in
           let a =
             (fun x ->
               (match ma x with None -> failwith "empty case" | Some v -> v))
             in
            (f, a)));;

let rec mk_renaminga (_A1, _A2)
  xs = mk_renaming _A1 (comp implode (fun x -> shows_prec _A2 zero_nata x []))
         xs;;

let rec list_of_set _A
  xs = remdups _A ((fun x -> match x with Set xs -> xs) xs);;

let rec make_renaming (_A1, _A2)
  = (fun broadcast automata bounds ->
      (let action_seta =
         list_of_set equal_literal (action_set equal_literal automata broadcast)
         in
       let clk_seta = list_of_set equal_literal (clk_set equal_literal automata)
         in
       let clk_setb = clk_seta @ ["_urge"] in
       let loc_seta = (fun i -> list_of_set _A1 (loc_set _A1 automata i)) in
       let loc_setaa =
         sup_seta _A1
           (image
             (fun (_, (_, (t, _))) ->
               sup_seta _A1
                 (image
                   (fun (l, (_, (_, (_, (_, (_, la)))))) ->
                     insert _A1 l (insert _A1 la bot_set))
                   (Set t)))
             (Set automata))
         in
       let loc_set_diff =
         (fun i ->
           list_of_set _A1 (minus_set _A1 loc_setaa (loc_set _A1 automata i)))
         in
       let _ = list_of_set _A1 loc_setaa in
       let var_set =
         list_of_set equal_literal
           (sup_set equal_literal
             (sup_seta equal_literal
               (image
                 (fun s ->
                   sup_seta equal_literal
                     (image (vars_of_bexp equal_literal) s))
                 (image (fun t -> image (comp fst snd) (Set t))
                   (image (fun (_, (_, (t, _))) -> t) (Set automata)))))
             (sup_seta equal_literal
               (image
                 (fun s ->
                   sup_seta equal_literal
                     (image
                       (fun f ->
                         sup_seta equal_literal
                           (image
                             (fun (x, e) ->
                               sup_set equal_literal
                                 (insert equal_literal x bot_set)
                                 (vars_of_exp equal_literal e))
                             (Set f)))
                       s))
                 (image
                   (fun t ->
                     image (comp (comp (comp (comp fst snd) snd) snd) snd)
                       (Set t))
                   (image (fun (_, (_, (t, _))) -> t) (Set automata))))))
         in
       let n_ps = size_list automata in
       let num_actions = size_list action_seta in
       let m = size_list (remdups equal_literal clk_setb) in
       let num_states_list =
         map (fun i -> size_list (remdups _A1 (loc_seta i)))
           (upt zero_nata n_ps)
         in
       let num_states = nth num_states_list in
       let mk_renamingb = mk_renaming equal_literal (fun x -> x) in
        binda (combine2 (mk_renamingb action_seta)
                (combine2 (mk_renamingb clk_setb) (mk_renamingb var_set)))
          (fun (a, b) ->
            (let (renum_acts, _) = a in
              (fun (aa, ba) ->
                (let (renum_clocks, inv_renum_clocks) = aa in
                  (fun (renum_vars, inv_renum_vars) ->
                    (let renum_clocksa = comp suc renum_clocks in
                     let inv_renum_clocksa =
                       (fun c ->
                         (if equal_nata c zero_nata then "0"
                           else inv_renum_clocks (minus_nat c one_nata)))
                       in
                      binda (combine_map
                              (fun i -> mk_renaminga (_A1, _A2) (loc_seta i))
                              (upt zero_nata n_ps))
                        (fun renum_states_list ->
                          (let renum_states_lista = map fst renum_states_list in
                           let renum_states_listaa =
                             map_index zero_nata
                               (fun i ma ->
                                 extend_domain _A1 (one_nat, plus_nat) ma
                                   (loc_set_diff i) (size_list (loc_seta i)))
                               renum_states_lista
                             in
                           let renum_states = nth renum_states_listaa in
                           let inv_renum_states =
                             nth (map snd renum_states_list) in
                            binda (asserta
                                    (subset (card_UNIV_literal, equal_literal)
                                      (image fst (Set bounds)) (Set var_set))
                                    "State variables are declared but do not appear in model")
                              (fun _ ->
                                Result
                                  (m, (num_states,
(num_actions,
  (renum_acts,
    (renum_vars,
      (renum_clocksa,
        (renum_states,
          (inv_renum_states, (inv_renum_vars, inv_renum_clocksa)))))))))))))))
                  ba))
              b)));;

let rec preproc_mc _A
  = (fun dc ids_to_names (broadcast, (automata, bounds)) l_0 s_0 formula ->
      (let _ = print_string "Make renaming" in
        (match make_renaming (equal_nat, show_nat) broadcast automata bounds
          with Result
                 (m, (num_states,
                       (num_actions,
                         (renum_acts,
                           (renum_vars,
                             (renum_clocks,
                               (renum_states,
                                 (inv_renum_states,
                                   (inv_renum_vars, inv_renum_clocks)))))))))
            -> (let _ = print_string "Renaming" in
                let (broadcasta, (automataa, boundsa)) =
                  rename_network countable_literal countable_literal
                    countable_nat countable_literal broadcast bounds automata
                    renum_acts renum_vars renum_clocks renum_states
                  in
                let _ = print_string "Calculating ceiling" in
                let k = local_ceiling broadcasta boundsa automataa m num_states
                  in
                let _ = print_string "Running model checker" in
                let inv_renum_statesa =
                  (fun i -> comp (ids_to_names i) (inv_renum_states i)) in
                 (fun f_ () -> f_
                   ((rename_mc _A show_literal show_literal dc broadcast bounds
                      automata k "_urge" l_0 s_0 formula m num_states
                      num_actions renum_acts renum_vars renum_clocks
                      renum_states inv_renum_statesa inv_renum_vars
                      inv_renum_clocks)
                   ()) ())
                   (fun r -> (fun () -> (Result r))))
          | Error e -> (fun () -> (Error e)))));;

let rec shows_json
  n x1 = match n, x1 with n, Nata m -> pad n (shows_prec_nat zero_nata m [])
    | n, Rat r -> pad n (shows_rat r)
    | n, Int r -> pad n (shows_prec_int zero_nata r [])
    | n, Boolean b ->
        pad n (if b then [Chara (false, false, true, false, true, true, true,
                                  false);
                           Chara (false, true, false, false, true, true, true,
                                   false);
                           Chara (true, false, true, false, true, true, true,
                                   false);
                           Chara (true, false, true, false, false, true, true,
                                   false)]
                else [Chara (false, true, true, false, false, true, true,
                              false);
                       Chara (true, false, false, false, false, true, true,
                               false);
                       Chara (false, false, true, true, false, true, true,
                               false);
                       Chara (true, true, false, false, true, true, true,
                               false);
                       Chara (true, false, true, false, false, true, true,
                               false)])
    | n, Null ->
        pad n [Chara (false, true, true, true, false, true, true, false);
                Chara (true, false, true, false, true, true, true, false);
                Chara (false, false, true, true, false, true, true, false);
                Chara (false, false, true, true, false, true, true, false)]
    | n, Stringa s ->
        pad n ([Chara (false, true, false, false, false, true, false, false)] @
                s @ [Chara (false, true, false, false, false, true, false,
                             false)])
    | n, Arrayb xs ->
        (if null xs
          then pad n [Chara (true, true, false, true, true, false, true, false);
                       Chara (true, false, true, true, true, false, true,
                               false)]
          else pad n [Chara (true, true, false, true, true, false, true, false);
                       Chara (false, true, false, true, false, false, false,
                               false)] @
                 concat
                   (intersperse
                     [Chara (false, false, true, true, false, true, false,
                              false);
                       Chara (false, true, false, true, false, false, false,
                               false)]
                     (map (shows_json
                            (plus_nata n (nat_of_integer (Z.Int.of_int 2))))
                       xs)) @
                   [Chara (false, true, false, true, false, false, false,
                            false)] @
                     pad n [Chara (true, false, true, true, true, false, true,
                                    false)])
    | n, Object xs ->
        (if null xs
          then pad n [Chara (true, true, false, true, true, true, true, false);
                       Chara (true, false, true, true, true, true, true, false)]
          else pad n [Chara (true, true, false, true, true, true, true, false);
                       Chara (false, true, false, true, false, false, false,
                               false)] @
                 concat
                   (intersperse
                     [Chara (false, false, true, true, false, true, false,
                              false);
                       Chara (false, true, false, true, false, false, false,
                               false)]
                     (map (fun (k, v) ->
                            pad (plus_nata n (nat_of_integer (Z.Int.of_int 2)))
                              ([Chara (false, true, false, false, false, true,
false, false)] @
                                k @ [Chara (false, true, false, false, false,
     true, false, false)]) @
                              [Chara (false, true, false, true, true, true,
                                       false, false);
                                Chara (false, true, false, true, false, false,
false, false)] @
                                shows_json
                                  (plus_nata n (nat_of_integer (Z.Int.of_int 4))) v)
                       xs)) @
                   [Chara (false, true, false, true, false, false, false,
                            false)] @
                     pad n [Chara (true, false, true, true, true, true, true,
                                    false)]);;

let rec do_preproc_mc _A
  = (fun dc ids_to_names (broadcast, (automata, bounds)) l_0 s_0 formula ->
      (fun f_ () -> f_
        ((preproc_mc _A dc ids_to_names (broadcast, (automata, bounds)) l_0 s_0
           formula)
        ()) ())
        (fun r ->
          (fun () ->
            (match r with Result Renaming_Failed -> err "Renaming failed"
              | Result Preconds_Unsat -> err "Input invalid"
              | Result Sat ->
                Result
                  (if dc then "Model has a deadlock!"
                    else "Property is satisfied!")
              | Result Unsat ->
                Result
                  (if dc then "Model has no deadlock!"
                    else "Property is not satisfied!")
              | Error es ->
                err ("Error during preprocessing:\010" ^
                      concat_str (intersperse "\010" es))))));;

let rec shows_prec_JSON p x rest = shows_json zero_nata x @ rest;;

let rec parse_convert_run
  dc s =
    (match
      binda (parse json s)
        (fun r ->
          (let sa = implode (shows_prec_JSON zero_nata r []) in
           let _ =
             trace_level (Int_of_integer (Z.Int.of_int 2)) (fun _ -> (fun () -> sa))
             in
            binda (parse json sa)
              (fun ra ->
                binda (asserta (equal_JSONa r ra)
                        "Parse-print-parse loop failed!")
                  (fun _ -> convert r))))
      with Result
             (ids_to_names,
               (_, (broadcast, (automata, (bounds, (formula, (l_0, s_0)))))))
        -> do_preproc_mc show_literal dc ids_to_names
             (broadcast, (automata, bounds)) l_0 s_0 formula
      | Error es -> (fun () -> (Error es)));;

let rec convert_run
  dc json_data =
    (match
      (let s = implode (shows_prec_JSON zero_nata json_data []) in
       let _ =
         trace_level (Int_of_integer (Z.Int.of_int 2)) (fun _ -> (fun () -> s)) in
        binda (parse json s)
          (fun r ->
            binda (asserta (equal_JSONa json_data r)
                    "Parse-print-parse loop failed!")
              (fun _ -> convert json_data)))
      with Result
             (ids_to_names,
               (_, (broadcast, (automata, (bounds, (formula, (l_0, s_0)))))))
        -> do_preproc_mc show_literal dc ids_to_names
             (broadcast, (automata, bounds)) l_0 s_0 formula
      | Error es -> (fun () -> (Error es)));;

end;; (*struct Model_Checker*)
