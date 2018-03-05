(* Error monad *)
type error = string

type 'a result =
  | Result of 'a
  | Error of error list

let return m = Result m

let bind m f = match m with
    | Result x -> f x
    | Error es -> Error es

let err_msg m = function
    | Error es -> Error (m :: es)
    | x -> x

let make_err m = function
    | Error es -> Error (es @ [m])
    | _ -> Error [m]

let assert_msg b m = if b then (fun x -> x) else make_err m

let combine2_gen (comb: 'a -> 'b -> 'c result) = function
    | (Error e1, Error e2) -> Error (List.append e1 e2)
    | (Error e, Result _) -> Error e
    | (Result _, Error e) -> Error e
    | (Result a, Result b) -> comb a b

(* let combine2: ('a result * 'b result -> ('a * 'b) result) = combine2_gen (fun a b -> Result (a, b)) *)

let combine2 = function
    | (Error e1, Error e2) -> Error (List.append e1 e2)
    | (Error e, Result _) -> Error e
    | (Result _, Error e) -> Error e
    | (Result a, Result b) -> Result (a, b)

(* let (<|>) (x: 'a result) (y: 'b result): ('a * 'b) result  = combine2 (x, y) *)
let (<|>) (x: 'a result) (y: 'b result): ('a * 'b) result = combine2 (x, y)
let (>>=) = bind

let rec combine: 'a result list -> 'a list result = function
    | [] -> Result []
    | (x :: xs) -> combine2_gen (fun (x: 'a) (xs: 'a list) -> Result (x :: xs)) (x, combine xs)

let combine_map f xs = List.map f xs |> combine

let map_errors f = function
    | Error errors -> Error (List.map f errors)
    | r -> r

let rec fold_error f a = function
    | [] -> return a
    | x :: xs -> f a x >>= fun a -> fold_error f a xs