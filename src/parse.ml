open Test2;;
open Parser;;

(* Error monad *)
type error = string

type 'a result =
  | Result of 'a
  | Error of error list

let return m = Result m

let bind m f = match m with
    | Result x -> f x
    | Error es -> Error es

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

let parse err_info p x = match parse (mk_input x) p with
    | ParseResult.ParseFailure (_, _) -> Error ["Failed to parse " ^ err_info ^ ": " ^ x]
    | ParseResult.ParseSuccess (r, s) ->
        if String.length s.text <= s.index then Result r else Error ["Failed to parse " ^ err_info ^ ": " ^ x]

(* Parsing automata *)
type id_t = int

type node_in = {
  id: id_t;
  label: string;
  invariant: string;
}

type node_out = {
  id: id_t;
  label: string;
  invariant: (string, int) bexp;
}

type edge_in = {
  source: id_t;
  target: id_t;
  guard: string;
  label: string;
  update: string;
}

type edge_out = {
  source: id_t;
  target: id_t;
  guard: (string, int) bexp;
  label: string action;
  update: string list;
}

type automaton_in = {
    nodes: node_in list;
    edges: edge_in list;
    clocks: string;
    vars: string;
}

type automaton_out = {
    nodes: node_out list;
    edges: edge_out list;
    clocks: string list;
    vars: string list;
}

let compile_node_label (label: string) =
    parse "node label" scan_var label

let compile_node ({id; label; invariant}: node_in) =
    parse "node label" scan_var label <|>
    parse "invariant" (scan_bexp scan_acconstraint) invariant >>= fun (label, invariant) ->
    Result ({id; label; invariant}: node_out)

let compile_edge ({source; target; guard; label; update}: edge_in) =
    parse "edge guard" (scan_bexp scan_acconstraint) guard <|>
    parse "edge label" scan_edge_label label <|>
    parse "edge update" scan_updates update >>= fun ((guard, label), update) ->
    Result ({source; target; guard; label; update}: edge_out)

let compile_automaton ({nodes; edges; clocks; vars}: automaton_in) =
    combine_map compile_node nodes <|>
    combine_map compile_edge edges <|>
    parse "clocks" scan_clocks clocks <|>
    parse "variables" scan_vars vars >>= fun (((nodes, edges), clocks), vars) ->
    Result {nodes; edges; clocks; vars}

let compile: (string * automaton_in) list -> (string * automaton_out) list result =
    combine_map (fun (s, x) -> compile_automaton x |> map_errors (fun e -> s ^ ": " ^ e) >>= fun x -> Result (s, x))