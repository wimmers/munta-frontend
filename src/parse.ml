open Test2;;
open Parser;;
open Error;;

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
    initial: id_t;
}

type automaton_out = {
    nodes: node_out list;
    edges: edge_out list;
    initial: id_t;
}

type network_in = {
    automata: (string * automaton_in) list;
    clocks: string;
    vars: string;
}

type network_out = {
    automata: (string * automaton_out) list;
    clocks: string list;
    vars: var list;
}

let compile_node_label (label: string) =
    parse "node label" scan_var label

let compile_node ({id; label; invariant}: node_in) =
    parse "node label" scan_var label <|>
    parse "invariant" (scan_bexp_or_true scan_acconstraint) invariant >>= fun (label, invariant) ->
    Result ({id; label; invariant}: node_out)

let compile_edge ({source; target; guard; label; update}: edge_in) =
    parse "edge guard" (scan_bexp_or_true scan_acconstraint) guard <|>
    parse "edge label" scan_edge_label label <|>
    parse "edge update" scan_updates update >>= fun ((guard, label), update) ->
    Result ({source; target; guard; label; update}: edge_out)

let compile_automaton ({nodes; edges; initial}: automaton_in) =
    combine_map compile_node nodes <|> combine_map compile_edge edges >>=
    fun (nodes, edges) -> Result {nodes; edges; initial}

let compile ({automata; clocks; vars}: network_in) =
    combine_map (fun (s, x) -> compile_automaton x |> map_errors (fun e -> s ^ ": " ^ e) >>= fun x -> Result (s, x)) automata <|>
    parse "clocks" scan_clocks clocks <|>
    parse "variables" scan_vars vars >>= fun ((automata, clocks), vars) ->
    Result {automata; clocks; vars}

let print_node ({id; label; invariant}) =
    label ^ print_parens (string_of_int id) ^ ": " ^ print_bexp (fun x -> x) invariant

let print_edge ({source; target; guard; label; update}) =
    string_of_int source ^
    " -- " ^ print_bexp (fun x -> x)  guard ^
    " : " ^ print_action (fun x -> x) label ^
    " : " ^ print_list print_update update ^
    " --> " ^ string_of_int target

let print_items print_elem xs = List.map print_elem xs |> String.concat "\n"

let print_automaton ({nodes; edges}) =
    "Nodes: \n" ^ print_items print_node nodes ^ "\n\n" ^
    "Edges: \n" ^ print_items print_edge edges ^ "\n\n"

let print ({automata; clocks; vars}) =
    "Clocks: \n" ^ print_list (fun x -> x) clocks ^ "\n\n" ^
    "Vars: \n" ^ print_list print_var vars ^ "\n\n" ^
    "Automata: \n" ^ print_items (fun (s, x) -> s ^ ":\n\n" ^ print_automaton x) automata

let compile_and_print xs = match compile xs with
    | Result r -> "Success!\n\n" ^ print r
    | Error es -> "Errors encountered during parsing!\n\n" ^  print_items (fun x -> x) es