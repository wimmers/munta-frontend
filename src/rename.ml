open Test2;;
open Error;;
open Compile;;

type edge = {
    source: Parse.id_t;
    target: Parse.id_t;
    guard: int;
    label: int action;
    update: int;
}

type node = {
    id: Parse.id_t;
    invariant: (int, int) bexp list;
    predicate: int;
}

type automaton = {
    nodes: node list;
    edges: edge list;
}

type network = {
    prog: (int, int) Compile.instrc list;
    automata: automaton list;
    vars: (int * int) list;
    num_processes: int; (* p *)
    num_clocks: int; (* m *)
    num_actions: int; (* na *)
    ceiling: int list; (* k *)
    formula: (int, int) formula;
}

let rec rename_bexp f g h = function
  | True -> True
  | Not e -> Not (rename_bexp f g h e)
  | And (e1, e2) -> And (rename_bexp f g h e1, rename_bexp f g h e2)
  | Or (e1, e2) -> Or (rename_bexp f g h e1, rename_bexp f g h e2)
  | Imply (e1, e2) -> Imply (rename_bexp f g h e1, rename_bexp f g h e2)
  | Lt (x, c) -> Lt (f x, c)
  | Le (x, c) -> Le (f x, c)
  | Eq (x, c) -> Eq (f x, c)
  | Ge (x, c) -> Ge (f x, c)
  | Gt (x, c) -> Gt (f x, c)
  | Loc (s, x) -> Loc (g s, h s x)

let map_formula f = function
    | EX e -> EX (f e)
    | EG e -> EG (f e)
    | AX e -> AX (f e)
    | AG e -> AG (f e)
    | Leadsto (e1, e2) -> Leadsto (f e1, f e2)

let rename_formula f g h = map_formula (rename_bexp f g h) 

let rename_instr rename_vars (rename_clocks: string -> int) = function
    | JMPZ a -> JMPZ (rename_vars a) (* Problematic but do not have jumps for now *)
    | LID a -> LID (rename_vars a)
    | STOREI (a, b) -> STOREI (rename_vars a, b)
    | STOREC (a, b) -> STOREC (rename_clocks a, b)
    | ADD -> ADD | NOT -> NOT | AND -> AND | LT -> LT | LE -> LE | EQ -> EQ
    | COPY -> COPY | CALL -> CALL | RETURN -> RETURN | HALT -> HALT
    | PUSH x -> PUSH x | POP -> POP | SETF b -> SETF b

let rename_instrc rename_vars (rename_clocks: string -> int) = function
    | CEXP e -> CEXP (rename_bexp rename_clocks (fun _ -> -1) (fun _ _ -> -1) e)
    | INSTR e -> INSTR (rename_instr rename_vars rename_clocks e)

let mk_renaming str xs =
    Error.fold_error (fun m x -> if List.mem_assoc x m then Error ["Duplicate name: " ^ str x] else (x, List.length m) :: m |> return) [] xs
    >>= fun mapping -> return (fun x -> if List.mem_assoc x mapping then List.assoc x mapping else -1)

let map_label f = function
    | Internal a -> Internal (f a)
    | Out a -> Out (f a)
    | In a -> In (f a)

let rename_edge f_action f_loc ({source; target; guard; label; update}: Compile.edge) =
    let f_loc x = let v = f_loc x in
        if v >= 0 then Result v else Error ["Unknown location: " ^ string_of_int x]
    in f_loc source <|> f_loc target >>= fun (source, target) ->
    Result {source; target; guard; label = map_label f_action label; update}

let rename_node f_id f_bexp ({id; invariant; predicate}: Compile.node) =
    {id = f_id id; invariant = List.map f_bexp invariant; predicate}

let rename_automaton f_action (f_clock: string -> int) ({nodes; edges; initial}: Compile.automaton) =
    List.map (fun ({id}: Compile.node) -> id) nodes |> mk_renaming string_of_int >>= fun f_loc ->
    let n = f_loc initial in
    let f_loc i = let x = f_loc i in if x = n then 0 else if x < n then x + 1 else x
    and labs = List.map (fun ({id; label}: Compile.node) -> (label, id)) nodes in
    let f_lab x = List.assoc x labs |> f_loc in
    combine_map (rename_edge f_action f_loc) edges >>= fun edges ->
    ({
        nodes = List.map (rename_node f_loc (rename_bexp f_clock (fun _ -> -1) (fun _ _ -> -1))) nodes;
        edges
    }, f_lab) |> return

let rename_network ({automata; prog; clocks; vars; num_processes; num_clocks; action_names; ceiling; formula}: Compile.network) =
    let mk_renaming = mk_renaming (fun x -> x) in
    mk_renaming action_names <|> mk_renaming clocks <|> mk_renaming (List.map (fun x -> x.name) vars) <|>
    mk_renaming (List.map fst automata) >>= fun (((f_action, f_clock), f_var), f_automata) ->
    let f_clock = fun x -> f_clock x + 1 in
    combine_map (fun (k, x) -> rename_automaton f_action f_clock x >>= fun a -> return (k, a)) automata >>= fun automata ->
    let prog = List.map (rename_instrc f_var f_clock) prog
    and num_actions = List.length action_names
    and vars = List.map (fun {lower; upper} -> (lower, upper)) vars
    and ceiling = ceiling |> List.sort (fun (i, _) (j, _) -> f_clock i - f_clock j) |> List.map snd
    and f_pair = fun a x -> snd (List.assoc a automata) x
    in
       return {
           automata = List.map (fun x -> snd x |> fst) automata;
           formula = rename_formula f_var f_automata f_pair formula;
           prog; vars; num_processes; num_clocks; num_actions; ceiling
        }

let print_node ({id; invariant; predicate}) =
    string_of_int id ^ ": " ^ string_of_int predicate ^ " : " ^ print_list (print_bexp string_of_int) invariant

let print_edge ({source; target; guard; label; update}) =
    string_of_int source ^
    " -- " ^ string_of_int guard ^
    " : " ^ print_action string_of_int label ^
    " : " ^ string_of_int update ^
    " --> " ^ string_of_int target

let print_automaton ({nodes; edges}) =
    "Nodes: \n" ^ Parse.print_items print_node nodes ^ "\n\n" ^
    "Edges: \n" ^ Parse.print_items print_edge edges ^ "\n\n"

let print ({automata; prog; vars; num_processes; num_clocks; num_actions; ceiling; formula}) =
    "Formula: " ^ Test2.print_formula string_of_int formula ^ "\n" ^
    "Vars: " ^ print_list (fun (l, u) -> "["^string_of_int l ^ ":" ^ string_of_int u ^ "]") vars ^ "\n" ^
    "Number of automata: " ^ string_of_int num_processes ^ "\n" ^
    "Number of clocks: " ^ string_of_int num_clocks ^ "\n" ^
    "Number of actions: " ^ string_of_int num_actions ^ "\n" ^
    "Clock ceiling: \n" ^ print_list string_of_int ceiling ^ "\n\n" ^
    "Automata: \n" ^ Parse.print_items print_automaton automata ^
    "Program: \n"  ^ Parse.print_items (print_instrc string_of_int) prog

let parse_compile xs =
    Parse.compile xs
    |> err_msg "Errors encountered during parsing!\n\n"
    >>= fun r1 -> compile_network r1
    |> err_msg "Errors encountered during compiling!\n\n"
    >>= fun r2 -> rename_network r2
    |> err_msg "Errors encountered during renaming!\n\n"
    >>= fun r3 -> return (r1, r2, r3)

let parse_compile_print xs =
    parse_compile xs >>= fun (r1, r2, r3) ->
    "Result of parsing:\n\n" ^ Parse.print r1 ^ "\n\n\n" ^
    "Result of compiling:\n\n" ^ Compile.print r2 ^ "\n\n\n" ^
    "Result of renaming:\n\n" ^ print r3
    |> return

let rename_and_print x = parse_compile_print x |> print_result
