open Error;;
open Test2;;

type ('a, 'b) instr =
    | JMPZ of 'a
    | ADD | NOT | AND | LT | LE | EQ
    | PUSH of 'b | POP
    | LID of 'a | STOREI of 'a * 'b
    | COPY | CALL | RETURN | HALT
    | STOREC of 'a * 'b | SETF of bool

type ('a, 'b) instrc =
    | INSTR of ('a, 'b) instr
    | CEXP  of ('a, 'b) bexp

let print_instr print_a print_b = function
    | JMPZ a -> "JMPZ " ^ print_a a
    | ADD -> "ADD"
    | NOT -> "NOT"
    | AND -> "AND"
    | LT -> "LT"
    | LE -> "LE"
    | EQ -> "EQ"
    | PUSH b -> "PUSH " ^ print_b b
    | POP -> "POP"
    | LID a -> "LID " ^ print_a a
    | STOREI (a, b) -> "STOREI " ^ print_a a ^ " " ^ print_b b
    | COPY -> "COPY"
    | CALL -> "CALL"
    | RETURN -> "RETURN"
    | HALT -> "HALT"
    | STOREC (a, b) -> "STOREC " ^ print_a a ^ " " ^ print_b b
    | SETF b -> if b then "SETF true" else "SETF false"

let print_instrc str = function
    | INSTR x -> print_instr str string_of_int x
    | CEXP x -> "CEXP " ^ print_bexp str  x

type edge = {
    source: Parse.id_t;
    target: Parse.id_t;
    guard: int;
    label: string action;
    update: int;
}

type node = {
    id: Parse.id_t;
    label: string;
    invariant: (string, int) bexp list;
    predicate: int;
}

type automaton = {
    nodes: node list;
    edges: edge list;
    initial: Parse.id_t;
}

type network = {
    prog: (string, int) instrc list;
    automata: (string * automaton) list;
    clocks: string list;
    vars: var list;
    num_processes: int; (* p *)
    num_clocks: int; (* m *)
    action_names: string list; (* length of this is na *)
    ceiling: (string * int) list; (* k *)
    formula: (string, int) formula;
}

let instr x = INSTR x
let cexp x = CEXP x
let map_instr xs = List.map instr xs
let map_cexp xs = List.map cexp xs
let return_cexp x = return [CEXP x]
let unknown_variable a = Error ["Unknown variable: " ^ a]

let action_name = function
    | Internal a -> a
    | In a -> a
    | Out a -> a

let check_bexp automata clocks vars =
    let is_clock c = List.mem c clocks
    and is_var v = List.mem v (List.map (fun {name} -> name) vars)
    in let check_cmp a b =
        if is_var a then return ()
        else if is_clock a then Error ["Clocks are not supported in formula"]
        else unknown_variable a
    and check_location a x =
        let matches = List.filter (fun (b, _) -> a = b) automata in
        if List.length matches = 0 then Error ["Unknown process: " ^ a]
        else if List.length matches > 1 then Error ["Ambiguous process name: " ^ a]
        else let {nodes} = snd (List.hd matches) in
        let names = List.map (fun {label} -> label) nodes
        in let matches = List.filter (fun y -> x = y) names in
        if List.length matches = 0 then Error ["Unknown location: " ^ a]
        else if List.length matches > 1 then Error ["Ambiguous location name: " ^ a]
        else return ()
    in let rec check = function
        | True -> Error ["True is not supported in formula"]
        | Not e -> check e
        | And (a, b)   -> check a <|> check b >>= fun _ -> return ()
        | Or (a, b)    -> check a <|> check b >>= fun _ -> return ()
        | Imply (a, b) -> check a <|> check b >>= fun _ -> return ()
        | Lt (a, b) -> check_cmp a b
        | Le (a, b) -> check_cmp a b
        | Eq (a, b) -> check_cmp a b
        | Ge (a, b) -> check_cmp a b
        | Gt (a, b) -> check_cmp a b
        | Loc (a, x) -> check_location a x
    in check

let check_formula automata clocks vars =
    let check = check_bexp automata clocks vars in
    function
    | EX f -> check f
    | EG f -> check f
    | AX f -> check f
    | AG f -> check f
    | Leadsto (f, g) -> check f <|> check g >>= fun _ -> return ()

let compile_bexp clocks vars =
    let is_clock c = List.mem c clocks
    and is_var v = List.mem v vars in
    let rec compile = function
        | True -> [SETF true] |> map_instr |> return
        | Not e -> compile e >>= fun xs ->
            xs @ map_instr [COPY; NOT] |> return
        | And (a, b) ->
            compile a <|> compile b >>= fun (xs, ys) ->
            xs @ map_instr [COPY] @ ys @ map_instr [AND] |> return
        | Or (a, b) ->
            compile a <|> compile b >>= fun (xs, ys) ->
            xs @ map_instr [COPY; NOT; COPY] @ ys @ map_instr [COPY; NOT; AND; COPY; NOT] |> return
        | Lt (a, b) as x ->
            if is_var a then [PUSH b; LID a; LT] |> map_instr |> return
            else if is_clock a then x |> return_cexp
            else unknown_variable a
        | Le (a, b) as x ->
            if is_var a then [PUSH b; LID a; LE] |> map_instr |> return
            else if is_clock a then x |> return_cexp
            else unknown_variable a
        | Eq (a, b) as x ->
            if is_var a then [PUSH b; LID a; EQ] |> map_instr |> return
            else if is_clock a then x |> return_cexp
            else unknown_variable a
        | Ge (a, b) as x ->
            if is_var a then [PUSH b; LID a; LT; COPY; NOT] |> map_instr |> return
            else if is_clock a then x |> return_cexp
            else unknown_variable a
        | Gt (a, b) as x ->
            if is_var a then [PUSH b; LID a; LE; COPY; NOT] |> map_instr |> return
            else if is_clock a then x |> return_cexp
            else unknown_variable a
        | Imply _ -> Error ["Implication is not supported here"]
        | Loc _ -> Error ["Location predicates are not supported here"]
    in compile

let compile_update clocks = combine_map (fun x -> if List.mem x clocks then STOREC (x, 0) |> instr |> return else unknown_variable x)

let compile_invariant clocks vars e =
    let is_clock c = List.mem c clocks
    and is_var v = List.mem v vars in
    let cexp_or_bexp a x =
        if is_clock a
        then return (True, [x])
        else if is_var a then return (x, [])
        else unknown_variable a
    in let rec chop = function
        | And (a, b) ->
            chop a <|> chop b >>= fun ((e1, xs1), (e2, xs2)) ->
            return (And (e1, e2), xs1 @ xs2)
        | Lt (a, _) as x -> cexp_or_bexp a x
        | Le (a, _) as x -> cexp_or_bexp a x
        | Eq (a, _) as x -> cexp_or_bexp a x
        | Ge (a, _) as x -> cexp_or_bexp a x
        | Gt (a, _) as x -> cexp_or_bexp a x
        | e -> return (e, [])
    in
        chop e >>= fun (e, inv) ->
        compile_bexp clocks vars e >>= fun xs ->
        return (xs, inv)

let compile_edge clocks vars pc ({source; target; guard; label; update}: Parse.edge_out) =
    compile_bexp clocks vars guard <|> compile_update clocks update >>= fun (guard, update) ->
    return ({source; target; guard = pc; label; update = pc + List.length guard + 1}, guard, update)

let compile_node clocks vars pc ({id; label; invariant}: Parse.node_out) =
    compile_invariant clocks vars invariant >>= fun (predicate, invariant) ->
    return ({id; label; predicate = pc; invariant}, predicate)

let compile_automaton clocks vars pc prog name ({nodes; edges; initial}: Parse.automaton_out) =
    let compile_edges = fold_error (fun (pc, prog, es) e ->
        compile_edge clocks vars pc e >>= fun (e, guard, update) ->
        return (pc + List.length guard + List.length update + 2, prog @ guard @ [instr HALT] @ update @ [instr HALT], es @ [e]))
    and compile_nodes = fold_error (fun (pc, prog, ns) n ->
        compile_node clocks vars pc n >>= fun (n, predicate) ->
        return (pc + List.length predicate + 1, prog @ predicate @ [instr HALT], ns @ [n]))
    in
        (compile_nodes (pc, prog, []) nodes >>= fun (pc, prog, nodes) ->
        compile_edges (pc, prog, []) edges >>= fun (pc, prog, edges) ->
        return (pc, prog, {nodes; edges; initial})
        |> assert_msg (initial >= 0) "No initial state"
        |> assert_msg (List.map (fun {id} -> id) nodes |> List.mem initial) "Initial state unknown"
        ) |> map_errors (fun e -> "In " ^ name ^ ": " ^ e)

let update_ceiling k x v =
    if List.mem_assoc x k then (x, max v (List.assoc x k)) :: List.remove_assoc x k else (x, v) :: k

let set_add x s = if List.mem x s then s else x :: s

let rec fold_ceiling_bexp ceiling = function
    | Not   x       -> fold_ceiling_bexp ceiling x
    | And   (a, b)  -> fold_ceiling_bexp ceiling a |> fun ceiling -> fold_ceiling_bexp ceiling b
    | Or    (a, b)  -> fold_ceiling_bexp ceiling a |> fun ceiling -> fold_ceiling_bexp ceiling b
    | Imply (a, b)  -> fold_ceiling_bexp ceiling a |> fun ceiling -> fold_ceiling_bexp ceiling b
    | Lt    (x, v)  -> update_ceiling ceiling x v
    | Le    (x, v)  -> update_ceiling ceiling x v
    | Eq    (x, v)  -> update_ceiling ceiling x v
    | Ge    (x, v)  -> update_ceiling ceiling x v
    | Gt    (x, v)  -> update_ceiling ceiling x v
    | _             -> ceiling

let compile_network ({automata; clocks; vars; formula}: Parse.network_out) =
    let compile_automata = fold_error (fun (pc, prog, xs) (name, x) ->
        compile_automaton clocks (List.map (fun x -> x.name) vars) pc prog name x >>= fun (pc, prog, x) ->
        return (pc, prog, xs @ [(name, x)]))
    in
        compile_automata (0, [], []) automata >>= fun (pc, prog, xs) ->
        let ceiling =
            List.map (fun c -> (c, 0)) clocks |>
            fun k -> List.fold_left (fun ceiling instr -> match instr with CEXP e -> fold_ceiling_bexp ceiling e | _ -> ceiling) k prog |>
            fun k -> List.fold_left (fun k (_, x) -> List.fold_left (fun k n -> List.fold_left fold_ceiling_bexp k n.invariant) k x.nodes) k xs
        and action_names =
            List.fold_left (fun s (_, x) -> List.fold_left (fun s ({label}: edge) -> set_add (action_name label) s) s x.edges) [] xs
        in
        check_formula xs clocks vars formula >>= fun () ->
        return {clocks; vars; prog;
            automata = xs;
            num_processes = List.length xs;
            num_clocks = List.length clocks;
            action_names;
            ceiling = ceiling;
            formula
        }

let print_node ({id; label; invariant; predicate}) =
    label ^ print_parens (string_of_int id) ^ ": " ^ string_of_int predicate ^ " : " ^ print_list (print_bexp (fun x -> x)) invariant

let print_edge ({source; target; guard; label; update}) =
    string_of_int source ^
    " -- " ^ string_of_int guard ^
    " : " ^ print_action (fun x -> x) label ^
    " : " ^ string_of_int update ^
    " --> " ^ string_of_int target

let print_automaton ({nodes; edges}) =
    "Nodes: \n" ^ Parse.print_items print_node nodes ^ "\n\n" ^
    "Edges: \n" ^ Parse.print_items print_edge edges ^ "\n\n"

let print ({automata; prog; clocks; vars; num_processes; num_clocks; action_names; ceiling}) =
    "Clocks: " ^ print_list (fun x -> x) clocks ^ "\n" ^
    "Vars: " ^ print_list print_var vars ^ "\n" ^
    "Number of automata: " ^ string_of_int num_processes ^ "\n" ^
    "Number of clocks: " ^ string_of_int num_clocks ^ "\n" ^
    "Action names: " ^ print_list (fun x -> x) action_names ^ "\n" ^
    "Clock ceiling: \n" ^ Parse.print_items (fun (c, k) -> c ^ ": " ^ string_of_int k) ceiling ^ "\n\n" ^
    "Automata: \n" ^ Parse.print_items (fun (s, x) -> s ^ ":\n\n" ^ print_automaton x) automata ^
    "Program: \n"  ^ Parse.print_items (print_instrc (fun x -> x)) prog

let compile_and_parse xs =
    Parse.compile xs
    |> err_msg "Errors encountered during parsing!\n\n"
    >>= fun r1 ->
    compile_network r1
    |> err_msg "Errors encountered during compiling!\n\n"
    >>= fun r2 ->
    "Result of parsing:\n\n" ^ Parse.print r1 ^ "\n\n\n" ^
    "Result of compiling:\n\n" ^ print r2
    |> return

let print_result r = 
    match r with
    | Result r -> r
    | Error es -> Parse.print_items (fun x -> x) es

let compile_and_print2 xs = compile_and_parse xs |> print_result

let compile_and_print xs = match Parse.compile xs with
    | Result r -> "Result of parsing:\n\n" ^ Parse.print r ^ "\n\n\n" ^
    (
        match compile_network r with
        | Result r -> "Result of compiling:\n\n" ^ print r
        | Error es -> "Errors encountered during compiling!\n\n" ^ Parse.print_items (fun x -> x) es
    )
    | Error es -> "Errors encountered during parsing!\n\n" ^ Parse.print_items (fun x -> x) es
