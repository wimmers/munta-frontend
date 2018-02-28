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

let print_instrc = function
    | INSTR x -> print_instr (fun x -> x) string_of_int x
    | CEXP x -> "CEXP " ^ print_bexp x

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
    clocks: string list;
    vars: string list;
}

type network = {
    prog: (string, int) instrc list;
    automata: (string * automaton) list;
}

let instr x = INSTR x
let cexp x = CEXP x
let map_instr xs = List.map instr xs
let map_cexp xs = List.map cexp xs
let return_cexp x = return [CEXP x]
let unknown_variable a = Error ["Unknown variable: " ^ a]

let compile_bexp clocks vars =
    let is_clock c = List.mem c clocks
    and is_var v = List.mem v vars in
    let rec compile = function
        | True -> [SETF true] |> map_instr |> return
        | Not e -> compile e >>= fun xs ->
            xs @ map_instr [NOT] |> return
        | And (a, b) ->
            compile a <|> compile b >>= fun (xs, ys) ->
            xs @ map_instr [COPY] @ ys @ map_instr [AND] |> return
        | Or (a, b) ->
            compile a <|> compile b >>= fun (xs, ys) ->
            xs @ map_instr [NOT; COPY] @ ys @ map_instr [NOT; AND; NOT] |> return
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
            if is_var a then [PUSH b; LID a; LT; NOT] |> map_instr |> return
            else if is_clock a then x |> return_cexp
            else unknown_variable a
        | Gt (a, b) as x ->
            if is_var a then [PUSH b; LID a; LE; NOT] |> map_instr |> return
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

let compile_automaton pc prog name ({nodes; edges; clocks; vars}: Parse.automaton_out) =
    let compile_edges = fold_error (fun (pc, prog, es) e ->
        compile_edge clocks vars pc e >>= fun (e, guard, update) ->
        return (pc + List.length guard + List.length update + 2, prog @ guard @ [instr HALT] @ update @ [instr HALT], es @ [e]))
    and compile_nodes = fold_error (fun (pc, prog, ns) n ->
        compile_node clocks vars pc n >>= fun (n, predicate) ->
        return (pc + List.length predicate + 1, prog @ predicate @ [instr HALT], ns @ [n]))
    in
        (compile_nodes (pc, prog, []) nodes >>= fun (pc, prog, nodes) ->
        compile_edges (pc, prog, []) edges >>= fun (pc, prog, edges) ->
        return (pc, prog, {nodes; edges; clocks; vars})) |>
        map_errors (fun e -> "In " ^ name ^ ": " ^ e)

let compile_network (xs: (string * Parse.automaton_out) list) =
    let compile_automata = fold_error (fun (pc, prog, xs) (name, x) ->
        compile_automaton pc prog name x >>= fun (pc, prog, x) ->
        return (pc, prog, xs @ [(name, x)]))
    in
        compile_automata (0, [], []) xs >>= fun (pc, prog, xs) ->
        return {prog; automata = xs}

let print_node ({id; label; invariant; predicate}) =
    label ^ print_parens (string_of_int id) ^ ": " ^ string_of_int predicate ^ " : " ^ print_list print_bexp invariant

let print_edge ({source; target; guard; label; update}) =
    string_of_int source ^
    " -- " ^ string_of_int guard ^
    " : " ^ print_action label ^
    " : " ^ string_of_int update ^
    " --> " ^ string_of_int target

let print_automaton ({nodes; edges; clocks; vars}) =
    "Nodes: \n" ^ Parse.print_items print_node nodes ^ "\n\n" ^
    "Edges: \n" ^ Parse.print_items print_edge edges ^ "\n\n" ^
    "Clocks: \n" ^ print_list (fun x -> x) clocks ^ "\n\n" ^
    "Vars: \n" ^ print_list (fun x -> x) vars ^ "\n\n"

let print ({automata; prog}) =
    "Automata: \n" ^ Parse.print_items (fun (s, x) -> s ^ ":\n\n" ^ print_automaton x) automata ^
    "Program: \n"  ^ Parse.print_items print_instrc prog

let compile_and_print xs = match Parse.compile xs with
    | Result r -> "Result of parsing:\n\n" ^ Parse.print r ^ "\n\n\n" ^
    (
        match compile_network r with
        | Result r -> "Result of compiling:\n\n" ^ print r
        | Error es -> "Errors encountered during compiling!\n\n" ^ Parse.print_items (fun x -> x) es
    )
    | Error es -> "Errors encountered during parsing!\n\n" ^ Parse.print_items (fun x -> x) es