open Test2;;
open Error;;
open Compile;;
open Rename;;

let print_list print_elem xs = "[" ^ Test2.print_list print_elem xs ^ "]"
let print_pair str1 str2 (a, b) = "(" ^ str1 a ^ ", " ^ str2 b ^ ")"

let print_pairc constr str1 str2 (a, b) =
    constr ^ "(" ^ str1 a ^ ", " ^ str2 b ^ ")"

let print_singlec constr str a =
    constr ^ "(" ^ str a ^ ")"

let print_action str = function
  | Internal x -> "Sil " ^ str x
  | Out x -> "Out " ^ str x
  | In x -> "In " ^ str x

let print_acconstraint str =
    let print_cmp constr = print_pairc constr str string_of_int
    in
function
  | Lt (x, c) -> print_cmp "LTa" (x, c)
  | Le (x, c) -> print_cmp "LEa" (x, c)
  | Eq (x, c) -> print_cmp "EQa" (x, c)
  | Ge (x, c) -> print_cmp "GE" (x, c)
  | Gt (x, c) -> print_cmp "GT" (x, c)
  | _ -> "FAIL"

let rec print_bexp str =
    let print_bin constr = print_pairc constr (print_bexp str) (print_bexp str)
    and print_cmp constr = print_pairc constr str string_of_int
    in
function
  | True -> "true"
  | Not e -> print_singlec "Not'" (print_bexp str) e
  | And (e1, e2) -> print_bin "And'" (e1, e2)
  | Or (e1, e2) -> print_bin "Or'" (e1, e2)
  | Imply (e1, e2) -> print_bin "Imply'" (e1, e2)
  | Lt (x, c) -> print_cmp "Lta'" (x, c)
  | Le (x, c) -> print_cmp "Lea'" (x, c)
  | Eq (x, c) -> print_cmp "Eq'" (x, c)
  | Ge (x, c) -> print_cmp "Ge'" (x, c)
  | Gt (x, c) -> print_cmp "Gt'" (x, c)
  | Loc (s, x) -> print_pairc "Loc'" str str (s, x)

let print_invariant ({nodes}) =
    nodes |> print_list (fun {invariant} ->
        print_list (print_bexp string_of_int) invariant
    )

let print_invariants = print_list print_invariant

let print_predicate ({nodes}) =
    nodes |> print_list (fun {predicate} -> string_of_int predicate)

let print_predicates = print_list print_predicate

let print_edge ({target; guard; label; update}) =
    "("  ^ string_of_int guard ^
    ", " ^ print_action string_of_int label ^
    ", " ^ string_of_int update ^
    ", " ^ string_of_int target ^
    ")"

let print_edges = print_list (fun {edges} -> print_list print_edge edges)

let print_ceiling = print_list string_of_int

let print_instr print_a print_b = function
    | JMPZ a -> "JMPZ' " ^ print_a a
    | ADD -> "ADD'"
    | NOT -> "NOT'"
    | AND -> "AND'"
    | LT -> "LT'"
    | LE -> "LE'"
    | EQ -> "EQ'"
    | PUSH b -> "PUSH' " ^ print_b b
    | POP -> "POP'"
    | LID a -> "LID' " ^ print_a a
    | STOREI (a, b) -> "STOREI' " ^ print_a a ^ " " ^ print_b b
    | COPY -> "COPY'"
    | CALL -> "CALL'"
    | RETURN -> "RETURN'"
    | HALT -> "HALT'"
    | STOREC (a, b) -> "STOREC' " ^ print_a a ^ " " ^ print_b b
    | SETF b -> if b then "SETF' true" else "SETF' false"

let print_instrc str = function
    | INSTR x -> "SOME (INSTR' (" ^ print_instr str string_of_int x ^ "))"
    | CEXP x -> "SOME (CEXP' (" ^ print_bexp str  x ^ "))"

let print_prog = print_instrc string_of_int |> print_list

let print_bounds = print_pair string_of_int string_of_int |> print_list

let rec repeat x n = if n <= 0 then [] else x :: repeat x (n - 1)

let print ({automata; prog; vars; num_processes; num_clocks; num_actions; ceiling}) =
    Parse.print_items (fun x -> x) [
        string_of_int num_processes;
        string_of_int num_clocks;
        print_ceiling ceiling;
        "10000";
        print_invariants automata;
        print_edges automata;
        print_prog prog;
        print_bounds vars;
        print_predicates automata;
        repeat 0 (List.length vars) |> print_list string_of_int;
        string_of_int num_actions;
    ]

let parse_compile_print xs =
    Parse.compile xs
    |> err_msg "Errors encountered during parsing!\n\n"
    >>= fun r1 -> compile_network r1
    |> err_msg "Errors encountered during compiling!\n\n"
    >>= fun r2 -> rename_network r2
    |> err_msg "Errors encountered during renaming!\n\n"
    >>= fun r3 ->
    "Result of parsing:\n\n" ^ Parse.print r1 ^ "\n\n\n" ^
    "Result of compiling:\n\n" ^ Compile.print r2 ^ "\n\n\n" ^
    "Result of renaming:\n\n" ^ Rename.print r3  ^ "\n\n\n" ^
    "Output for Munta:\n\n" ^ print r3
    |> return

let rename_and_print x = parse_compile_print x |> print_result