open Rename;;
open UPPAAL_Model_Checker;;
open Util2;;

let int_of x = Big_int.big_int_of_int x |> fun x -> Model_Checker.Int_of_integer x
let nat_of x = Big_int.big_int_of_int x |> Model_Checker.nat_of_integer 
let int_to_nat x = Model_Checker.integer_of_int x |> Model_Checker.nat_of_integer

let convert_instr convert_a convert_b = function
    | Compile.JMPZ a -> Model_Checker.JMPZ (convert_a a)
    | ADD -> ADD
    | NOT -> NOT
    | AND -> AND
    | COPY -> COPY
    | CALL -> CALL
    | RETURN -> RETURN
    | LT -> LT
    | LE -> LE
    | EQ -> EQ
    | PUSH b -> PUSH (convert_b b)
    | POP -> POP
    | LID a -> LID (convert_a a)
    | STOREI (a, b) -> STOREI (convert_a a, convert_b b)
    | COPY -> COPY
    | RETURN -> RETURN
    | HALT -> HALT
    | STOREC (a, b) -> STOREC (convert_a a, convert_b b)
    | SETF b -> SETF b

let convert_acconstraint conv_a conv_b =
function
  | Test2.Lt (x, c) -> Model_Checker.LTa (conv_a x, conv_b c)
  | Le (x, c) -> LEa (conv_a x, conv_b c)
  | Eq (x, c) -> EQa (conv_a x, conv_b c)
  | Ge (x, c) -> GE (conv_a x, conv_b c)
  | Gt (x, c) -> GT (conv_a x, conv_b c)

let convert_instrc conv_a conv_b = function
    | Compile.INSTR x ->
        Model_Checker.INSTR (convert_instr conv_a conv_b x)
    | CEXP x -> CEXP (convert_acconstraint conv_a conv_b x)

let rec convert_bexp conv_a conv_b =
function
  | Test2.Not e -> Model_Checker.Not (convert_bexp conv_a conv_b e)
  | And (e1, e2) ->
    And (convert_bexp conv_a conv_b e1, convert_bexp conv_a conv_b e2)
  | Or (e1, e2) ->
    Or (convert_bexp conv_a conv_b e1, convert_bexp conv_a conv_b e2)
  | Imply (e1, e2) ->
    Imply (convert_bexp conv_a conv_b e1, convert_bexp conv_a conv_b e2)
  | Lt (x, c) -> Lta (conv_a x, conv_b c)
  | Le (x, c) -> Lea (conv_a x, conv_b c)
  | Eq (x, c) -> Eq (conv_a x, conv_b c)
  | Ge (x, c) -> Ge (conv_a x, conv_b c)
  | Gt (x, c) -> Gt (conv_a x, conv_b c)
  | Loc (s, x) -> Loc (conv_a s, conv_a x)

let convert_action conv = function
  | Test2.Internal x -> Model_Checker.Sil (conv x)
  | Out x -> Out (conv x)
  | In x -> In (conv x)

let convert_formula conv_a conv_b =
  let conv = convert_bexp conv_a conv_b in
  function
  | Test2.EX f -> Model_Checker.EX (conv f)
  | EG f -> EG (conv f)
  | AX f -> AX (conv f)
  | AG f -> AG (conv f)
  | Leadsto (f, g) -> Leadsto (conv f, conv g)

let convert_edge ({target; guard; label; update}) =
    (nat_of guard,
    (convert_action nat_of label,
    (nat_of update,
    nat_of target)))

let convert_edges = List.map (fun {nodes; edges} ->
    edges
    |> groupBy (fun {source} -> source)
    |> fun xs -> fill_groups (fun {source} -> source) (upto 0 (List.length nodes), xs)
    |> (List.map convert_edge |> List.map))

let convert_invariants {nodes} =
    nodes |> List.map (fun {invariant} ->
        List.map (convert_acconstraint nat_of int_of) invariant
    )

let convert_predicate ({nodes}) =
    nodes |> List.map (fun {predicate} -> predicate)

let print_result = function
    | None -> "Invalid input\n"
    | Some(true)  -> "Property is satisfied\n"
    | Some(false) -> "Property is not satisfied\n"

let print_checks name tests =
    if List.for_all snd tests then "" else
    "\n\nThe following checks failed (" ^ name ^ "):\n- " ^
    String.concat "\n- " (tests |> List.filter (fun (_, y) -> not y) |> List.map (fun x -> fst x |> Util2.implode))

let make_checks p m k max_steps inv trans prog query bounds pred s na =
    let pre_checks = Model_Checker.pre_checks p m inv pred trans prog
    and start_checks = Model_Checker.start_checks p max_steps trans prog bounds pred s
    and ceiling_checks = Model_Checker.ceiling_checks p m max_steps inv trans prog k
    and more_checks = Model_Checker.more_checks trans na
    in
    [
        print_checks "Preconditions" pre_checks;
        print_checks "Start" start_checks;
        print_checks "Ceiling" ceiling_checks;
        print_checks "Actions" more_checks;
    ]

let println (s : string) = print_string s; print_string "\n"

let print_bool b = string_of_bool b |> println

let convert_and_run (
    {automata; prog; vars; num_processes; num_clocks; num_actions; ceiling; formula}:
    Rename.network) () =
    let p = nat_of num_processes;
    and m = nat_of num_clocks;
    and ceiling = 0 :: ceiling;
    and max_steps = nat_of 10000;
    and inv = List.map convert_invariants automata;
    and trans = convert_edges automata;
    and prog = List.map (convert_instrc nat_of int_of) prog |> List.map (fun x -> Some x);
    and query = convert_formula nat_of int_of formula;
    and bounds = vars |> List.map (fun (x, y) -> (int_of x, int_of y));
    and pred = List.map convert_predicate automata |> (List.map nat_of |> List.map);
    and s = Util2.repeat 0 (List.length vars) |> List.map int_of;
    and na = num_actions |> nat_of;
    in
    let k = Model_Checker.k p m max_steps inv trans prog
    in
    let k = (int_to_nat |> List.map |> List.map |> List.map) k;
    in
    (
    make_checks p m k max_steps inv trans prog query bounds pred s na,
    Model_Checker.precond_mc p m k max_steps inv trans prog query bounds pred s na ()
    )

let convert_run_print network () =
    convert_and_run network () |>
    fun (check_string, r) -> String.concat "\n" check_string ^ "\n\n" ^ print_result r