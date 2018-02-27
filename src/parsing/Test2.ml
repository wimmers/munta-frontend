open Parser

let input = Input.{text = "abcabc  abc"; index = 0; whitespace = " "}

let abc = stringParser "abc"

type ('a, 'b) bexp =
  Not of ('a, 'b) bexp |
  And of ('a, 'b) bexp * ('a, 'b) bexp |
  Or of ('a, 'b) bexp * ('a, 'b) bexp |
  Imply of ('a, 'b) bexp * ('a, 'b) bexp |
  Loc of 'a * 'a | Eq of 'a * 'b | Le of 'a * 'b |
  Lt of 'a * 'b | Ge of 'a * 'b | Gt of 'a * 'b

type ('a, 'b) formula =
  EX of ('a, 'b) bexp |
  EG of ('a, 'b) bexp |
  AX of ('a, 'b) bexp |
  AG of ('a, 'b) bexp |
  Leadsto of ('a, 'b) bexp * ('a, 'b) bexp

type 'a test =
  | Fail of string * 'a Parser.t
  | Success of string * 'a Parser.t * 'a

type 'a action =
  | Internal of 'a
  | Out of 'a
  | In of 'a

let scan_infix_pair p q s = p <*> (str s *> q)

let scan_failure = fun (rawInput: Input.t) ->
    let input = skipWhitespace rawInput.whitespace rawInput in ParseResult.ParseFailure ("Failed parser", input)

let scan_success x (input: Input.t) = ParseResult.ParseSuccess(x, input)

let rec scan_first xs = match xs with
    | [] -> scan_failure
    | (x::xs) -> x <|> scan_first xs

let int_re = [%bs.re "/0|(-)?[1-9]\d*/"]
let str_re = [%bs.re "/[A-Za-z_]\w*/"]

let scan_int = regex int_re ^^ int_of_string
let scan_var = regex str_re

let scan_acconstraint =
    let scan s c = scan_infix_pair scan_var scan_int s ^^ (fun (x, y) -> c x y) in
    scan_first [
        scan "<"  (fun x y -> Lt (x, y));
        scan "<=" (fun x y -> Le (x, y));
        scan "="  (fun x y -> Eq (x, y));
        scan ">=" (fun x y -> Ge (x, y));
        scan ">"  (fun x y -> Gt (x, y));
    ]

let scan_parens lparen rparen inner =
    str lparen *> (inner <* str rparen)

let scan_loc =
    (scan_var <*> (str "." *> scan_var)) ^^ (fun (x, y) -> Loc (x, y))

let scan_bexp_elem =
    scan_acconstraint <|> scan_loc

let scan_bexp scan_bexp_elem =
    let scan_parens = scan_parens "(" ")"
    in
    let rec scan_7 xs = parse xs
      (
        scan_infix_pair scan_6 scan_7 "->" ^^ (fun (x,y) -> Imply (x,y)) <|>
        scan_infix_pair scan_6 scan_7 "||"    ^^ (fun (x,y) -> Or (x,y)) <|>
        scan_6
      )
    and scan_6 xs = parse xs
      (scan_infix_pair scan_0 scan_6 "&&" ^^ (fun (x,y) -> And (x,y)) <|> scan_0)
    and scan_inner_bexp xs = parse xs (scan_parens scan_7)
    and scan_prefix sep =
      str sep *> scan_inner_bexp
    and scan_0 xs = parse xs
      (
        scan_prefix "~" ^^ (fun x -> Not(x)) <|>
        scan_bexp_elem <|>
        scan_inner_bexp
      )
  in
    scan_7

let scan_prefix p head = str head *> p

let scan_formula =
  let scan_bexp = scan_bexp scan_bexp_elem in
  scan_parens "(" ")" (scan_first [
  scan_prefix scan_bexp "EX" ^^ (fun x -> EX x);
  scan_prefix scan_bexp "EG" ^^ (fun x -> EG x);
  scan_prefix scan_bexp "AX" ^^ (fun x -> AX x);
  scan_prefix scan_bexp "AG" ^^ (fun x -> AG x);
  scan_infix_pair scan_bexp scan_bexp "-->" ^^ (fun (x, y) -> Leadsto (x, y))
  ])

let rec scan_sep_gen sep item_parser =
    ((item_parser <*> rep (sep *> item_parser)) ^^ (fun (x, y) -> x :: y)) <|>
    scan_success []

let scan_sep sep = scan_sep_gen (str sep)

let scan_vars   = scan_sep "," scan_var
let scan_clocks = scan_vars

let scan_infix_mult p q seps =
  List.map (fun sep -> scan_infix_pair p q sep) seps |> scan_first

let scan_update = scan_infix_mult scan_var (str "0") ["="; ":="] ^^ fst
let scan_updates = scan_sep "," scan_update

let scan_edge_label = scan_first [
  (scan_var <* str "?") ^^ (fun x -> In x);
  (scan_var <* str "!") ^^ (fun x -> Out x);
  scan_var              ^^ (fun x -> Internal x);
]

let mk_input s = Input.{text = s; index = 0; whitespace = " "}

let run_test = function
  | Fail (x, p) -> (
    match parse (mk_input x) p with
    | ParseResult.ParseFailure (_, _) -> "Test succeeded"
    | _ -> "Test did not fail as expected for input: " ^ x
  )
  | Success (x, p, r) -> match parse (mk_input x) p with
    | ParseResult.ParseSuccess(r', s) ->
      if r' = r
      then "Test succeeded"
      else "Test returned wrong result for input: " ^ x
    | _ -> "Test failed for input: " ^ x

let tests_bexp = [
  "Bexp 1", Success ("a < 3 && b >= 2 || c <= 4", scan_bexp scan_bexp_elem, Or (And (Lt("a", 3), Ge("b", 2)), Le("c", 4)));
  "Bexp 2", Success ("a < 3 && b >= 2 || ~ c <= 4", scan_bexp scan_bexp_elem, And (Lt("a", 3), Ge("b", 2)));
  "Bexp 3", Success ("a < 3 && b >= 2 || ~ (c <= 4)", scan_bexp scan_bexp_elem, Or (And (Lt("a", 3), Ge("b", 2)), Not (Le("c", 4))));
  "Bexp 4", Success ("a < 3 -> (b >= 2 || ~ (c <= 4))", scan_bexp scan_bexp_elem, Imply (Lt("a", 3), Or (Ge("b", 2), Not (Le("c", 4)))));
  "Bexp 5", Fail ("a << 3", scan_bexp scan_bexp_elem);
  "Bexp 6", Success ("a < 3", scan_bexp scan_bexp_elem, Lt("a", 3));
  "Bexp 7", Success ("a < 3 && b >= 2", scan_bexp scan_bexp_elem, And (Lt("a", 3), Ge("b", 2)));
  "Bexp 81", Success ("(_b = 123456789 && a12 > 0) && A_B3 <= -1", scan_bexp scan_bexp_elem, And (And(Eq("_b", 123456789), Gt("a12", 0)), Le("A_B3", -1)));
  "Bexp 82", Success ("_b = 123456789 && (A._a1 && A_B3 <= -1)", scan_bexp scan_bexp_elem, And (Eq("_b", 123456789), And(Loc("A", "_a1"), Le("A_B3", -1))));
  "Bexp 8", Success ("_b = 123456789 && a12 > 0 && A_B3 <= -1", scan_bexp scan_bexp_elem, And (Eq("_b", 123456789), And(Gt("a12", 0), Le("A_B3", -1))));
  "a < 3", Success ("a < 3", scan_bexp_elem, Lt("a", 3));
  "_b = 123456789", Success ("_b = 123456789", scan_bexp_elem, Eq("_b", 123456789));
  "a12 > 0", Success ("a12 > 0", scan_bexp_elem, Gt("a12", 0));
  "A_B3 <= -1", Success ("A_B3 <= -1", scan_bexp_elem, Le("A_B3", -1));
  "a._a1", Success ("a._a1", scan_bexp_elem, Loc("a", "_a1"));
]

let tests_var = [
  "a", Success ("a", scan_var, "a");
  "__a_bC_1234_", Success ("__a_bC_1234_", scan_var, "__a_bC_1234_");
  "_", Success ("_", scan_var, "_");
  "1abc", Fail ("1abc", scan_var);
]

let tests_int = [
  "0", Success ("0", scan_int, 0);
  "1", Success ("1", scan_int, 1);
  "-1", Success ("-1", scan_int, -1);
  "10", Success ("10", scan_int, 10);
  "-10", Success ("-10", scan_int, -10);
  "-123456789", Success ("-123456789", scan_int, -123456789);
  "01", Success ("01", scan_int, 0);
  "-0", Fail ("-0", scan_int);
  "-01", Fail ("-01", scan_int);
]

let tests_upds = [
  "Upd 0", Success ("0", scan_updates, []);
  "Upd []", Success ("", scan_updates, []);
  "Upd [x=0]", Success ("x=0", scan_updates, ["x"]);
  "Upd [x=1]", Success ("x=1", scan_updates, []);
  "Upd [x=0, y:=0]", Success ("x=0, y=0", scan_updates, ["x"; "y"]);
  "Upd [x :=  0  ,y:=  0]", Success ("x :=  0  ,y:=  0", scan_updates, ["x"; "y"]);
  "Upd [x : =  0  ,y:=  0]", Success ("x : =  0  ,y:=  0", scan_updates, []);
  "Upd [_x:= 0 ,    Y_1AYyz_z  =0]", Success ("_x:= 0 ,    Y_1AYyz_z  =0", scan_updates, ["_x"; "Y_1AYyz_z"]);
]

let test_vars = [
  "Vars []", Success ("", scan_vars, []);
  "Vars [x]", Success ("x", scan_vars, ["x"]);
  "Vars [x , y]", Success ("x , y", scan_vars, ["x"; "y"]);
  "Vars [x, y]", Success ("x, y", scan_clocks, ["x"; "y"]);
  "Vars [x,y]", Success ("x,y", scan_vars, ["x"; "y"]);
  "Vars [1,y]", Success ("1,y", scan_vars, []);
]

let evaluated_tests = List.concat([
  List.map (fun (x, y) -> (x, run_test y)) tests_int;
  List.map (fun (x, y) -> (x, run_test y)) tests_var;
  List.map (fun (x, y) -> (x, run_test y)) tests_bexp;
  List.map (fun (x, y) -> (x, run_test y)) tests_upds;
])

let test = fun () ->
  input
  |> rep abc
  |> Js.log;