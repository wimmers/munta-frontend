/* Util */
let str = ReasonReact.stringToElement;

let to_js_bool = Js.Boolean.to_js_boolean;

let valueFromEvent = evt : string => (
                                       evt
                                       |> ReactEventRe.Form.target
                                       |> ReactDOMRe.domElementToObj
                                     )##value;

let boolFromEvent = evt : bool => (
                                    evt
                                    |> ReactEventRe.Form.target
                                    |> ReactDOMRe.domElementToObj
                                  )##value;

let assoc_upd_with = (f, key) =>
  List.map(((k, v)) => k == key ? (k, f(v)) : (k, v));

let assoc_upd = (key, value) => assoc_upd_with(_v => value, key);

let the = x =>
  switch x {
  | Some(x) => x
  };

let make_new_name = (names, name) => {
  let rec f = i => {
    let n = name ++ "_" ++ string_of_int(i);
    List.mem(n, names) ? f(i + 1) : n
  };
  f(1);
};