/* Util */
[@bs.val] external alert: string => unit = "window.alert";

let str = React.string;

let to_js_bool = x => x;

let valueFromEvent = evt: string => evt->ReactEvent.Form.target##value;

let boolFromEvent = evt: bool => evt->ReactEvent.Form.target##value;

let assoc_upd_with = (f, key) =>
  List.map(((k, v)) => k == key ? (k, f(v)) : (k, v));

let assoc_upd = (key, value) => assoc_upd_with(_v => value, key);

let the = x =>
  switch (x) {
  | Some(x) => x
  };

let make_new_name = (names, name) => {
  let rec f = i => {
    let n = name ++ "_" ++ string_of_int(i);
    List.mem(n, names) ? f(i + 1) : n;
  };
  f(1);
};

let max_list = (f, xs) => List.fold_left((a, x) => max(a, f(x)), 0, xs);
