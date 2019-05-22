let node = (n: App_Data.node) => {
  open Json.Encode;
  let obj = [
    ("id", n.node##id |> int),
    ("name", n.node##title |> string),
    ("x", n.node##x |> float),
    ("y", n.node##y |> float),
    ("invariant", n.invariant |> string),
  ];
  object_(obj);
};

let edge = (e: App_Data.edge) => {
  open Json.Encode;
  let obj = [
    ("source", e.edge##source |> int),
    ("target", e.edge##target |> int),
    ("guard", e.guard |> string),
    ("label", e.label |> string),
    ("update", e.update |> string),
  ];
  object_(obj);
};

let single_state = ((_, (name: string, state: App_Data.single_state))) => {
  open Json.Encode;
  let obj = [
    ("name", name |> string),
    ("initial", state.initial |> int),
    ("nodes", state.nodes |> list(node)),
    ("edges", state.edges |> list(edge)),
  ];
  object_(obj);
};

let state = (state: App_Data.state) => {
  open Json.Encode;
  let obj = [
    ("automata", state.automata |> list(single_state)),
    ("clocks", state.clocks |> string),
    ("vars", state.vars |> string),
    ("formula", state.formula |> string),
  ];
  object_(obj);
};
