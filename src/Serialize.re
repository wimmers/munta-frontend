let graph_node = (node: GraphView.node) =>
Json.Encode.({
    let obj = [
        ("id",      node##id    |> int),
        ("title",   node##title |> string),
        ("x",       node##x     |> float),
        ("y",       node##y     |> float),
        ("type",    node##_type |> string),
    ];
    object_(obj)
});

let graph_edge = (edge: GraphView.edge) =>
Json.Encode.({
    let obj = [
        ("source",  edge##source    |> int),
        ("target",  edge##target    |> int),
        ("type",    edge##_type     |> string),
    ];
    object_(obj)
});

let node = (node: App_Data.node) =>
Json.Encode.({
    let obj = [
        ("invariant", node.invariant |> string),
        ("node",      node.node      |> graph_node)
    ];
    object_(obj)
});

let edge = (edge: App_Data.edge) =>
Json.Encode.({
    let obj = [
        ("guard",   edge.guard  |> string),
        ("update",  edge.update |> string),
        ("label",   edge.label  |> string),
        ("edge",    edge.edge   |> graph_edge)
    ];
    object_(obj)
});

let single_state = (state: App_Data.single_state) =>
Json.Encode.({
    let obj = [
        ("initial", state.initial   |> int),
        ("nodes",   state.nodes     |> list(node)),
        ("edges",   state.edges     |> list(edge))
    ];
    object_(obj)
});

let state = (state: App_Data.state) =>
Json.Encode.({
    let obj = [
        ("automata",    state.automata  |> list(pair(int, pair(string, single_state)))),
        ("clocks",      state.clocks    |> string),
        ("vars",        state.vars      |> string),
        ("formula",     state.formula   |> string),
    ];
    object_(obj)
});