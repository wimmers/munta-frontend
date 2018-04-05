let node_standard_type = GraphView.emptyType;
let edge_standard_type = GraphView.emptyEdgeType;

let node = json =>
Json.Decode.({
    let node = [%bs.obj {
        id:     json |> field("id", int),
        title:  json |> field("name", string),
        x:      json |> field("x", float),
        y:      json |> field("y", float),
        _type: node_standard_type
    }];
    {
        invariant:     json |> field("invariant", string),
        App_Data.node: node
    }
});

let edge = json =>
Json.Decode.({
    let edge = [%bs.obj {
        source: json |> field("source", int),
        target: json |> field("target", int),
        _type: edge_standard_type
    }];
    {
        guard:  json |> field("guard", string),
        update: json |> field("update", string),
        label:  json |> field("label", string),
        App_Data.edge: edge
    }
});

let single_state = json =>
Json.Decode.({
    let automaton = {
        initial: json |> field("initial", int),
        nodes:   json |> field("nodes", list(node)),
        edges:   json |> field("edges", list(edge)),
        App_Data.selected: Nothing
    };
    (json |> field("name", string), automaton)
});

let state = json =>
Json.Decode.({
    let automata =
        json 
        |> field("automata", list(single_state))
        |> xs => List.map2((x, y) => (x, y), Util2.upto(0, List.length(xs)), xs);
    let nextId = Util.max_list(
        a => Util.max_list(
            (n: App_Data.node) => n.node##id,
            (snd(snd(a)): App_Data.single_state).nodes),
        automata);
    let nextId = max(nextId, List.length(automata)) + 1;
    {
    automata,
    nextId,
    clocks:   json |> field("clocks", string),
    vars:     json |> field("vars", string),
    formula:  json |> field("formula", string),
    App_Data.reply: None,
    selected: None
}});

let decode = s => switch (s |> Json.parseOrRaise |> state) {
| result => Some(result)
| exception Json.ParseError(_) => None
| exception Json.Decode.DecodeError(_) => None
};