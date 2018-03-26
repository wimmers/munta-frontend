let graph_node = json =>
Json.Decode.([%bs.obj {
    id:     json |> field("id", int),
    title:  json |> field("title", string),
    x:      json |> field("x", float),
    y:      json |> field("y", float),
    _type:  json |> field("type", string),
}]);

let graph_edge = json =>
Json.Decode.([%bs.obj {
    source: json |> field("source", int),
    target: json |> field("target", int),
    _type:  json |> field("type", string),
}]);

let node = json =>
Json.Decode.({
    invariant:     json |> field("invariant", string),
    App_Data.node: json |> field("node", graph_node)
});

let edge = json =>
Json.Decode.({
    guard:  json |> field("guard", string),
    update: json |> field("update", string),
    label:  json |> field("label", string),
    App_Data.edge: json |> field("edge", graph_edge)
});

let single_state = json =>
Json.Decode.({
    initial: json |> field("initial", int),
    nodes:   json |> field("nodes", list(node)),
    edges:   json |> field("edges", list(edge)),
    App_Data.selected: Nothing
});

let state = json =>
Json.Decode.({
    automata: json |> field("automata", list(pair(int, pair(string, single_state)))),
    clocks:   json |> field("clocks", string),
    vars:     json |> field("vars", string),
    formula:  json |> field("formula", string),
    App_Data.reply: None,
    selected: None
});

let decode = s => switch (s |> Json.parseOrRaise |> state) {
| result => Some(result)
| exception Json.ParseError(_) => None
| exception Json.Decode.DecodeError(_) => None
};

let test = {|{"automata":[[0,["A",{"initial":1,"nodes":[{"invariant":"x >   0 && y < 3","node":{"id":1,"title":"A","x":256.1482238769531,"y":331.9783248901367,"type":"special"}},{"invariant":"","node":{"id":2,"title":"B","x":593.9393920898438,"y":260.6060791015625,"type":"empty"}},{"invariant":"","node":{"id":3,"title":"C","x":237.5757598876953,"y":61.81818389892578,"type":"empty"}},{"invariant":"","node":{"id":4,"title":"D","x":600.5757598876953,"y":600.8181838989258,"type":"empty"}}],"edges":[{"guard":"","update":"","label":"","edge":{"source":1,"target":2,"type":"specialEdge"}},{"guard":"","update":"","label":"","edge":{"source":2,"target":4,"type":"emptyEdge"}}]}]]],"clocks":"x,    y","vars":"","formula":"E<> A.C"}|};

/* Shoud evaluate to true */
let test1 = test |> Json.parseOrRaise |> state |> Serialize.state |> Json.stringify |> x => x == test;