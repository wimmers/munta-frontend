[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo : string = "./logo.svg";

let globalId = ref(1);

let globalId2 = ref(1.0);

let nextId = () => {
  let n = globalId^;
  globalId := n + 1;
  n;
};

let nextId2 = () => {
  let n = globalId2^;
  globalId2 := n +. 100.0;
  n;
};

/* Util */
let str = ReasonReact.stringToElement;

let valueFromEvent = evt : string => (
                                       evt
                                       |> ReactEventRe.Form.target
                                       |> ReactDOMRe.domElementToObj
                                     )##value;

let nodeA = {
  "id": nextId(),
  "title": "Node A",
  "x": 258.3976135253906,
  "y": 331.9783248901367,
  "type": GraphView.specialType
};

let nodeB = {
  "id": nextId(),
  "title": "Node B",
  "x": 593.9393920898438,
  "y": 260.6060791015625,
  "type": GraphView.emptyType
};

let nodeC = {
  "id": nextId(),
  "title": "Node C",
  "x": 237.5757598876953,
  "y": 61.81818389892578,
  "type": GraphView.emptyType
};

let nodeD = {
  "id": nextId(),
  "title": "Node C",
  "x": 600.5757598876953,
  "y": 600.81818389892578,
  "type": GraphView.emptyType
};

let nextTestNode = () => {
  let n = nextId();
  let n2 = nextId2();
  {
    "id": n,
    "title": "Node C",
    "x": 600.5757598876953 +. n2,
    "y": 600.81818389892578 +. n2,
    "type": GraphView.emptyType
  };
};

let nodes = [nodeA, nodeB, nodeC, nodeD];

let edges = [
  {"source": 1, "target": 2, "type": GraphView.specialEdgeType},
  {"source": 2, "target": 4, "type": GraphView.emptyEdgeType}
];

let init_state: GraphView.graph_state = {nodes, edges};

type node = {
  invariant: string,
  node: GraphView.node
};

type edge = {
  guard: string,
  update: string,
  label: string,
  edge: GraphView.edge
};

type selected =
  | Nothing
  | Node(node)
  | Edge(edge);

type state = {
  /* graph: state, */
  selected,
  nodes: list(node),
  edges: list(edge),
  clocks: string,
  vars: string
};

let init_node = v => {invariant: "", node: v};

let init_edge = e => {guard: "", update: "", label: "", edge: e};

let selected_to_view: selected => GraphView.selected =
  s =>
    switch s {
    | Nothing => GraphView.Nothing
    | Node(v) => GraphView.Node(v.node)
    | Edge(e) => GraphView.Edge(e.edge)
    };

let selected_node = s =>
  switch s {
  | Node(v) => v
  };

let selected_edge = s =>
  switch s {
  | Edge(e) => e
  };

let onSelectNode = (v: node) => Js.log(v);

let onDeselectNode = () => Js.log("Deslected node");

let onCreateNode = (graph: state, x: float, y: float) => {
  let node: GraphView.node = {
    "id": nextId(),
    "title": "Node N",
    "x": x,
    "y": y,
    "type": GraphView.emptyType
  };
  let nodes = [init_node(node), ...graph.nodes];
  {...graph, nodes};
};

let onSelectEdge: edge => unit = e => Js.log(e);

let onCreateEdge: (state, GraphView.node, GraphView.node) => state =
  (graph, v, w) => {
    let edge: GraphView.edge = {
      "source": v##id,
      "target": w##id,
      "type": GraphView.emptyEdgeType
    };
    let edges = [init_edge(edge), ...graph.edges];
    {...graph, edges};
  };

let removeEdge = edge =>
  List.filter(e =>
    e.edge##source != edge##source || e.edge##target != edge##target
  );

let onSwapEdge:
  (state, GraphView.node, GraphView.node, GraphView.edge) => state =
  (graph, v, w, e) => {
    let edge: GraphView.edge = {
      "source": v##id,
      "target": w##id,
      "type": GraphView.emptyEdgeType
      /* TODO: Problem with ##type accessor */
    };
    let e = List.find(x => x.edge === e, graph.edges);
    let edges = [{...e, edge}, ...removeEdge(edge, graph.edges)];
    {...graph, edges};
  };

let get_node: (GraphView.node, list(node)) => node =
  node => List.find(v => v.node##id == node##id);

let get_edge = edge => List.find(e => e.edge === edge);

let update_node = (nodes, node) =>
  List.map(v => v.node##id == node.node##id ? node : v, nodes);

let update_edge = (edges, edge) =>
  List.map(e => e.edge === edge.edge ? edge : e, edges);

type action =
  | UpdateClocks(string)
  | UpdateVars(string)
  | UpdateNodeInvariant(string)
  | UpdateNodeLabel(string)
  | UpdateEdgeGuard(string)
  | UpdateEdgeLabel(string)
  | UpdateEdgeUpdate(string)
  | Deselect
  | SelectNode(GraphView.node)
  | SelectEdge(GraphView.edge)
  | UpdateNode(GraphView.node)
  | DeleteNode(GraphView.node)
  | CreateNode(float, float)
  | CreateEdge(GraphView.node, GraphView.node)
  | DeleteEdge(GraphView.edge)
  | SwapEdge(GraphView.node, GraphView.node, GraphView.edge);

let component = ReasonReact.reducerComponent("App");

module Declaration = {
  let component = ReasonReact.statelessComponent("Declaration");
  let make = (~desc, ~placeholder, ~onChange, ~value, _children) => {
    ...component,
    render: self =>
      <div>
        (str(desc))
        <textarea
          placeholder
          onChange=(evt => onChange(valueFromEvent(evt)))
          value
        />
      </div>
  };
};

/* module Declaration = {
     type state = string;
     let component = ReasonReact.reducerComponent("Declaration");
     let make = (~desc, ~placeholder, ~onChange, ~value, _children) => {
       ...component,
       initialState: () => {
         Js.log(desc ++ " Init with " ++ value);
         value;
       },
       reducer: (newText, _text) =>
         /* onChange(newText); */
         ReasonReact.Update(newText),
       render: ({state: text, reduce}) =>
         <div>
           (str(desc))
           <textarea
             placeholder
             onChange=(reduce(evt => valueFromEvent(evt)))
             onBlur=(
               _evt => {
                 Js.log(desc ++ " Blur with " ++ text);
                 onChange(text);
               }
             )
             value=text
           />
         </div>
     };
   }; */
let key_of_node = v => string_of_int(v.node##id);

let key_of_edge = e =>
  string_of_int(e.edge##source) ++ "|" ++ string_of_int(e.edge##target);

let renderLabel = (~reduce, ~state) =>
  switch state.selected {
  | Node(v) =>
    <Declaration
      desc="Label:"
      placeholder="Node Label"
      value=v.node##title
      key=("LN" ++ key_of_node(v))
      onChange=(reduce(evt => UpdateNodeLabel(evt)))
    />
  | Edge(e) =>
    <Declaration
      desc="Label:"
      placeholder="Edge Label"
      value=e.label
      key=("LE" ++ key_of_edge(e))
      onChange=(reduce(evt => UpdateEdgeLabel(evt)))
    />
  | Nothing => ReasonReact.nullElement
  };

let renderGuard = (~reduce, ~state) =>
  switch state.selected {
  | Node(v) =>
    <Declaration
      desc="Invariant:"
      placeholder="Node Invariant"
      value=v.invariant
      key=("GN" ++ key_of_node(v))
      onChange=(reduce(evt => UpdateNodeInvariant(evt)))
    />
  | Edge(e) =>
    <Declaration
      desc="Guard:"
      placeholder="Edge Guard"
      value=e.guard
      key=("GE" ++ key_of_edge(e))
      onChange=(reduce(evt => UpdateEdgeGuard(evt)))
    />
  | Nothing => ReasonReact.nullElement
  };

let renderUpdate = (~reduce, ~state) =>
  switch state.selected {
  | Edge(e) =>
    <Declaration
      desc="Update:"
      placeholder="Edge Update"
      value=e.update
      key=("UE" ++ key_of_edge(e))
      onChange=(reduce(evt => UpdateEdgeUpdate(evt)))
    />
  | _ => ReasonReact.nullElement
  };

let make = (~message, _children) => {
  ...component,
  initialState: () => {
    clocks: "",
    vars: "",
    nodes: List.map(init_node, nodes),
    edges: List.map(init_edge, edges),
    selected: Nothing
  },
  reducer: (action: action, state: state) => {
    let update_node = upd => {
      let node = upd(selected_node(state.selected));
      ReasonReact.Update({
        ...state,
        selected: Node(node),
        nodes: update_node(state.nodes, node)
      });
    };
    let update_edge = upd => {
      let edge = upd(selected_edge(state.selected));
      ReasonReact.Update({
        ...state,
        selected: Edge(edge),
        edges: update_edge(state.edges, edge)
      });
    };
    switch action {
    | UpdateClocks(s) => ReasonReact.Update({...state, clocks: s})
    | UpdateVars(s) => ReasonReact.Update({...state, vars: s})
    | UpdateNodeInvariant(s) => update_node(node => {...node, invariant: s})
    | UpdateNodeLabel(s) =>
      update_node(node =>
        {
          ...node,
          node: {
            "id": node.node##id,
            "title": s,
            "x": node.node##x,
            "y": node.node##y,
            /* TODO: type is not a valid selector */
            "type": GraphView.emptyType
          }
        }
      )
    | UpdateEdgeGuard(s) => update_edge(edge => {...edge, guard: s})
    | UpdateEdgeLabel(s) => update_edge(edge => {...edge, label: s})
    | UpdateEdgeUpdate(s) => update_edge(edge => {...edge, update: s})
    | Deselect => ReasonReact.Update({...state, selected: Nothing})
    | SelectNode(v) =>
      ReasonReact.Update({...state, selected: Node(get_node(v, state.nodes))})
    | SelectEdge(e) =>
      ReasonReact.Update({...state, selected: Edge(get_edge(e, state.edges))})
    | DeleteNode(node) =>
      let nodes = List.filter(v => v.node##id != node##id, state.nodes);
      let edges =
        List.filter(
          e => e.edge##source != node##id && e.edge##target != node##id,
          state.edges
        );
      ReasonReact.Update({...state, edges, nodes});
    | DeleteEdge(edge) =>
      let edges = removeEdge(edge, state.edges);
      ReasonReact.Update({...state, edges});
    | CreateNode(x, y) => ReasonReact.Update(onCreateNode(state, x, y))
    | CreateEdge(v, w) => ReasonReact.Update(onCreateEdge(state, v, w))
    | SwapEdge(v, w, e) => ReasonReact.Update(onSwapEdge(state, v, w, e))
    | UpdateNode(node) =>
      let nodes =
        List.map(v => v.node##id == node##id ? {...v, node} : v, state.nodes);
      ReasonReact.Update({...state, nodes});
    };
  },
  render: ({reduce, state, handle}) =>
    <div className="App">
      <div className="App-header">
        <img src=logo className="App-logo" alt="logo" />
        <h2> (ReasonReact.stringToElement(message)) </h2>
      </div>
      <p className="App-intro">
        (ReasonReact.stringToElement("To get started, edit"))
        <code> (ReasonReact.stringToElement(" src/app.re ")) </code>
        (ReasonReact.stringToElement("and save to reload!"))
      </p>
      <div className="height">
        <GraphView
          onSelectNode=(reduce(v => SelectNode(v)))
          onDeselectNode=(reduce(() => Deselect))
          onUpdateNode=(reduce(v => UpdateNode(v)))
          onCreateNode=(reduce(p => CreateNode(fst(p), snd(p))))
          onDeleteNode=(reduce(v => DeleteNode(v)))
          onDeleteEdge=(reduce(e => DeleteEdge(e)))
          onSelectEdge=(reduce(e => SelectEdge(e)))
          onCreateEdge=(reduce(p => CreateEdge(fst(p), snd(p))))
          onSwapEdge=(
            reduce(p => {
              let (v, w, e) = p;
              SwapEdge(v, w, e);
            })
          )
          nodes=(List.map(v => v.node, state.nodes))
          edges=(List.map(e => e.edge, state.edges))
          selected=(selected_to_view(state.selected))
        />
        <Declaration
          desc="Clocks:"
          placeholder="Clock Declarations"
          value=state.clocks
          onChange=(reduce(evt => UpdateClocks(evt)))
        />
        <Declaration
          desc="Vars:"
          placeholder="Declarations of integer variables"
          value=state.vars
          onChange=(reduce(evt => UpdateVars(evt)))
        />
        (renderUpdate(reduce, state))
        (renderGuard(reduce, state))
        (renderLabel(reduce, state))
      </div>
    </div>
};