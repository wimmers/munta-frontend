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

type state = {
  /* graph: state, */
  selected: GraphView.selected,
  nodes: list(node),
  edges: list(edge)
};

let init_node = v => {invariant: "", node: v};

let init_edge = e => {guard: "", update: "", label: "", edge: e};

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
    let e = List.find(x => x.edge == e, graph.edges);
    let edges = [{...e, edge}, ...removeEdge(edge, graph.edges)];
    {...graph, edges};
  };

/* type additionalNodeState = {
     id: int,
     invariant: string
   };

   type additionalEdgeState = {
     source: int,
     target: int,
     guard: string,
     update: string,
     label: string
   }; */
type action =
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

let make = (~message, _children) => {
  ...component,
  initialState: () => {
    nodes: List.map(init_node, nodes),
    edges: List.map(init_edge, edges),
    selected: Nothing
  },
  reducer: (action: action, state: state) =>
    switch action {
    | Deselect => ReasonReact.Update({...state, selected: Nothing})
    | SelectNode(v) => ReasonReact.Update({...state, selected: Node(v)})
    | SelectEdge(e) => ReasonReact.Update({...state, selected: Edge(e)})
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
    },
  render: ({reduce, state, handle}) => {
    Js.log("render");
    Js.log(List.length(state.nodes));
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
          selected=state.selected
        />
      </div>
    </div>;
  }
};