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

let nodeA: GraphView.node = {
  "id": nextId(),
  "title": "Node A",
  "x": 258.3976135253906,
  "y": 331.9783248901367,
  "type": GraphView.specialType
};

let nodeB: GraphView.node = {
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

let nodes: list(GraphView.node) = [nodeA, nodeB, nodeC, nodeD];

let edges: list(GraphView.edge) = [
  {"source": 1, "target": 2, "type": GraphView.specialEdgeType},
  {"source": 2, "target": 4, "type": GraphView.emptyEdgeType}
];

let init_state = {GraphView.nodes, edges};

let onSelectNode = (v: GraphView.node) => Js.log(v);

let onDeselectNode = () => Js.log("Deslected node");

let onCreateNode = (graph: GraphView.graph_state, x: float, y: float) => {
  let node: GraphView.node = {
    "id": nextId(),
    "title": "Node N",
    "x": x,
    "y": y,
    "type": GraphView.emptyType
  };
  let nodes = [node, ...graph.nodes];
  {...graph, GraphView.nodes};
};

let onSelectEdge: GraphView.edge => unit = e => Js.log(e);

let onCreateEdge:
  (GraphView.graph_state, GraphView.node, GraphView.node) =>
  GraphView.graph_state =
  (graph, v, w) => {
    let edge: GraphView.edge = {
      "source": v##id,
      "target": w##id,
      "type": GraphView.emptyEdgeType
    };
    let edges = [edge, ...graph.edges];
    {...graph, edges};
  };

let removeEdge = edge =>
  List.filter(e => e##source != edge##source || e##target != edge##target);

let onSwapEdge:
  (GraphView.graph_state, GraphView.node, GraphView.node, GraphView.edge) =>
  GraphView.graph_state =
  (graph, v, w, edge) => {
    let edge: GraphView.edge = {
      "source": v##id,
      "target": w##id,
      "type": GraphView.emptyEdgeType
      /* TODO: Problem with ##type accessor */
    };
    let edges = [edge, ...removeEdge(edge, graph.edges)];
    {...graph, edges};
  };

type state = {
  graph: GraphView.graph_state,
  selected: GraphView.selected
};

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
  initialState: () => {graph: init_state, selected: Nothing},
  reducer: (action: action, state: state) =>
    switch action {
    | Deselect => ReasonReact.Update({...state, selected: Nothing})
    | SelectNode(v) => ReasonReact.Update({...state, selected: Node(v)})
    | SelectEdge(e) => ReasonReact.Update({...state, selected: Edge(e)})
    | DeleteNode(node) =>
      let nodes = List.filter(v => v##id != node##id, state.graph.nodes);
      let edges =
        List.filter(
          e => e##source != node##id && e##target != node##id,
          state.graph.edges
        );
      ReasonReact.Update({
        ...state,
        graph: {
          edges,
          nodes
        }
      });
    | DeleteEdge(edge) =>
      let edges = removeEdge(edge, state.graph.edges);
      ReasonReact.Update({
        ...state,
        graph: {
          ...state.graph,
          edges
        }
      });
    | CreateNode(x, y) =>
      ReasonReact.Update({...state, graph: onCreateNode(state.graph, x, y)})
    | CreateEdge(v, w) =>
      ReasonReact.Update({...state, graph: onCreateEdge(state.graph, v, w)})
    | SwapEdge(v, w, e) =>
      ReasonReact.Update({...state, graph: onSwapEdge(state.graph, v, w, e)})
    | UpdateNode(node) =>
      let nodes =
        List.map(v => v##id == node##id ? node : v, state.graph.nodes);
      ReasonReact.Update({
        ...state,
        graph: {
          ...state.graph,
          GraphView.nodes
        }
      });
    },
  render: ({reduce, state, handle}) => {
    Js.log("render");
    Js.log(List.length(state.graph.nodes));
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
          nodes=state.graph.nodes
          edges=state.graph.edges
          selected=state.selected
        />
      </div>
    </div>;
  }
};