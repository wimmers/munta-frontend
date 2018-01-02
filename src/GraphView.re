[@bs.module "./graph.js"]
external graphView : ReasonReact.reactClass = "default";

type edge = {
  .
  "source": int,
  "target": int,
  "type": string
};

type node = {
  .
  "id": int,
  "title": string,
  "x": float,
  "y": float,
  "type": string
};

type graph_state = {
  nodes: list(node),
  edges: list(edge)
};

type js_graph_state = {
  .
  "nodes": array(node),
  "edges": array(edge)
};

type selected =
  | Nothing
  | Node(node)
  | Edge(edge);

let toJs: graph_state => js_graph_state =
  x => {"nodes": Array.of_list(x.nodes), "edges": Array.of_list(x.edges)};

let comp = (f, g, x) => f(g(x));

let comp2 = (f, g, x, y) => f(g(x, y));

let comp3 = (f, g, x, y, z) => f(g(x, y, z));

let emptyType = "empty";

let nodeKey = "id";

let specialType = "special";

let specialChildSubtype = "specialChild";

let emptyEdgeType = "emptyEdge";

let specialEdgeType = "specialEdge";

/* let nodes = [
     {
       id: 1,
       title: "Node A",
       x: 258.3976135253906,
       y: 331.9783248901367,
       type_: specialType
     },
     {
       id: 2,
       title: "Node B",
       x: 593.9393920898438,
       y: 260.6060791015625,
       type_: emptyType
     },
     {
       id: 3,
       title: "Node C",
       x: 237.5757598876953,
       y: 61.81818389892578,
       type_: emptyType
     },
     {
       id: 4,
       title: "Node C",
       x: 600.5757598876953,
       y: 600.81818389892578,
       type_: emptyType
     }
   ]; */
let make =
    (
      ~onSelectNode: node => unit,
      ~onDeselectNode: unit => unit,
      ~onUpdateNode: node => unit,
      ~onCreateNode: ((float, float)) => unit,
      ~onDeleteNode: node => unit,
      ~onDeleteEdge: edge => unit,
      ~onSelectEdge: edge => unit,
      ~onCreateEdge: ((node, node)) => unit,
      ~onSwapEdge: ((node, node, edge)) => unit,
      ~nodes: list(node),
      ~edges: list(edge),
      ~selected: selected,
      children
    ) => {
  let (selectedEdge, selectedNode) =
    switch selected {
    | Nothing => (Js.Nullable.null, Js.Nullable.null)
    | Node(v) => (Js.Nullable.null, Js.Nullable.return(v))
    | Edge(e) => (Js.Nullable.return(e), Js.Nullable.null)
    };
  ReasonReact.wrapJsForReason(
    ~reactClass=graphView,
    ~props={
      "onSelectNode": onSelectNode,
      "onSelectEdge": onSelectEdge,
      "onUpdateNode": onUpdateNode,
      "onCreateNode": (x, y) => onCreateNode((x, y)),
      "onDeleteNode": onDeleteNode,
      "onDeleteEdge": onDeleteEdge,
      "onDeselectNode": onDeselectNode,
      "onCreateEdge": (x, y) => onCreateEdge((x, y)),
      "onSwapEdge": (v, w, e) => onSwapEdge((v, w, e)),
      "nodes": Array.of_list(nodes),
      "edges": Array.of_list(edges),
      "selectedEdge": selectedEdge,
      "selectedNode": selectedNode
      /* "selected":
         switch selected {
         | Nothing => Js.Nullable.null
         | Node(v) => Js.Nullable.return(v)
         | Edge(e) => Js.Nullable.return(e)
         } */
    },
    children
  );
};