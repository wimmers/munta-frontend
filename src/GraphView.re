[@bs.module "./graph.js"]
external graphView : ReasonReact.reactClass = "default";

type edge = Js.t({
  .
  source: int,
  target: int,
  _type: string
});

type node = Js.t({
  .
  id: int,
  title: string,
  x: float,
  y: float,
  _type: string
});

type graph_state = {
  nodes: list(node),
  edges: list(edge)
};

type js_graph_state = Js.t({
  .
  nodes: array(node),
  edges: array(edge)
});

type selected =
  | Nothing
  | Node(node)
  | Edge(edge);

let toJs: graph_state => js_graph_state =
  x => {"nodes": Array.of_list(x.nodes), "edges": Array.of_list(x.edges)};

let emptyType = "empty";

let nodeKey = "id";

let specialType = "special";

let specialChildSubtype = "specialChild";

let emptyEdgeType = "emptyEdge";

let specialEdgeType = "specialEdge";

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
      ~enableFocus: bool=false,
      ~graphControls: bool=true,
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
      "selectedNode": selectedNode,
      "graphControls": graphControls |> Util.to_js_bool,
      "enableFocus": enableFocus |> Util.to_js_bool,
    },
    children
  );
};