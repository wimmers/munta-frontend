type node = {
  invariant: string,
  node: GraphView.node,
};

type edge = {
  guard: string,
  update: string,
  label: string,
  edge: GraphView.edge,
};

type selected =
  | Nothing
  | Node(node)
  | Edge(edge);

type single_state = {
  /* graph: state, */
  selected,
  initial: int,
  nodes: list(node),
  edges: list(edge),
};

type verification_status =
  | Invalidated
  | Computing
  | Unknown
  | Rejected
  | Verified;

type state = {
  automata: list((int, (string, single_state))),
  selected: option(int),
  clocks: string,
  vars: string,
  formula: string,
  reply: option(string),
  nextId: int /* For automata and nodes */,
  show_help: bool,
  verification_status: verification_status,
};

let selected_to_view: selected => GraphView.selected =
  s =>
    switch (s) {
    | Nothing => GraphView.Nothing
    | Node(v) => GraphView.Node(v.node)
    | Edge(e) => GraphView.Edge(e.edge)
    };

let selected_node = s =>
  switch (s) {
  | Node(v) => v
  };

let selected_edge = s =>
  switch (s) {
  | Edge(e) => e
  };

let node_out: node => Parse.node_in =
  node => {
    id: node.node##id,
    invariant: node.invariant,
    label: node.node##title,
  };

let merge_node = (node, {id, label, invariant}: Parse.node_in) =>
  node.node##id == id ?
    {
      ...node,
      invariant,
      /* XXX Update label */
    } :
    node;

let edge_out: edge => Parse.edge_in =
  edge => {
    source: edge.edge##source,
    target: edge.edge##target,
    guard: edge.guard,
    label: edge.label,
    update: edge.update,
  };

let merge_edge =
    (edge, {source, target, guard, label, update}: Parse.edge_in) =>
  source == edge.edge##source && target == edge.edge##target ?
    {...edge, guard, label, update} : edge;

let automaton_out: (string, single_state) => Parse.automaton_in =
  (label, {selected, nodes, edges, initial}) => {
    nodes: List.map(node_out, nodes),
    edges: List.map(edge_out, edges),
    initial,
  };

/* XXX Spit error if initial doesn't match */
let merge_automaton =
    (automaton: single_state, {nodes, edges, initial}: Parse.automaton_in) => {
  nodes:
    List.map(x => List.fold_left(merge_node, x, nodes), automaton.nodes),
  edges:
    List.map(x => List.fold_left(merge_edge, x, edges), automaton.edges),
  initial,
  selected: automaton.selected,
};

let state_out =
    ({selected, automata, clocks, vars, formula}): Parse.network_in => {
  automata:
    List.map(
      ((_, (label, x))) => (label, automaton_out(label, x)),
      automata,
    ),
  clocks,
  vars,
  formula,
};

/* Check that automata lists are of equal length */
let merge_state =
    (state, {automata, clocks, vars, formula}: Parse.network_in) => {
  ...state,
  automata:
    List.map(
      ((i, (s, x))) => (
        i,
        (s, merge_automaton(x, List.assoc(s, automata))),
      ),
      state.automata,
    ),
  clocks,
  vars,
  formula,
};
