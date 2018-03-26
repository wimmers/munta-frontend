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
  
  type single_state = {
    /* graph: state, */
    selected,
    initial: int,
    nodes: list(node),
    edges: list(edge)
  };
  
  type state = {
    automata: list((int, (string, single_state))),
    selected: option(int),
    clocks: string,
    vars: string,
    formula: string,
    reply: option(string)
  };