[%bs.raw {|require('./bootstrap/css/bootstrap.min.css')|}];

[%bs.raw {|require('./bootstrap/css/bootstrap-theme.min.css')|}];

[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo : string = "./logo.svg";

open Util;

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

let nodeA = {
  "id": nextId(),
  "title": "A",
  "x": 258.3976135253906,
  "y": 331.9783248901367,
  "type": GraphView.specialType
};

let nodeB = {
  "id": nextId(),
  "title": "B",
  "x": 593.9393920898438,
  "y": 260.6060791015625,
  "type": GraphView.emptyType
};

let nodeC = {
  "id": nextId(),
  "title": "C",
  "x": 237.5757598876953,
  "y": 61.81818389892578,
  "type": GraphView.emptyType
};

let nodeD = {
  "id": nextId(),
  "title": "D",
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

type single_state = {
  /* graph: state, */
  selected,
  initial: int,
  nodes: list(node),
  edges: list(edge)
};

type state = {
  automata: list((int, single_state)),
  selected: option(int),
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

let node_out: node => Parse.node_in =
  node => {
    id: node.node##id,
    invariant: node.invariant,
    label: node.node##title
  };

let edge_out: edge => Parse.edge_in =
  edge => {
    source: edge.edge##source,
    target: edge.edge##target,
    guard: edge.guard,
    label: edge.label,
    update: edge.update
  };

let automaton_out: (string, single_state) => Parse.automaton_in =
  (label, {selected, nodes, edges, initial}) => {
    nodes: List.map(node_out, nodes),
    edges: List.map(edge_out, edges),
    initial
  };

let state_out = ({selected, automata, clocks, vars}) : Parse.network_in => {
  automata:
    List.map(
      ((i, x)) => {
        let label = string_of_int(i);
        (label, automaton_out(label, x));
      },
      automata
    ),
  clocks,
  vars
};

let onSelectNode = (v: node) => Js.log(v);

let onDeselectNode = () => Js.log("Deslected node");

let onCreateNode = (graph: single_state, x: float, y: float) => {
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

let onCreateEdge:
  (single_state, GraphView.node, GraphView.node) => single_state =
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
  (single_state, GraphView.node, GraphView.node, GraphView.edge) =>
  single_state =
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
  | ChangeAutomaton(int, string)
  | DeleteAutomaton(int)
  | UpdateClocks(string)
  | UpdateVars(string)
  | SetInitial
  | UnsetInitial
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

module CheckBox = {
  let component = ReasonReact.statelessComponent("CheckBox");
  let make = (~onCheck, ~onUncheck, ~desc, ~checked, _children) => {
    ...component,
    render: self =>
      <div className="item">
        <input
          _type="checkbox"
          checked=(to_js_bool(checked))
          onChange=(_evt => checked ? onUncheck() : onCheck())
        />
        (str(desc))
      </div>
  };
};

module Declaration = {
  let component = ReasonReact.statelessComponent("Declaration");
  let make = (~desc, ~placeholder, ~onChange, ~value, _children) => {
    ...component,
    render: _self =>
      <div className="form-group col-md-3">
        <label htmlFor="text-box"> (str(desc)) </label>
        <textarea
          id="text-box"
          className="form-control"
          rows=7
          cols=20
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

let renderLabel = (~reduce, ~state: single_state) =>
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

let renderGuard = (~reduce, ~state: single_state) =>
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

let renderUpdate = (~reduce, ~state: single_state) =>
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

let renderInitial = (~reduce, ~state: single_state) =>
  switch state.selected {
  | Node(v) =>
    <CheckBox
      desc="Initial"
      checked=(v.node##id == state.initial)
      key=("IN" ++ key_of_node(v))
      onCheck=(reduce(_evt => SetInitial))
      onUncheck=(reduce(_evt => UnsetInitial))
    />
  | _ => ReasonReact.nullElement
  };

let empty_automaton = {nodes: [], edges: [], selected: Nothing, initial: (-1)};

let make = (~message, _children) => {
  ...component,
  initialState: () => {
    automata: [
      (
        0,
        {
          nodes: List.map(init_node, nodes),
          edges: List.map(init_edge, edges),
          selected: Nothing,
          initial: (-1)
        }
      )
    ],
    selected: Some(0),
    clocks: "",
    vars: ""
  },
  reducer: (action: action, {selected, automata} as state: state) => {
    let mk_upd = f =>
      switch selected {
      | None => ReasonReact.NoUpdate
      | Some(key) =>
        ReasonReact.Update({
          ...state,
          automata: assoc_upd_with(f, key, automata)
        })
      };
    let update_node = upd =>
      mk_upd(state => {
        let node = upd(selected_node(state.selected));
        {
          ...state,
          selected: Node(node),
          nodes: update_node(state.nodes, node)
        };
      });
    let update_edge = upd =>
      mk_upd(state => {
        let edge = upd(selected_edge(state.selected));
        {
          ...state,
          selected: Edge(edge),
          edges: update_edge(state.edges, edge)
        };
      });
    switch action {
    | ChangeAutomaton(key, value) =>
      selected == Some(key) ?
        ReasonReact.NoUpdate :
        List.mem_assoc(key, automata) ?
          ReasonReact.Update({...state, selected: Some(key)}) :
          ReasonReact.Update({
            ...state,
            selected: Some(key),
            automata: [(key, empty_automaton), ...automata]
          })
    | DeleteAutomaton(key) =>
      ReasonReact.Update({
        ...state,
        selected: None,
        automata: List.remove_assoc(key, automata)
      })
    | UpdateClocks(s) => ReasonReact.Update({...state, clocks: s})
    | UpdateVars(s) => ReasonReact.Update({...state, vars: s})
    | SetInitial =>
      mk_upd(automaton =>
        {...automaton, initial: selected_node(automaton.selected).node##id}
      )
    | UnsetInitial => mk_upd(automaton => {...automaton, initial: (-1)})
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
    | Deselect => mk_upd(state => {...state, selected: Nothing})
    | SelectNode(v) =>
      mk_upd(state => {...state, selected: Node(get_node(v, state.nodes))})
    | SelectEdge(e) =>
      mk_upd(state => {...state, selected: Edge(get_edge(e, state.edges))})
    | DeleteNode(node) =>
      mk_upd(state => {
        let nodes = List.filter(v => v.node##id != node##id, state.nodes);
        let edges =
          List.filter(
            e => e.edge##source != node##id && e.edge##target != node##id,
            state.edges
          );
        {...state, edges, nodes};
      })
    | DeleteEdge(edge) =>
      mk_upd(state => {
        let edges = removeEdge(edge, state.edges);
        {...state, edges};
      })
    | CreateNode(x, y) => mk_upd(state => onCreateNode(state, x, y))
    | CreateEdge(v, w) => mk_upd(state => onCreateEdge(state, v, w))
    | SwapEdge(v, w, e) => mk_upd(state => onSwapEdge(state, v, w, e))
    | UpdateNode(node) =>
      mk_upd(state => {
        let nodes =
          List.map(
            v => v.node##id == node##id ? {...v, node} : v,
            state.nodes
          );
        {...state, nodes};
      })
    };
  },
  render: ({reduce, state, handle}) => {
    let button_class = "btn btn-lg btn-default";
    let mk_render = f =>
      switch state.selected {
      | None => ReasonReact.nullElement
      | Some(key) => f(List.assoc(key, state.automata))
      };
    <div className="container">
      /* <div className="App-header">
           <img src=logo className="App-logo" alt="logo" />
           <h2> (ReasonReact.stringToElement(message)) </h2>
         </div>
         <p className="App-intro">
           (ReasonReact.stringToElement("To get started, edit"))
           <code> (ReasonReact.stringToElement(" src/app.re ")) </code>
           (ReasonReact.stringToElement("and save to reload!"))
         </p> */

        <div className="page-header">
          <h1 className="text-muted"> (str("Munta")) </h1>
          <p className="lead">
            (str("Verified Timed Automata Model Checker"))
          </p>
        </div>
        <div>
          (
            mk_render(state =>
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
                graphControls=false
                enableFocus=true
              />
            )
          )
          <div className="row">
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
          </div>
          (
            mk_render(state =>
              <div className="row">
                (renderUpdate(~reduce, ~state))
                (renderGuard(~reduce, ~state))
                (renderLabel(~reduce, ~state))
                (renderInitial(~reduce, ~state))
              </div>
            )
          )
          <ItemList
            onChangeFocus=(
              reduce(x => {
                let (k, v) = x;
                ChangeAutomaton(k, v);
              })
            )
            onDelete=(reduce(x => DeleteAutomaton(x)))
          />
          <input _type="button" className=button_class value="Compile!" />
        </div>
        <div>
          <pre> (str(state_out(state) |> Rename.rename_and_print)) </pre>
        </div>
      </div>;
    /* <Test />; */
  }
};