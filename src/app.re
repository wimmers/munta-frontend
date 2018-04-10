[%bs.raw {|require('./bootstrap/css/bootstrap.min.css')|}];

[%bs.raw {|require('./bootstrap/css/bootstrap-theme.min.css')|}];

[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo : string = "./logo.svg";

[@bs.module]
external fileDownload : (string, string) => unit = "js-file-download";

open Util;

open App_Data;

let page_header =
  <div className="page-header">
    <h1>
      (str("Munta"))
      (str(" "))
      <small> (str("Verified Timed Automata Model Checker")) </small>
    </h1>
  </div>;

let init_node = v => {invariant: "", node: v};

let init_edge = e => {guard: "", update: "", label: "", edge: e};

let onSelectNode = (v: node) => Js.log(v);

let onDeselectNode = () => Js.log("Deslected node");

let onCreateNode = (graph: single_state, x: float, y: float, id) => {
  let node: GraphView.node = {
    "id": id,
    "title": "New",
    "x": x,
    "y": y,
    "_type": GraphView.emptyType
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
      "_type": GraphView.emptyEdgeType
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
      "_type": GraphView.emptyEdgeType
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
  | LoadState(state)
  | UpdateState(Parse.network_in)
  | StartQuery
  | ReceiveReply(string)
  | AddAutomaton(string)
  | ChangeAutomaton(int, string)
  | CopyAutomaton(int)
  | DeleteAutomaton(int)
  | UpdateClocks(string)
  | UpdateVars(string)
  | UpdateFormula(string)
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
    render: self => {
      let className = "form-control btn btn-large btn-default";
      let className =
        className ++ (checked ? " active" : " btn-cursor disabled");
      <div className="form-group col-md-3">
        <label htmlFor="checkbox-button"> (str(desc)) </label>
        <input
          _type="button"
          id="checkbox-button"
          className
          value=(checked ? "yes" : "no")
          onClick=(_evt => checked ? onUncheck() : onCheck())
        />
      </div>;
    }
  };
};

module FormulaBox = {
  let component = ReasonReact.statelessComponent("Formula");
  let make = (~desc, ~placeholder, ~onChange, ~value, _children) => {
    ...component,
    render: _self =>
      <div className="form-group col-md-3">
        <label htmlFor="text-input"> (str(desc)) </label>
        <input
          _type="text"
          id="text-input"
          className="form-control"
          cols=20
          placeholder
          onChange=(evt => onChange(valueFromEvent(evt)))
          value
        />
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
          rows=3
          cols=15
          placeholder
          onChange=(evt => onChange(valueFromEvent(evt)))
          value
        />
      </div>
  };
};

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
      desc="Initial:"
      checked=(v.node##id == state.initial)
      key=("IN" ++ key_of_node(v))
      onCheck=(reduce(_evt => SetInitial))
      onUncheck=(reduce(_evt => UnsetInitial))
    />
  | _ => ReasonReact.nullElement
  };

let send_query = (~onSend, ~onReceive, ~query, ()) => {
  onSend();
  Js.Promise.(
    Fetch.fetchWithInit(
      "http://localhost:8000/test",
      Fetch.RequestInit.make(
        ~method_=Post,
        ~body=Fetch.BodyInit.make(query),
        ()
      )
    )
    |> then_(Fetch.Response.text)
    |> then_(text => onReceive(text) |> resolve)
    |> catch(_error =>
         Util.alert("Could not connect to verification server!") |> resolve
       )
  )
  |> ignore;
};

let default_filename = "automata.muntax";

let new_automaton_name = "New Automaton";

let update_node_type = (t: string, node: GraphView.node) => {
  "id": node##id,
  "title": node##title,
  "x": node##x,
  "y": node##y,
  "_type": t
};

let display_node = (is_initial, is_selected, v) => {
  let t =
    is_initial ?
      is_selected ? GraphView.specialChildType : GraphView.specialType :
      is_selected ? GraphView.emptyChildType : GraphView.emptyType;
  update_node_type(t, v);
};

let empty_automaton = {nodes: [], edges: [], selected: Nothing, initial: (-1)};

let make = (~initialState, _children) => {
  ...component,
  initialState: () => initialState,
  reducer: (action: action, {selected, automata} as state: state) => {
    let mk_upd = f =>
      switch selected {
      | None => ReasonReact.NoUpdate
      | Some(key) =>
        ReasonReact.Update({
          ...state,
          automata:
            assoc_upd_with(((label, x)) => (label, f(x)), key, automata),
          reply: None
        })
      };
    let mk_upd_with_id = f =>
      switch selected {
      | None => ReasonReact.NoUpdate
      | Some(key) =>
        ReasonReact.Update({
          ...state,
          nextId: state.nextId + 1,
          automata:
            assoc_upd_with(
              ((label, x)) => (label, f(state.nextId, x)),
              key,
              automata
            ),
          reply: None
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
    | LoadState(s) => ReasonReact.Update(s)
    | UpdateState(s) => ReasonReact.Update(merge_state(state, s))
    | StartQuery => ReasonReact.Update({...state, reply: None})
    | ReceiveReply(s) => ReasonReact.Update({...state, reply: Some(s)})
    | AddAutomaton(value) =>
      ReasonReact.Update({
        ...state,
        nextId: state.nextId + 1,
        selected: Some(state.nextId),
        automata: [(state.nextId, (value, empty_automaton)), ...automata],
        reply: None
      })
    | ChangeAutomaton(key, value) =>
      selected == Some(key) ?
        ReasonReact.Update({
          ...state,
          automata: assoc_upd_with(((_k, x)) => (value, x), key, automata)
        }) :
        List.mem_assoc(key, automata) ?
          ReasonReact.Update({
            ...state,
            selected: Some(key),
            automata: assoc_upd_with(((_k, x)) => (value, x), key, automata),
            reply: None
          }) :
          ReasonReact.NoUpdate
    | CopyAutomaton(key) =>
      ReasonReact.Update(
        {
          let (name, automaton) = List.assoc(key, automata);
          let names = List.map(((_, (name, _))) => name, automata);
          let name = Util.make_new_name(names, name);
          let new_key = state.nextId;
          {
            ...state,
            nextId: state.nextId + 1,
            automata: [(new_key, (name, automaton)), ...automata],
            reply: None
          };
        }
      )
    | DeleteAutomaton(key) =>
      ReasonReact.Update({
        ...state,
        selected: None,
        automata: List.remove_assoc(key, automata),
        reply: None
      })
    | UpdateClocks(s) => ReasonReact.Update({...state, clocks: s, reply: None})
    | UpdateVars(s) => ReasonReact.Update({...state, vars: s, reply: None})
    | SetInitial =>
      mk_upd(automaton =>
        {...automaton, initial: selected_node(automaton.selected).node##id}
      )
    | UpdateFormula(s) =>
      ReasonReact.Update({...state, formula: s, reply: None})
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
            "_type": GraphView.emptyType
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
    | CreateNode(x, y) =>
      mk_upd_with_id((id, state) => onCreateNode(state, x, y, id))
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
    let mk_render = f =>
      switch state.selected {
      | None => ReasonReact.nullElement
      | Some(key) => List.assoc(key, state.automata) |> snd |> f
      };
    let compiled = state_out(state) |> Rename.parse_compile;
    <div className="container">
      page_header
      <div>
        (
          mk_render(state =>
            <div className="graph-panel">
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
                nodes=(
                  List.map(
                    v =>
                      display_node(
                        v.node##id == state.initial,
                        switch state.selected {
                        | Node(w) => v === w
                        | _ => false
                        },
                        v.node
                      ),
                    state.nodes
                  )
                )
                edges=(List.map(e => e.edge, state.edges))
                selected=(selected_to_view(state.selected))
                graphControls=false
                enableFocus=true
              />
            </div>
          )
        )
        <div className="row">
          <Declaration
            desc="Clocks:"
            placeholder="Clock Declarations\nExample: c_1, c_2, c_3"
            value=state.clocks
            onChange=(reduce(evt => UpdateClocks(evt)))
          />
          <Declaration
            desc="Variables:"
            placeholder="Declarations of integer variables\nExample: x[-10:10], y[0:3]"
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
          onAdd=(reduce(() => AddAutomaton(new_automaton_name)))
          onChangeFocus=(
            reduce(k =>
              ChangeAutomaton(k, List.assoc(k, state.automata) |> fst)
            )
          )
          onCopy=(reduce(x => CopyAutomaton(x)))
          onDelete=(reduce(x => DeleteAutomaton(x)))
          onUpdate=(reduce(((k, v)) => ChangeAutomaton(k, v)))
          items=(
            List.map(((key, (label, _v))) => (key, label), state.automata)
            |> List.rev
          )
          selected=state.selected
          desc="Automata:"
        />
      </div>
      <div className="row">
        <FormulaBox
          desc="Formula:"
          placeholder="Formula"
          value=state.formula
          onChange=(reduce(evt => UpdateFormula(evt)))
        />
      </div>
      (
        switch compiled {
        | Error.Error(_) => ReasonReact.nullElement
        | Error.Result((_, _, r)) =>
          <div className="btn-toolbar btn-toolbar-lg" role="toolbar">
            <div className="btn-group btn-group-lg mr-2" role="group">
              <input
                _type="button"
                className="btn btn-primary"
                value="Save"
                onClick=(
                  _evt =>
                    state
                    |> Serialize.state
                    |> Json.stringify
                    |> (s => fileDownload(s, default_filename))
                )
              />
            </div>
            <div className="btn-group btn-group-lg mr-2" role="group">
              <input
                _type="button"
                className="btn btn-info"
                value="Check input"
                onClick=(
                  _evt => {
                    reduce(
                      _evt =>
                        UpdateState(
                          state
                          |> state_out
                          |> Parse.compile
                          |> Error.the_result
                          |> Parse.show_network
                        ),
                      ()
                    );
                    reduce(_evt => Deselect, ());
                  }
                )
              />
            </div>
            <div className="btn-group btn-group-lg" role="group">
              <input
                _type="button"
                className="btn btn-success"
                value="Verify"
                onClick=(
                  _evt =>
                    send_query(
                      ~onSend=reduce(() => StartQuery),
                      ~onReceive=reduce(s => ReceiveReply(s)),
                      ~query=Print_munta.print(r),
                      ()
                    )
                )
              />
              <input
                _type="button"
                className="btn btn-success"
                value="Verify in your browser"
                onClick=(
                  reduce(_evt =>
                    ReceiveReply(Checker.convert_run_print(r, ()))
                  )
                )
              />
            </div>
          </div>
        }
      )
      (
        switch state.reply {
        | None => ReasonReact.nullElement
        | Some(s) =>
          <div className="output">
            <label htmlFor="verification-output">
              (str("Verification result:"))
            </label>
            <pre id="verification-output"> (str(s)) </pre>
          </div>
        }
      )
      <div className="output">
        <label htmlFor="compilation-output">
          (str("Result of parsing/compilation:"))
        </label>
        <pre id="compiliation-output">
          (state_out(state) |> Print_munta.rename_and_print |> str)
        </pre>
      </div>
    </div>;
  }
};