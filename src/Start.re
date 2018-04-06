[@bs.module] external test : string = "./example/simple.muntax";

[@bs.module] external fddi : string = "./example/HDDI_02.muntax";

let is_chrome: Js.boolean = [%bs.raw {| (window.chrome != undefined) |}];

let is_chrome = Js.to_bool(is_chrome);

let best_used_on_chrome =
  <div className="alert alert-info" role="alert">
    (Util.str("This application is best used with Google Chrome."))
  </div>;

let examples = [("Simple", test), ("FDDI token ring protocol", fddi)];

type state =
  | Initialized(App_Data.state)
  | Started;

type action =
  | LoadState(App_Data.state);

let component = ReasonReact.reducerComponent("StartScreen");

let empty_state: App_Data.state = {
  automata: [],
  nextId: 0,
  selected: None,
  clocks: "",
  vars: "",
  formula: "",
  reply: None
};

module ExampleURLs = {
  module LoadFromURL = {
    let component = ReasonReact.statelessComponent("LoadFromURL");
    let make = (~desc, ~href, ~onLoad, _children) => {
      ...component,
      render: _self => {
        let send = _evt =>
          Js.Promise.(
            Fetch.fetch(href)
            |> then_(Fetch.Response.text)
            |> then_(text => onLoad(text) |> resolve)
          )
          |> ignore; /* TODO: Error handling */
        <a className="list-group-item" onClick=send> (Util.str(desc)) </a>;
      }
    };
  };
  let component = ReasonReact.statelessComponent("ExampleURLs");
  let make = (~examples, ~onLoad, _children) => {
    ...component,
    render: _self => {
      let mk = (i, (desc, href)) =>
        <LoadFromURL desc href onLoad key=(string_of_int(i)) />;
      <div className="list-group">
        (
          List.mapi(mk, examples) |> Array.of_list |> ReasonReact.arrayToElement
        )
      </div>;
    }
  };
};

let load_file = (~reduce, file) =>
  switch (Deserialize.decode(file)) {
  | None => Util.alert("Error while reading file") /* TODO: better error indication */
  | Some(state) => reduce(() => LoadState(state), ())
  };

let make = _children => {
  ...component,
  initialState: () => Started,
  reducer: (action: action, _state: state) =>
    switch action {
    | LoadState(state) => ReasonReact.Update(Initialized(state))
    },
  render: ({reduce, state, handle}) =>
    switch state {
    | Initialized(state) => <App initialState=state />
    | Started =>
      <div className="container">
        App.page_header
        (is_chrome ? ReasonReact.nullElement : best_used_on_chrome)
        <div className="jumbotron">
          <h1 className="display-3"> (Util.str("Get started")) </h1>
          <h3 className="display-3"> (Util.str("Load a file")) </h3>
          <div className="dropzone">
            <Dropzone
              accept=".muntax"
              onDrop=(load_file(~reduce))
              className="container dropzone lead">
              (
                Util.str(
                  "Drop a file here, or click to select a file to upload."
                )
              )
            </Dropzone>
          </div>
          <h3 className="display-3">
            (Util.str("or "))
            <input
              _type="button"
              className="btn btn-lg btn-primary"
              value="Create new"
              onClick=(reduce(_evt => LoadState(empty_state)))
            />
          </h3>
          <h3 className="display-3">
            (Util.str("or start from an example:"))
          </h3>
          <ExampleURLs onLoad=(load_file(~reduce)) examples />
        </div>
      </div>
    }
};