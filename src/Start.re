[@bs.module] external test: string = "./example/simple.muntax";

[@bs.module] external fddi: string = "./example/HDDI_02.muntax";

[@bs.module] external light_switch: string = "./example/light_switch.muntax";

let is_chrome: bool = [%bs.raw {| (window.chrome != undefined) |}];

let best_used_on_chrome =
  <div className="alert alert-info" role="alert">
    (Util.str("This application is best used with Google Chrome."))
  </div>;

let page_header =
  <div className="page-header">
    <h1>
      (Util.str("Munta"))
      (Util.str(" "))
      <small> (Util.str("Verified Timed Automata Model Checker")) </small>
    </h1>
  </div>;

let examples = [
  ("Simple", test),
  ("Light Switch", light_switch),
  ("FDDI token ring protocol", fddi),
];

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
  reply: None,
  show_help: false,
};

[@react.component]
module ExampleURLs = {
  [@react.component]
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
          |> ignore;
        <a className="list-group-item" onClick=send> (Util.str(desc)) </a>;
      } /* TODO: Error handling */
    };
  };
  let component = ReasonReact.statelessComponent("ExampleURLs");
  let make = (~examples, ~onLoad, _children) => {
    ...component,
    render: _self => {
      let mk = (i, (desc, href)) =>
        <LoadFromURL desc href onLoad key=(string_of_int(i)) />;
      <div className="list-group">
        (List.mapi(mk, examples) |> Array.of_list |> React.array)
      </div>;
    },
  };
};

let load_file = (~send, file) =>
  switch (Deserialize.decode(file)) {
  | None => Util.alert("Error while reading file") /* TODO: better error indication */
  | Some(state) => send(LoadState(state))
  };

let read_file_on_drop = [%raw
  {|
    function(onDrop, files) {
      var reader = new FileReader();
      reader.onload = (function(e) { onDrop(e.target.result); });
      reader.readAsText(files[0]);
    }
  |}];

let make = _children => {
  ...component,
  initialState: () => Started,
  reducer: (action: action, _state: state) =>
    switch (action) {
    | LoadState(state) => ReasonReact.Update(Initialized(state))
    },
  render: ({send, state, handle}) =>
    switch (state) {
    | Initialized(state) => <App initialState=state />
    | Started =>
      <div className="container">
        page_header
        (is_chrome ? React.null : best_used_on_chrome)
        <div className="jumbotron">
          <h1 className="display-3"> (Util.str("Get started")) </h1>
          <h3 className="display-3"> (Util.str("Load a file")) </h3>
          <div className="dropzone">
            <ReactDropzone
              accept=(ReactDropzone.Single(".muntax"))
              onDropAccepted=(files => read_file_on_drop(load_file(~send), files))
            >
              ...{(
                {
                  getInputProps,
                  getRootProps,
                },
              ) => {
                let inputProps = getInputProps();
                let rootProps = getRootProps();
                <div
                  onBlur={rootProps.onBlur}
                  onDragEnter={rootProps.onDragEnter}
                  onDragLeave={rootProps.onDragLeave}
                  onDragOver={rootProps.onDragOver}
                  onDragStart={rootProps.onDragStart}
                  onDrop={rootProps.onDrop}
                  onFocus={rootProps.onFocus}
                  onKeyDown={rootProps.onKeyDown}
                  ref={rootProps.ref}
                  tabIndex={rootProps.tabIndex}>
                      <div>
                        <label onClick={rootProps.onClick} className="lead">
                          {"Drop a file here, or click to select a file to upload." |> ReasonReact.string}
                        </label>
                        <input
                          autoComplete={inputProps.autoComplete}
                          onChange={inputProps.onChange}
                          onClick={inputProps.onClick}
                          ref={inputProps.ref}
                          style={inputProps.style}
                          tabIndex={inputProps.tabIndex}
                          type_={inputProps.type_}
                          multiple={inputProps.multiple}
                        />
                      </div>
                </div>;
              }}
            </ReactDropzone>
          </div>
          <h3 className="display-3">
            (Util.str("or "))
            <input
              type_="button"
              className="btn btn-lg btn-primary"
              value="Create new"
              onClick=(_evt => send(LoadState(empty_state)))
            />
          </h3>
          <h3 className="display-3">
            (Util.str("or start from an example:"))
          </h3>
          <ExampleURLs onLoad=(load_file(~send)) examples />
        </div>
      </div>
    },
};