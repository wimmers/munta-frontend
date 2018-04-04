[@bs.module] external test: string = "./example/test.muntax";

type state =
  | Initialized(App_Data.state)
  | Started;

type action = LoadState(App_Data.state);

let component = ReasonReact.reducerComponent("StartScreen");

let empty_state: App_Data.state = {
    automata: [],
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
            let send = _evt => Js.Promise.(
                Fetch.fetch(href)
                |> then_(Fetch.Response.text)
                |> then_(text => onLoad(text) |> resolve)
            ) |> ignore; /* TODO: Error handling */
            <a
                className="list-group-item"
                onClick=send
            >
                (Util.str(desc))
            </a>
            }
        };
    };

    let component = ReasonReact.statelessComponent("ExampleURLs");
    let make = (~examples, ~onLoad, _children) => {
        ...component,
        render: _self => {
        let mk = (i, (desc, href)) =>
            <LoadFromURL
            desc
            href
            onLoad
            key=string_of_int(i)
            />;
        <div className="list-group">
            (List.mapi(mk, examples) |> Array.of_list |> ReasonReact.arrayToElement)
        </div>
        }
    }
};

let load_file = (~reduce, file) => switch (Deserialize.decode(file)) {
    | None => Js.log("Error while reading file") /* TODO: better error indication */
    | Some(state) => reduce(() => LoadState(state))()
};

let make = _children => {
    ...component,
    initialState: () => Started,
    reducer: (action: action, _state: state) => switch (action) {
    | LoadState(state) => ReasonReact.Update(Initialized(state));
    },
    render: ({reduce, state, handle}) => switch (state) {
    | Initialized(state) => <App initialState=state />
    | Started =>
        <div className="container">
        App.page_header
        <div className="jumbotron">
            <h1 className="display-3">(Util.str("Get started"))</h1>
            <h3 className="display-3">(Util.str("Load a file"))</h3>
            <div className="dropzone">
            <Dropzone
                accept=".muntax"
                onDrop=load_file(~reduce)
            >
                <p>(Util.str("Drop a file here, or click to select a file to upload."))</p>
            </Dropzone>
            </div>
            <h3 className="display-3">(Util.str("or "))
            <input
                _type="button"
                className="btn btn-lg btn-primary"
                value="Create new"
                onClick=reduce(
                  _evt => LoadState(empty_state)
                )
            />
            </h3>
            <h3 className="display-3">(Util.str("or start from an example:"))</h3>
            <ExampleURLs
                onLoad=load_file(~reduce)
                examples=[("Test1", test), ("Test2", test)]
            />
        </div>
        </div>
    }
};