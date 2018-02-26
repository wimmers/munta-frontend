open Util;

type item = (int, string);

type state = list(item);

module Item = {
    let component = ReasonReact.statelessComponent("Item");
    let make = (~item: item, ~onClick, ~onChange, _children) => {
      ...component,
      render: _self =>
        <div className="form-group col-md-3">
          <textarea
            id="text-box"
            className="form-control"
            rows=7
            cols=20
            onClick=(_evt => onClick(fst(item)))
            onChange=(evt => onChange((fst(item), valueFromEvent(evt))))
            value=snd(item)
          />
        </div>
    };
  };

type action =
  | UpdateItem(int, string)
  | DeleteItem(int)
  | ChangeFocus(int)
  | AddItem;

let component = ReasonReact.reducerComponent("ItemList");

let make = (~onChangeFocus, ~onDelete, _children) => {
    ...component,
    initialState: () => [(0, "New Automaton")],
    reducer: (action: action, state: state) => {
        switch action {
        | UpdateItem(key, value) => {
            let item = (key, value);
            onChangeFocus(item);
            ReasonReact.Update(assoc_upd(key, value, state))
        }
        | DeleteItem(key) => {
            onDelete(key);
            ReasonReact.Update(List.remove_assoc(key, state))
        }
        | AddItem => {
            let new_item = (List.length(state), "New Automaton");
            onChangeFocus(new_item);
            ReasonReact.Update(List.append(state, [new_item]))
        }
        | ChangeFocus(key) => {
            onChangeFocus((key, List.assoc(key, state)));
            ReasonReact.NoUpdate
        }
        };
    },
    render: ({reduce, state, handle}) => {
        <div>
        (
            List.map(item => <Item
                key=(string_of_int(fst(item)))
                item
                onClick=(reduce(key => ChangeFocus(key)))
                onChange=(reduce(((x,y)) => UpdateItem(x,y)))
            />, state)
            |> Array.of_list
            |> ReasonReact.arrayToElement
        )
        <div onClick=(reduce(_evt => AddItem))>(str("+"))</div>
        </div>
    }
};