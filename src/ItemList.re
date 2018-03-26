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

let component = ReasonReact.statelessComponent("ItemList");

let make = (~onAdd, ~onChangeFocus, ~onDelete, ~onUpdate, ~items, _children) => {
    ...component,
    render: ({reduce, state, handle}) => {
        <div>
        (
            List.map(item => <Item
                key=(string_of_int(fst(item)))
                item
                onClick=(key => onChangeFocus(key))
                onChange=(v => onUpdate(v))
            />, items)
            |> Array.of_list
            |> ReasonReact.arrayToElement
        )
        <div onClick=(_evt => onAdd())>(str("+"))</div>
        </div>
    }
};