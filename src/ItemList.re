open Util;

type item = (int, string);

type state = list(item);

module Item = {
    let component = ReasonReact.statelessComponent("Item");
    let make = (~item: item, ~onClick, ~onChange, ~highlighted, _children) => {
      ...component,
      render: _self =>
        <div className="col-sm-2 text-box-outer">
          <input
            _type="text"
            className=("form-control text-box" ++ (highlighted ? " text-box-highlight" : ""))
            rows=1
            cols=5
            onClick=(_evt => onClick(fst(item)))
            onChange=(evt => onChange((fst(item), valueFromEvent(evt))))
            value=snd(item)
          />
        </div>
    };
  };

let component = ReasonReact.statelessComponent("ItemList");

let make = (~onAdd, ~onChangeFocus, ~onDelete, ~onUpdate, ~items, ~selected, ~desc, _children) => {
    ...component,
    render: ({reduce, state, handle}) => {
        <div>
        <label htmlFor="item-list"> (str(desc)) </label>
        <div className="form-group row" id="item-list">
        (
            List.map(item => <Item
                key=(string_of_int(fst(item)))
                item
                onClick=(key => onChangeFocus(key))
                onChange=(v => onUpdate(v))
                highlighted=(Some(fst(item)) == selected)
            />, items)
            |> Array.of_list
            |> ReasonReact.arrayToElement
        )
            <div className="col-sm-2">
                <input
                    _type="button"
                    className="btn btn-md btn-default"
                    value="Add Automaton"
                    onClick=(_evt => onAdd())
                />
            </div>
        </div>
        </div>
    }
};