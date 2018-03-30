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

let make = (
    ~onAdd, ~onChangeFocus, ~onCopy, ~onDelete, ~onUpdate, ~items, ~selected, ~desc, _children
) => {
    ...component,
    render: ({reduce, state, handle}) => {
        let on_selected(f) = (_evt => switch selected {
        | None => ()
        | Some(key) => f(key)
        });
        let disabled_class = "btn btn-default" ++ (selected == None ? " disabled" : "");
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
            <div className="col-sm-3 btn-group btn-group-md" role="group">
                <input
                    _type="button"
                    className="btn btn-default"
                    value="Add"
                    onClick=(_evt => onAdd())
                />
                <input
                    _type="button"
                    className=disabled_class
                    value="Copy"
                    onClick=(on_selected(onCopy))
                />
                <input
                    _type="button"
                    className=disabled_class
                    value="Delete"
                    onClick=(on_selected(onDelete))
                />
            </div>
        </div>
        </div>
    }
};