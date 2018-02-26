/* Util */
let str = ReasonReact.stringToElement;

let valueFromEvent = evt : string => (
                                       evt
                                       |> ReactEventRe.Form.target
                                       |> ReactDOMRe.domElementToObj
                                     )##value;

let assoc_upd_with = (f, key) => List.map(((k, v)) => k == key ? (k, f(v)): (k, v));
let assoc_upd = (key, value) => assoc_upd_with(_v => value, key);

let the = x => switch(x){
| Some(x) => x
}