open Parser;

open Test2;

let component = ReasonReact.statelessComponent("Test Results");

let make = _children => {
  ...component,
  render: _self =>
    <div>
      <li>
        (
          List.map(
            item =>
              <ul key=(fst(item))>
                <div> (Util.str(fst(item))) (Util.str(":")) </div>
                <div> (Util.str(snd(item))) </div>
              </ul>,
            evaluated_tests
          )
          |> Array.of_list
          |> ReasonReact.arrayToElement
        )
      </li>
    </div>
};
/* let input = Input.{text: "abcabc  abc", index: 0, whitespace: " "};

   let abc = stringParser("abc");

   let () = {
     input |> rep(abc) |> Js.log;
     Js.log("Test");
   };

   let () = test(); */