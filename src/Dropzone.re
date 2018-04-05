[@bs.module "react-dropzone"] external dropzone : ReasonReact.reactClass = "default";

let make =
    (
        ~onDrop: string => unit,
        ~accept: option(string) =?,
        ~className: option(string) =?,
    ) => {
        let read_file_on_drop = [%raw {|
            function(onDrop, files) {
                var reader = new FileReader();
                reader.onload = (function(e) { onDrop(e.target.result); });
                reader.readAsText(files[0]);
            }
        |}];
        ReasonReact.wrapJsForReason(
            ~reactClass=dropzone,
            ~props={
                "onDropAccepted": files => read_file_on_drop(onDrop, files),
                "accept": Js.Nullable.from_opt(accept),
                "className": Js.Nullable.from_opt(className),
                "multiple": Js.false_
            }
        )
    };