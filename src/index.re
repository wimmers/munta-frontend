[%bs.raw {|require('./bootstrap/css/bootstrap.min.css')|}];

[%bs.raw {|require('./bootstrap/css/bootstrap-theme.min.css')|}];

[%bs.raw {|require('./index.css')|}];

[@bs.module "./registerServiceWorker"]
external register_service_worker: unit => unit = "default";

ReactDOMRe.renderToElementWithId(<Start />, "root");

register_service_worker();
