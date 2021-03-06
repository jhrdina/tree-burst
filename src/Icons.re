open MaterialUi.SvgIcon;

module type IconRef = {let reactClass: ReasonReact.reactClass;};

module BuildIcon = (Base: IconRef) => {
  let reactClass = Base.reactClass;
  let make =
      (
        ~className: option(string)=?,
        ~color: option(color)=?,
        ~component:
           option(
             [
               | `String(string)
               | `Callback('genericCallback)
               | `ObjectGeneric(Js.t({..}))
             ],
           )=?,
        ~fontSize: option(fontSize)=?,
        ~nativeColor: option(string)=?,
        ~titleAccess: option(string)=?,
        ~viewBox: option(string)=?,
        ~classes: option(Classes.t)=?,
        ~style: option(ReactDOMRe.Style.t)=?,
        children,
      ) =>
    ReasonReact.wrapJsForReason(
      ~reactClass=Base.reactClass,
      ~props=
        makeProps(
          ~className?,
          ~color=?color->(Belt.Option.map(v => colorToJs(v))),
          ~component=?
            component->(
                         Belt.Option.map(v =>
                           MaterialUi_Helpers.unwrapValue(v)
                         )
                       ),
          ~fontSize=?fontSize->(Belt.Option.map(v => fontSizeToJs(v))),
          ~nativeColor?,
          ~titleAccess?,
          ~viewBox?,
          ~classes=?Belt.Option.map(classes, v => Classes.to_obj(v)),
          ~style?,
          (),
        ),
      children,
    );
};

module ArrowBack =
  BuildIcon({
    [@bs.module "@material-ui/icons/ArrowBack"]
    external reactClass: ReasonReact.reactClass = "default";
  });

module MoreVert =
  BuildIcon({
    [@bs.module "@material-ui/icons/MoreVert"]
    external reactClass: ReasonReact.reactClass = "default";
  });

module Conflict = {
  let component = ReasonReact.statelessComponent("Conflict");
  let make = (~className="", _children) => {
    ...component,
    render: _self =>
      <MaterialUi.SvgIcon className>
        <path d="M8 5v8h2v7l5-9h-3l3-6z" />
      </MaterialUi.SvgIcon>,
  };
};