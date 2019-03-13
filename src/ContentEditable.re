[@bs.module "react-simple-contenteditable"]
external reactClass: ReasonReact.reactClass = "default";
Js.log(reactClass);

let make =
    (
      ~html: string,
      ~className: option(string)=?,
      ~onChange: option((ReactEvent.Form.t, string) => unit)=?,
      ~onBlur: option(ReactEvent.Form.t => unit)=?,
      ~onFocus: option(ReactEvent.Form.t => unit)=?,
      ~onKeyPress: option((ReactEvent.Keyboard.t, string) => unit)=?,
      ~contentEditable: option([ | `PlainTextOnly | `False | `True])=?,
      ~style: option(ReactDOMRe.Style.t)=?,
      children,
    ) =>
  ReasonReact.wrapJsForReason(
    ~reactClass,
    ~props={
      "html": html,
      "className": className,
      "onFocus": onFocus,
      "onBlur": onBlur,
      "onChange": onChange->Belt.Option.getWithDefault((_, _) => ()),
      "onKeyPress": onKeyPress->Belt.Option.getWithDefault((_, _) => ()),
      "contentEditable":
        Belt.Option.map(
          contentEditable,
          fun
          | `PlainTextOnly => "plaintext-only"
          | `True => "true"
          | `False => "false",
        ),
      "style": style,
    },
    children,
  );