[@bs.module "react-contenteditable"]
external reactClass: ReasonReact.reactClass = "default";

let make =
    (
      ~html: option(string)=?,
      ~onChange: option(ReactEvent.Form.t => unit)=?,
      ~onBlur: option(ReactEvent.Form.t => unit)=?,
      ~onKeyDown: option(ReactEvent.Keyboard.t => unit)=?,
      ~disabled: option(bool)=?,
      ~tagName: option(string)=?,
      ~className: option(string)=?,
      ~style: option(ReactDOMRe.Style.t)=?,
      ~innerRef: option(Js.Nullable.t(ReasonReact.reactRef) => unit)=?,
      ~focus: option(bool)=?,
      ~onKeyPress: option(ReactEvent.Keyboard.t => unit)=?,
      ~onFocus: option(ReactEvent.Form.t => unit)=?,
      children,
    ) =>
  ReasonReact.wrapJsForReason(
    ~reactClass,
    ~props={
      "html": html,
      "onChange": onChange,
      "onBlur": onBlur,
      "onKeyDown": onKeyDown,
      "disabled": disabled,
      "tagName": tagName,
      "className": className,
      "style": style,
      "innerRef": innerRef,
      "focus": focus,
      "onKeyPress": onKeyPress,
      "onFocus": onFocus,
    },
    children,
  );