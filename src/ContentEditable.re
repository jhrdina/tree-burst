[@bs.module "./ContentEditable"]
external reactClass: ReasonReact.reactClass = "default";

let make =
    (
      ~text: option(string)=?,
      ~onChange: option((ReactEvent.Form.t, string) => unit)=?,
      ~onBlur: option(ReactEvent.Form.t => unit)=?,
      ~onKeyDown: option(ReactEvent.Keyboard.t => unit)=?,
      ~className: option(string)=?,
      ~style: option(ReactDOMRe.Style.t)=?,
      ~innerRef: option(Js.Nullable.t(ReasonReact.reactRef) => unit)=?,
      ~focus: option(bool)=?,
      ~onKeyPress: option(ReactEvent.Keyboard.t => unit)=?,
      ~onFocus: option(ReactEvent.Form.t => unit)=?,
      ~onSizeChange: option(((int, int)) => unit)=?,
      children,
    ) =>
  ReasonReact.wrapJsForReason(
    ~reactClass,
    ~props={
      "text": text,
      "onChange": onChange,
      "onBlur": onBlur,
      "onKeyDown": onKeyDown,
      "className": className,
      "style": style,
      "innerRef": innerRef,
      "focus": focus,
      "onKeyPress": onKeyPress,
      "onFocus": onFocus,
      "onSizeChange": onSizeChange,
    },
    children,
  );