[@bs.module "react-sane-contenteditable"]
external reactClass: ReasonReact.reactClass = "default";

let make =
    (
      ~content: option(string)=?,
      ~editable: option(bool)=?,
      ~focus: option(bool)=?,
      ~maxLength: option(bool)=?,
      ~multiLine: option(bool)=?,
      ~sanitise: option(bool)=?,
      ~caretPosition: option([ | `Start | `End])=?,
      ~tagName: option(string)=?,
      ~innerRef: option(Js.Nullable.t(ReasonReact.reactRef) => unit)=?,
      ~onBlur: option(ReactEvent.Form.t => unit)=?,
      ~onKeyDown: option(ReactEvent.Keyboard.t => unit)=?,
      ~onKeyPress: option(ReactEvent.Keyboard.t => unit)=?,
      ~onPaste: option(ReactEvent.Clipboard.t => unit)=?,
      ~onChange: option((ReactEvent.Form.t, string) => unit)=?,
      ~onFocus: option(ReactEvent.Form.t => unit)=?,
      ~className: option(string)=?,
      ~style: option(ReactDOMRe.Style.t)=?,
      ~styled: option(bool)=?,
      children,
    ) =>
  ReasonReact.wrapJsForReason(
    ~reactClass,
    ~props={
      "content": content,
      "editable": editable,
      "focus": focus,
      "maxLength": maxLength,
      "multiLine": multiLine,
      "sanitise": sanitise,
      "caretPosition": caretPosition,
      "tagName": tagName,
      "innerRef": innerRef,
      "onBlur": onBlur,
      "onKeyDown": onKeyDown,
      "onKeyPress": onKeyPress,
      "onPaste": onPaste,
      "onChange": onChange,
      "onFocus": onFocus,
      "className": className,
      "style": style,
      "styled": styled,
    },
    children,
  );