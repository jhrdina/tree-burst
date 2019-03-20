module Styles = {
  open Css;
  let nodePadding = 8;
  let nodeSelectedBorderWidth = 2;

  let root =
    style([
      minHeight(`em(1.0)),
      position(`absolute),
      backgroundColor(`hex("ffffff")),
      color(`rgba((0, 0, 0, 0.87))),
      padding(`px(nodePadding)),
      fontFamily("\"Roboto\", \"Helvetica\", \"Arial\", sans-serif"),
      fontSize(`rem(0.875)),
      fontWeight(`normal),
      // lineHeight(`abs(1.5)),
      letterSpacing(`em(0.01071)),
      boxShadows([
        boxShadow(
          ~x=`px(0),
          ~y=`px(1),
          ~blur=`px(5),
          ~spread=`px(0),
          `rgba((0, 0, 0, 0.2)),
        ),
        boxShadow(
          ~x=`px(0),
          ~y=`px(2),
          ~blur=`px(2),
          ~spread=`px(0),
          `rgba((0, 0, 0, 0.14)),
        ),
        boxShadow(
          ~x=`px(0),
          ~y=`px(3),
          ~blur=`px(1),
          ~spread=`px(-2),
          `rgba((0, 0, 0, 0.12)),
        ),
      ]),
      borderRadius(`px(4)),
      transition(~duration=300, "transform"),
      focus([
        padding(`px(nodePadding - nodeSelectedBorderWidth)),
        outlineStyle(`none),
        border(`px(nodeSelectedBorderWidth), `solid, `hex("63ccff")),
      ]),
    ]);
};

let component = ReasonReact.statelessComponent("Node");

let make =
    (
      ~pos as (x, y),
      ~text,
      ~selected=false,
      ~onChange=(_, _) => (),
      ~onSizeChange=_ => (),
      ~onBlur=_ => (),
      ~onFocus=_ => (),
      ~onAddSibling=() => (),
      ~onAddChild=() => (),
      _children,
    ) => {
  ...component,

  render: _self =>
    <ContentEditable
      className=Styles.root
      style={ReactDOMRe.Style.make(
        ~transform=
          "translate(" ++ Utils.pxOfInt(x) ++ ", " ++ Utils.pxOfInt(y) ++ ")",
        (),
      )}
      text
      onBlur
      onFocus
      onKeyPress={e =>
        switch (e |> ReactEvent.Keyboard.key) {
        | "Enter"
            when
              !ReactEvent.Keyboard.shiftKey(e)
              && !ReactEvent.Keyboard.ctrlKey(e) =>
          e |> ReactEvent.Keyboard.preventDefault;
          e |> ReactEvent.Keyboard.stopPropagation;
          onAddSibling();
        | _ => ()
        }
      }
      onKeyDown={e =>
        switch (e |> ReactEvent.Keyboard.key) {
        | "Enter" when e |> ReactEvent.Keyboard.ctrlKey =>
          e |> ReactEvent.Keyboard.preventDefault;
          e |> ReactEvent.Keyboard.stopPropagation;
          onAddChild();
        | _ => ()
        }
      }
      onChange
      onSizeChange
    />,
};