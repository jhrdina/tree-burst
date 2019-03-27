type state = {contentEditableSize: ref(option((int, int)))};
type retainedProps = {hasConflict: bool};

let maybeNotifySizeChange =
    (self: ReasonReact.self('a, 'b, 'c), hasConflict, onSizeChange) => {
  switch (self.state.contentEditableSize^) {
  | Some((width, height)) =>
    onSizeChange((width + (hasConflict ? 32 : 0), height))
  | None => ()
  };
};

module Styles = {
  open Css;
  let nodePaddingWidth = 8;
  let nodePaddingHeight = 6;
  let nodeSelectedBorderWidth = 2;

  let root =
    style([
      display(`flex),
      backgroundColor(`hex("ffffff")),
      color(`rgba((0, 0, 0, 0.87))),
      fontFamily("\"Roboto\", \"Helvetica\", \"Arial\", sans-serif"),
      fontSize(`rem(0.875)),
      fontWeight(`normal),
      lineHeight(`abs(1.428571429)),
      letterSpacing(`em(0.01071)),
      borderRadius(`px(4)),
      transition(~duration=300, "transform"),
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
    ]);

  let contentEditable =
    style([
      minHeight(`em(1.0)),
      padding2(~h=`px(nodePaddingWidth), ~v=`px(nodePaddingHeight)),
      focus([
        padding2(
          ~h=`px(nodePaddingWidth - nodeSelectedBorderWidth),
          ~v=`px(nodePaddingHeight - nodeSelectedBorderWidth),
        ),
        outlineStyle(`none),
        border(`px(nodeSelectedBorderWidth), `solid, `hex("63ccff")),
      ]),
    ]);
  let conflictButton =
    style([
      backgroundColor(`hex("f44336")) |> important,
      color(`hex("ffffff")) |> important,
      minWidth(`zero) |> important,
      padding(`px(4)) |> important,
      borderTopLeftRadius(`zero) |> important,
      borderBottomLeftRadius(`zero) |> important,
      borderTopRightRadius(`px(4)),
      borderBottomRightRadius(`px(4)),
      hover([important(backgroundColor(`hex("d32f2f")))]),
    ]);
};

let component:
  ReasonReact.componentSpec(
    state,
    ReasonReact.stateless,
    retainedProps,
    ReasonReact.noRetainedProps,
    ReasonReact.actionless,
  ) =
  ReasonReact.reducerComponentWithRetainedProps("Node");

let make =
    (
      ~className="",
      ~style=ReactDOMRe.Style.make(),
      ~text,
      ~selected=false,
      ~hasConflict=false,
      ~onConflictClick=_ => (),
      ~onChange=(_, _) => (),
      ~onSizeChange=_ => (),
      ~onBlur=_ => (),
      ~onFocus=_ => (),
      ~onAddSibling=() => (),
      ~onAddChild=() => (),
      ~onDelete=() => (),
      _children,
    ) => {
  ...component,

  initialState: () => {contentEditableSize: ref(None)},

  retainedProps: {
    hasConflict: hasConflict,
  },

  didUpdate: ({oldSelf, newSelf}) =>
    if (oldSelf.retainedProps.hasConflict != newSelf.retainedProps.hasConflict) {
      maybeNotifySizeChange(newSelf, hasConflict, onSizeChange);
    },

  render: self =>
    <div className={[Styles.root, className] |> String.concat(" ")} style>
      <ContentEditable
        className=Styles.contentEditable
        text
        onBlur
        onFocus
        onKeyPress={e =>
          ReactEvent.Keyboard.(
            switch (e |> key) {
            | "Enter" when !shiftKey(e) && !ctrlKey(e) =>
              e |> preventDefault;
              e |> stopPropagation;
              onAddSibling();
            | _ => ()
            }
          )
        }
        onKeyDown={e =>
          ReactEvent.Keyboard.(
            switch (e |> key) {
            | "Enter" when e |> ctrlKey =>
              e |> preventDefault;
              e |> stopPropagation;
              onAddChild();
            | "Delete"
            | "Backspace" when text == "" =>
              e |> preventDefault;
              onDelete();
            | _ => ()
            }
          )
        }
        onChange
        onSizeChange={size => {
          self.state.contentEditableSize := Some(size);
          maybeNotifySizeChange(self, hasConflict, onSizeChange);
        }}
      />
      {hasConflict ?
         <MaterialUi.Button
           onClick=onConflictClick
           color=`Inherit
           className={
             [/*classes##conflictButton, */ Styles.conflictButton]
             |> String.concat(" ")
           }>
           <Icons.Conflict />
         </MaterialUi.Button> :
         ReasonReact.null}
    </div>,
};