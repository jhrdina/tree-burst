type state = {
  rootRef: ref(option(ReasonReact.reactRef)),
  doReportSize: ref(bool),
};

type retainedProps = {text: string};

type action;

let getSizeOfRef: ReasonReact.reactRef => (int, int) =
  r => {
    let rect = ReasonReact.refToJsObj(r)##getBoundingClientRect();
    (rect##width, rect##height);
  };

module Styles = {
  open Css;
  let nodePadding = 8;
  let nodeSelectedBorderWidth = 2;

  let root =
    style([
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

let component:
  ReasonReact.componentSpec(
    'state,
    ReasonReact.stateless,
    retainedProps,
    ReasonReact.noRetainedProps,
    ReasonReact.actionless,
  ) =
  ReasonReact.reducerComponentWithRetainedProps("Node");

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

  initialState: () => {rootRef: ref(None), doReportSize: ref(false)},

  reducer: (_action, _s) => ReasonReact.NoUpdate,

  retainedProps: {
    text: text,
  },

  didUpdate: ({oldSelf, newSelf}) =>
    switch (newSelf.state.rootRef^) {
    | Some(rootRef)
        when
          /*newSelf.state.doReportSize^
            &&*/ oldSelf.
            retainedProps.
            text
          != newSelf.retainedProps.text =>
      rootRef |> getSizeOfRef |> onSizeChange
    | Some(_)
    | None => ()
    },

  render: self =>
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
      innerRef={r => {
        let maybeRef = Js.Nullable.toOption(r);
        self.state.rootRef := maybeRef;
        switch (maybeRef) {
        | Some(newRef) => newRef |> getSizeOfRef |> onSizeChange
        | None => ()
        };
      }}
    />,
};