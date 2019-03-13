open Infix;

type editedNode = {
  nodeId: string,
  text: string,
};

type state = {v: string};

type action =
  | UpdateNodeText(string);

let pxOfInt = x => string_of_int(x) ++ "px";

let nodePadding = 8;
let nodeSelectedBorderWidth = 2;

let useStyles =
  MuiStylesHooks.makeWithTheme(theme =>
    [
      {
        name: "root",
        styles:
          ReactDOMRe.Style.make(
            ~position="relative",
            ~backgroundColor="#eeeeee",
            ~flexGrow="1",
            (),
          ),
      },
    ]
  );

module Styles = {
  open Css;
  let node =
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
      focus([
        padding(`px(nodePadding - nodeSelectedBorderWidth)),
        outlineStyle(`none),
        border(`px(nodeSelectedBorderWidth), `solid, `hex("63ccff")),
      ]),
    ]);
};

let renderNode =
    (
      ~classes,
      ~pos as (x, y),
      ~text,
      ~selected=false,
      ~onChange=(_, _) => (),
      ~onBlur=_ => (),
      ~pushMsg,
      (),
    ) =>
  MaterialUi.(
    <ContentEditable
      className=Styles.node
      style={ReactDOMRe.Style.make(~top=pxOfInt(y), ~left=pxOfInt(x), ())}
      html=text
      contentEditable=`PlainTextOnly
      onBlur
      onFocus={_ => Js.log2("focused", text)}
      onKeyPress={(e, s) =>
        switch (e |> ReactEvent.Keyboard.key) {
        | "Enter" when !ReactEvent.Keyboard.shiftKey(e) =>
          e |> ReactEvent.Keyboard.preventDefault;
          e |> ReactEvent.Keyboard.stopPropagation;
        // pushMsg(PressedEnter);
        | x => Js.log2("asdf", x)
        }
      }
      onChange
    />
  );

let component = ReasonReact.reducerComponent("TreeView");

let make = (~model: RootModel.model, ~pushMsg, _children) => {
  ...component,

  initialState: () => {v: "Hello world"},

  reducer: (action, s) =>
    ReasonReact.Update(
      switch (action) {
      | UpdateNodeText(text) => {...s, v: text}
      },
    ),

  render: self => {
    let groupId =
      switch (PM.PeersGroup.Id.ofString("aaa")) {
      | Some(groupId) => groupId
      | None => raise(Not_found)
      };
    MaterialUi.(
      <UseHook
        hook=useStyles
        render={classes =>
          <div className=classes##root>
            {renderNode(
               ~classes,
               ~pos=(100, 100),
               ~text=self.state.v,
               ~pushMsg,
               ~onChange=(_, v) => self.send(UpdateNodeText(v)),
               (),
             )}
            {renderNode(
               ~classes,
               ~pos=(250, 50),
               ~selected=true,
               ~text="Other node",
               ~pushMsg,
               (),
             )}
            {switch (model.p2p |> PM.State.classify) {
             | HasIdentity(dbState, runtimeState) =>
               dbState
               |> PM.DbState.groups
               |> PM.PeersGroups.findOpt(groupId)
               |?>> PM.PeersGroup.content
               |?> (
                 content =>
                   {content
                    |> Content.getRootNodeId
                    |?> (
                      rootNodeId =>
                        PM.Crdt.Json.(
                          content
                          |> Content.findNodeById(rootNodeId)
                          |?> Map.get(Content.NodeKeys.text)
                          |?> asString
                        )
                        |?>> (
                          text =>
                            renderNode(
                              ~classes,
                              ~pos=(100, 170),
                              ~selected=true,
                              ~text,
                              ~pushMsg,
                              ~onChange=
                                (e, v) =>
                                  pushMsg(
                                    RootModel.P2PMsg(
                                      PM.Msg.updateGroupContent(
                                        groupId,
                                        content
                                        |> Content.updateNodeText(
                                             rootNodeId,
                                             v,
                                           ),
                                      ),
                                    ),
                                  ),
                              (),
                            )
                        )
                    )}
               )
               |? ReasonReact.null
             | WaitingForDbAndIdentity(_) => ReasonReact.null
             }}
          </div>
        }
      />
    );
  },
};