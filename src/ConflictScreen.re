open Infix;

// VIEW

let variantTitleBorder = "1px solid #cbcbcb";
let useStyles =
  MuiStylesHooks.makeWithTheme(_theme =>
    [
      {
        name: "root",
        styles:
          ReactDOMRe.Style.make(
            ~backgroundColor="#eeeeee",
            ~position="relative",
            ~display="flex",
            ~flexDirection="column",
            (),
          ),
      },
      {
        name: "appBar",
        styles:
          ReactDOMRe.Style.make(
            ~backgroundColor="#ffffff",
            ~color="#e53935",
            (),
          ),
      },
      {
        name: "toolbarLeftBtn",
        styles:
          ReactDOMRe.Style.make(~marginLeft="-16px", ~marginRight="10px", ()),
      },
      {
        name: "toolbarTitle",
        styles: ReactDOMRe.Style.make(~flexGrow="1", ()),
      },
      {
        name: "variantTitle",
        styles:
          ReactDOMRe.Style.make(
            ~backgroundColor="#ffffff",
            ~borderTop=variantTitleBorder,
            ~borderBottom=variantTitleBorder,
            ~lineHeight="1.64",
            ~padding="4px 24px",
            (),
          ),
      },
      {
        name: "variantTitleNote",
        styles:
          ReactDOMRe.Style.make(~fontWeight="normal", ~fontSize="12px", ()),
      },
      {
        name: "nodeWrapper",
        styles: ReactDOMRe.Style.make(~padding="12px 24px", ()),
      },
      {
        name: "node",
        styles: ReactDOMRe.Style.make(~display="inline-flex", ()),
      },
    ]
  );

let renderSubtrees = (~classes, ~model: RootModel.model, ~pushMsg, ~groupId) =>
  MaterialUi.(
    <>
      <Typography
        variant=`Subtitle2
        color=`TextSecondary
        className={classes##variantTitle}>
        {"Current value" |> ReasonReact.string}
      </Typography>
      <TreeView groupId model pushMsg />
      <Typography
        variant=`Subtitle2
        color=`TextSecondary
        className={classes##variantTitle}>
        {"Value suggested by XY" |> ReasonReact.string}
      </Typography>
      <TreeView groupId model pushMsg />
    </>
  );

let renderCurrentTitle = classes =>
  <MaterialUi.Typography
    variant=`Subtitle2
    color=`TextSecondary
    className={
      classes##variantTitle;
    }>
    {"Current value" |> ReasonReact.string}
    <span className={classes##variantTitleNote}>
      {" (you can edit this variant)" |> ReasonReact.string}
    </span>
  </MaterialUi.Typography>;

let renderTitleForVariant = (~key="", ~classes, peerAlias) =>
  <MaterialUi.Typography
    key
    variant=`Subtitle2
    color=`TextSecondary
    className={
      classes##variantTitle;
    }>
    {ReasonReact.string("Value suggested by " ++ peerAlias)}
  </MaterialUi.Typography>;

let renderNodesConflict =
    (~classes, ~model: RootModel.model, ~groupId, ~nodeId) => {
  model.p2p
  |> RootModel.p2pMatchWithIdentity
  |?>> (
    ((dbState, _runtimeState)) => {
      dbState
      |> PM.DbState.groups
      |> PM.PeersGroups.findOpt(groupId)
      |?>> PM.PeersGroup.content
      |?> Content.findNodeByIdSafe(nodeId)
      |?>> (
        node => {
          let currentText = node.text.value;
          let alternatives =
            PM.Peer.Id.Map.fold(
              (peerId, text, arr) => {
                let peerName =
                  dbState
                  |> PM.DbState.peers
                  |> PM.Peers.findOpt(peerId)
                  |?>> PocketMeshPeerMaterialUi.GuiUtils.getPeerVisibleName
                  |? (peerId |> PM.Peer.Id.toString);
                let key = {
                  peerId |> PM.Peer.Id.toString;
                };
                let addedEl =
                  <div key>
                    {renderTitleForVariant(~classes, peerName)}
                    <div className=classes##nodeWrapper>
                      <Node editable=false className=classes##node text />
                    </div>
                  </div>;

                arr |> Array.append([|addedEl|]);
              },
              node.text.conflicts,
              [||],
            )
            |> ReasonReact.array;
          <>
            {renderCurrentTitle(classes)}
            <div className=classes##nodeWrapper>
              <Node className=classes##node text=currentText />
            </div>
            alternatives
          </>;
        }
      )
      |? ReasonReact.null;
    }
  )
  |? ReasonReact.null;
};

let component = ReasonReact.statelessComponent("ConflictScreen");
let make =
    (
      ~model: RootModel.model,
      ~pushMsg,
      ~groupId,
      ~variant: Route.conflictVariant,
      _children,
    ) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <UseHook
        hook=useStyles
        render={classes =>
          <div className=classes##root>
            <AppBar className=classes##appBar position=`Static>
              <Toolbar variant=`Dense>
                <IconButton
                  className=classes##toolbarLeftBtn
                  color=`Inherit
                  onClick={_ => pushMsg(Route.Change(Editor))}>
                  <Icons.ArrowBack />
                </IconButton>
                <Typography
                  variant=`H6 color=`Inherit className={classes##toolbarTitle}>
                  {"Conflict solving" |> ReasonReact.string}
                </Typography>
              </Toolbar>
            </AppBar>
            {switch (variant) {
             | Text(nodeId) =>
               renderNodesConflict(~classes, ~model, ~groupId, ~nodeId)
             }}
          </div>
        }
      />
    ),
};