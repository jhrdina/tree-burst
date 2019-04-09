open Infix;

module PMGui = PocketMeshPeerMaterialUi;

// CONSTANTS

let appName = "TreeBurst";

// VIEW

let useStyles =
  MuiStylesHooks.makeWithTheme(_theme =>
    [
      {
        name: "root",
        styles:
          ReactDOMRe.Style.make(
            ~position="relative",
            ~display="flex",
            ~flexDirection="column",
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
        name: "toolbarRightBlock",
        styles:
          ReactDOMRe.Style.make(~marginLeft="10px", ~marginRight="-16px", ()),
      },
      {
        name: "loadingMessage",
        styles:
          ReactDOMRe.Style.make(
            ~padding="24px",
            ~flexGrow="1",
            ~backgroundColor="#eeeeee",
            (),
          ),
      },
      {
        name: "shortcuts",
        styles:
          ReactDOMRe.Style.make(
            ~position="absolute",
            ~bottom="0px",
            ~left="0px",
            ~right="0px",
            ~padding="16px",
            ~pointerEvents="none",
            (),
          ),
      },
    ]
  );

let rs = ReasonReact.string;

let component = ReasonReact.statelessComponent("MainScreen");

let make = (~model: RootModel.model, ~pushMsg, _children) => {
  ...component,
  render: _self => {
    let p2pTagged = model.p2p |> PM.State.classify;
    let title =
      switch (p2pTagged, model.openedGroup) {
      | (HasIdentity(dbState, _runtimeState), Some(groupId)) =>
        dbState
        |> PM.DbState.groups
        |> PM.PeersGroups.findOpt(groupId)
        |?>> PMGui.GuiUtils.getPeerGroupVisibleName
        |? (groupId |> PM.PeersGroup.Id.toString)
      | (HasIdentity(_, _), None)
      | (WaitingForDbAndIdentity(_), _) => appName
      };
    MaterialUi.(
      <UseHook
        hook=useStyles
        render={classes =>
          <div className=classes##root>
            <AppBar position=`Static>
              <Toolbar variant=`Dense>
                <Typography
                  variant=`H6 color=`Inherit className={classes##toolbarTitle}>
                  {title |> ReasonReact.string}
                </Typography>
                <div className=classes##toolbarRightBlock>
                  <IconButton
                    color=`Inherit onClick={_ => pushMsg(Route.Change(P2P))}>
                    <PocketMeshPeerMaterialUi.GlobalIcon
                      signalState=Connected
                      peerState=Online
                    />
                  </IconButton>
                  <IconButton color=`Inherit className={classes##leftToolBtn}>
                    <Icons.MoreVert />
                  </IconButton>
                </div>
              </Toolbar>
            </AppBar>
            {switch (p2pTagged, model.openedGroup) {
             | (HasIdentity(dbState, _), Some(groupId)) =>
               //  TODO: pass only dbState instead of model
               <TreeView groupId model pushMsg />
             | (HasIdentity(dbState, _), None) =>
               <Typography variant=`Body2 className=classes##loadingMessage>
                 {"There is no group opened." |> ReasonReact.string}
               </Typography>
             | (WaitingForDbAndIdentity(_), _) =>
               <Typography variant=`Body2 className=classes##loadingMessage>
                 {"Loading..." |> ReasonReact.string}
               </Typography>
             }}
            <Typography
              variant=`Caption
              color=`TextSecondary
              className=classes##shortcuts>
              <b> {"Ctrl + Enter: " |> rs} </b>
              {"Add child, " |> rs}
              <b> {"Shift + Enter: " |> rs} </b>
              {"New line in node, " |> rs}
              <b> {"Backspace/Delete" |> rs} </b>
              {" (on empty node): Remove node" |> rs}
            </Typography>
          </div>
        }
      />
    );
  },
};