open BlackTea;

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
    ]
  );

let component = ReasonReact.statelessComponent("MainScreen");

let make = (~model: RootModel.model, ~pushMsg, _children) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <UseHook
        hook=useStyles
        render={classes =>
          <div className=classes##root>
            <AppBar position=`Static>
              <Toolbar variant=`Dense>
                <Typography
                  variant=`H6 color=`Inherit className={classes##toolbarTitle}>
                  {"TreeBurst" |> ReasonReact.string}
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
            <TreeView
              groupId={
                switch (PM.PeersGroup.Id.ofString("aaa")) {
                | Some(x) => x
                | None => raise(Not_found)
                }
              }
              model
              pushMsg
            />
          </div>
        }
      />
    ),
};