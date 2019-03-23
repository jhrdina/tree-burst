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
    ]
  );

let component = ReasonReact.statelessComponent("ConflictScreen");

let make = (~model: RootModel.model, ~pushMsg, _children) => {
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
            <Typography
              variant=`Subtitle2
              color=`TextSecondary
              className={classes##variantTitle}>
              {"Current value" |> ReasonReact.string}
            </Typography>
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
            <Typography
              variant=`Subtitle2
              color=`TextSecondary
              className={classes##variantTitle}>
              {"Value suggested by XY" |> ReasonReact.string}
            </Typography>
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