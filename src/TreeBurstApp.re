module PMGui = PocketMeshPeerMaterialUi;

module ModelProvider = {
  let make =
    BlackTea.ReactProvider.createMake(
      ~name="ModelProvider",
      ~store=RootModel.store,
    );
};

module Styles = {
  open Css;
  let root =
    style([
      position(`absolute),
      top(`zero),
      right(`zero),
      bottom(`zero),
      left(`zero),
      display(`flex),
      children([flex(1)]),
    ]);
};

module App = {
  let component = ReasonReact.statelessComponent("App");

  let make = (~model: RootModel.model, ~pushMsg, _children) => {
    ...component,
    render: _self =>
      <ThemeProvider>
        <div className=Styles.root>
          {switch (model.route) {
           | Editor => <EditorScreen model pushMsg />
           | Conflict(groupId, variant) =>
             <ConflictScreen groupId variant model pushMsg />
           | P2P =>
             <PMGui.PeerScreens
               core={model.p2p |> PM.State.classify}
               model={model.p2pGui}
               pushMsg={msg => msg |> RootModel.p2pGuiMsgToMsg |> pushMsg}
             />
           }}
        </div>
      </ThemeProvider>,
  };
};

ReactDOMRe.renderToElementWithId(<ModelProvider component=App.make />, "app");