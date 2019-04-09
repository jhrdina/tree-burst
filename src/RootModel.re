open BlackTea;
module PMGui = PocketMeshPeerMaterialUi;

type model = {
  p2p: PM.State.t,
  p2pGui: PMGui.PeerScreens.model,
  route: Route.t,
  openedGroup: option(PM.PeersGroup.Id.t),
};

type Msg.t +=
  | P2PMsg(PM.Msg.t)
  | P2PGuiMsg(PMGui.Msg.t);

let p2pMsgToMsg = m => P2PMsg(m);
let p2pGuiMsgToMsg = m => P2PGuiMsg(m);

// HELPERS

let groupExists = (groupId, dbState) =>
  dbState
  |> PM.DbState.groups
  |> PM.PeersGroups.findOpt(groupId)
  |> Belt.Option.isSome;

let findSomeGroup = dbState =>
  dbState
  |> PM.DbState.groups
  |> PM.PeersGroups.fold(
       (selectedGroup, group) =>
         switch (selectedGroup) {
         | None => Some(group |> PM.PeersGroup.id)
         | Some(_) => selectedGroup
         },
       None,
     );

// UPDATES

let updateOpenedGroup = (~p2pTagged: PM.State.taggedT, openedGroup, msg) => {
  let newOpenedGroup =
    switch (msg) {
    | P2PGuiMsg(PMGui.Msg.ClickedOpenGroup(groupId)) => Some(groupId)
    | _ => openedGroup
    };

  switch (p2pTagged, newOpenedGroup, openedGroup) {
  | (HasIdentity(dbState, _), Some(groupId), _)
      when dbState |> groupExists(groupId) => newOpenedGroup
  | (HasIdentity(dbState, _), _, Some(groupId))
      when dbState |> groupExists(groupId) => openedGroup
  | (HasIdentity(dbState, _), _, _) => dbState |> findSomeGroup
  | (WaitingForDbAndIdentity(_), _, _) => None
  };
};

let init = () => {
  let (p2p, p2pCmd) =
    PM.init(
      PM.InitConfig.make(
        ~contentInitializer=Content.initNodes("Hello CRDT World", "AAA"),
        (),
      ),
    );
  let (p2pGui, p2pGuiCmd) = PMGui.PeerScreens.init();
  let openedGroup =
    updateOpenedGroup(~p2pTagged=p2p |> PM.State.classify, None, Msg.Noop);
  (
    {p2p, p2pGui, route: Editor, openedGroup},
    Cmd.batch([
      p2pCmd |> Cmd.map(p2pMsgToMsg),
      p2pGuiCmd |> Cmd.map(p2pGuiMsgToMsg),
    ]),
  );
};

let update = (model, msg) => {
  // Js.log(msg);
  let (model, cmd) =
    switch (msg) {
    | P2PMsg(p2pMsg)
      // Handle cases when PMGui wants to send a msg to PM
    | P2PGuiMsg(PMGui.Msg.ReqP2PMsg(p2pMsg)) =>
      let (p2p, p2pCmd) = PM.update(model.p2p, p2pMsg);

      let (p2pGui, p2pGuiCmd) =
        PMGui.PeerScreens.update(
          ~core=p2p |> PM.State.classify,
          PMGui.Msg.Noop,
          model.p2pGui,
        );

      (
        {...model, p2p, p2pGui},
        Cmd.batch([
          p2pCmd |> Cmd.map(p2pMsgToMsg),
          p2pGuiCmd |> Cmd.map(p2pGuiMsgToMsg),
        ]),
      );

    | P2PGuiMsg(p2pGuiMsg) =>
      let (p2pGui, p2pGuiCmd) =
        PMGui.PeerScreens.update(
          ~core=model.p2p |> PM.State.classify,
          p2pGuiMsg,
          model.p2pGui,
        );

      let route =
        switch (p2pGuiMsg) {
        | PMGui.Msg.ClickedGoBackToApp
        | PMGui.Msg.ClickedOpenGroup(_) => Route.Editor
        | _ => model.route
        };

      ({...model, p2pGui, route}, p2pGuiCmd |> Cmd.map(p2pGuiMsgToMsg));
    | Route.Change(route) => ({...model, route}, Cmd.none)
    | _ => (model, Cmd.none)
    };

  (
    {
      ...model,
      openedGroup:
        updateOpenedGroup(
          ~p2pTagged=model.p2p |> PM.State.classify,
          model.openedGroup,
          msg,
        ),
    },
    cmd,
  );
};

let p2pMatchWithIdentity = p2pState =>
  switch (p2pState |> PM.State.classify) {
  | HasIdentity(dbState, runtimeState) => Some((dbState, runtimeState))
  | _ => None
  };

let subscriptions = model =>
  Sub.batch([PM.subscriptions(model.p2p) |> Sub.map(p2pMsgToMsg)]);

let store =
  Store.create(~init, ~update, ~subscriptions, ~shutdown=_ => Cmd.none);

// store.subscribe(model => Js.log(model));
/* */