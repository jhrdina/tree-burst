open BlackTea;
module PMGui = PocketMeshPeerMaterialUi;

type model = {
  p2p: PM.State.t,
  p2pGui: PMGui.PeerScreens.model,
  route: Route.t,
};

type Msg.t +=
  | P2PMsg(PM.Msg.t)
  | P2PGuiMsg(PMGui.Msg.t);

let p2pMsgToMsg = m => P2PMsg(m);
let p2pGuiMsgToMsg = m => P2PGuiMsg(m);

let init = () => {
  let (p2p, p2pCmd) =
    PM.init(
      PM.InitConfig.make(
        ~contentInitializer=Content.initNodes("Hello CRDT World", "AAA"),
        (),
      ),
    );
  let (p2pGui, p2pGuiCmd) = PMGui.PeerScreens.init();
  (
    {p2p, p2pGui, route: Editor},
    Cmd.batch([
      p2pCmd |> Cmd.map(p2pMsgToMsg),
      p2pGuiCmd |> Cmd.map(p2pGuiMsgToMsg),
    ]),
  );
};

let update = (model, msg) => {
  // Js.log(msg);
  switch (msg) {
  | P2PMsg(p2pMsg) =>
    let (p2p, cmd) = PM.update(model.p2p, p2pMsg);
    ({...model, p2p}, cmd |> Cmd.map(p2pMsgToMsg));
  | P2PGuiMsg(p2pGuiMsg) =>
    // Handle cases when PMGui wants to send a msg to PM
    let (p2p, p2pCmd) =
      switch (p2pGuiMsg) {
      | PMGui.Msg.ReqP2PMsg(p2pMsg) => PM.update(model.p2p, p2pMsg)
      | _ => (model.p2p, Cmd.none)
      };
    let (p2pGui, p2pGuiCmd) =
      PMGui.PeerScreens.update(
        ~core=p2p |> PM.State.classify,
        p2pGuiMsg,
        model.p2pGui,
      );

    let route =
      switch (p2pGuiMsg) {
      | PMGui.Msg.ClickedGoBackToApp => Route.Editor
      | _ => model.route
      };

    (
      {...model, p2p, p2pGui, route},
      Cmd.batch([
        p2pCmd |> Cmd.map(p2pMsgToMsg),
        p2pGuiCmd |> Cmd.map(p2pGuiMsgToMsg),
      ]),
    );
  | Route.Change(route) => ({...model, route}, Cmd.none)
  | _ => (model, Cmd.none)
  };
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