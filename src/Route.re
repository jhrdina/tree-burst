type t =
  | P2P
  | Editor;

type Msg.t +=
  | Change(t);