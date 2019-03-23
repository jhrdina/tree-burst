type t =
  | P2P
  | Editor
  | Conflict;

type Msg.t +=
  | Change(t);