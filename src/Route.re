type conflictVariant =
  | Text(string);
/*| Subtree(string)
  | Root*/

type t =
  | P2P
  | Editor
  | Conflict(PM.PeersGroup.Id.t, conflictVariant);

type Msg.t +=
  | Change(t);