open Infix;

type nodeId = string;

module RootKeys = {
  let nodes = "nodes";
  let rootNodeId = "rootNodeId";
};

module NodeKeys = {
  let id = "id";
  let parentId = "parentId";
  let text = "text";
  let children = "children";
  let deleted = "deleted";
};

type conflictable('a) = {
  value: 'a,
  conflicts: PM.Peer.Id.Map.t('a),
};

type node = {
  id: string,
  parentId: option(string),
  text: conflictable(string),
  deleted: bool,
  children: list(string),
};

let nodes = (root: PM.Crdt.Json.Map.t) =>
  PM.Crdt.Json.(root |> Map.get(RootKeys.nodes) |?> Map.ofJson);

// let foldNodes = (f, acc, crdt) =>
//   PM.Crdt.(
//     crdt
//     |> root
//     |> Json.Map.get(nodesKey)
//     |?> Json.Map.ofJson
//     |?> Json.Map.get()
//     |?>> Json.List.foldLeft(
//            (acc, item) =>
//              switch (item |> Json.asString) {
//              | Some(strItem) => f(acc, strItem)
//              | None => acc
//              },
//            acc,
//          )
//   );

let conflictableMap = (mapper, conflictable: PM.Crdt.Json.conflictable) =>
  conflictable.value
  |> mapper
  |?>> (
    newValue => {
      {
        value: newValue,
        conflicts:
          conflictable.conflicts
          |?>> (
            conflictValues =>
              PM.Crdt.Json.ConflictValues.fold(
                (peerId, json, conflicts) =>
                  switch (json |> mapper) {
                  | Some(newValue) =>
                    conflicts |> PM.Peer.Id.Map.add(peerId, newValue)
                  | None => conflicts
                  },
                conflictValues,
                PM.Peer.Id.Map.empty,
              )
          )
          |? PM.Peer.Id.Map.empty,
      };
    }
  );

let resolveDeletedConflict = conflictable =>
  // If there is a single peer that thinks node should stay visible, make it
  // visible.
  conflictable.value
  && !(
       conflictable.conflicts
       |> PM.Peer.Id.Map.exists((_peerId, deleted) => !deleted)
     );

let findNodeById = (nodeId, t) =>
  PM.Crdt.(t |> root |> nodes |?> Json.Map.get(nodeId) |?> Json.Map.ofJson);

let findNodeByIdSafe = (nodeId, t) =>
  PM.Crdt.Json.(
    t
    |> findNodeById(nodeId)
    |?> (
      node => {
        switch (
          node |> Map.get(NodeKeys.id) |?> asString,
          node |> Map.get(NodeKeys.parentId) |?> asString,
          node |> Map.getC(NodeKeys.text) |?> conflictableMap(asString),
          node
          |> Map.getC(NodeKeys.deleted)
          |?> conflictableMap(asBool)
          |?>> resolveDeletedConflict
          |? false,
          node
          |> Map.get(NodeKeys.children)
          |?> List.ofJson
          |?>> (
            children =>
              List.foldRight(
                (childIdJson, acc) =>
                  switch (childIdJson |> asString) {
                  | Some(childId) => [childId, ...acc]
                  | None => acc
                  },
                children,
                [],
              )
          )
          |? [],
        ) {
        | (Some(id), parentId, Some(text), deleted, children) =>
          Some({id, parentId, text, deleted, children})
        | _ => None
        };
      }
    )
  );

let updateKey = (key, f, map) =>
  PM.Crdt.Json.(
    switch (map |> Map.get(key)) {
    | Some(item) =>
      switch (item |> f) {
      | Some(newValue) => map |> Map.add(key, newValue)
      | None => map
      }
    | None => map
    }
  );

let initNodes = (text, nodeId, t) =>
  t
  |> PM.Crdt.change("Init nodes", root =>
       PM.Crdt.Json.(
         root
         |> Map.add(
              RootKeys.nodes,
              Map.create()
              |> Map.add(
                   nodeId,
                   Map.create()
                   |> Map.add(NodeKeys.id, string(nodeId))
                   |> Map.add(NodeKeys.text, string(text))
                   |> Map.toJson,
                 )
              |> Map.toJson,
            )
         |> Map.add(RootKeys.rootNodeId, string(nodeId))
       )
     );

let addChild = (~parentId, ~childId, ~text, t) =>
  t
  |> PM.Crdt.change("Add child", root =>
       PM.Crdt.Json.(
         root
         |> updateKey(RootKeys.nodes, nodes =>
              nodes
              |> Map.ofJson
              |?>> updateKey(
                     parentId,
                     parent => {
                       let pMap = parent |> Map.ofJson;
                       pMap
                       |?>> Map.add(
                              NodeKeys.children,
                              pMap
                              |?> Map.get(NodeKeys.children)
                              |?> List.ofJson
                              |? List.create()
                              |> List.prepend(string(childId))
                              |> List.toJson,
                            )
                       |?>> Map.toJson;
                     },
                   )
              |?>> Map.add(
                     childId,
                     Map.create()
                     |> Map.add(NodeKeys.id, string(childId))
                     |> Map.add(NodeKeys.parentId, string(parentId))
                     |> Map.add(NodeKeys.text, string(text))
                     |> Map.toJson,
                   )
              |?>> Map.toJson
            )
       )
     );

let rec removeSubtreeFromNodes = (~nodeId, nodes) => {
  PM.Crdt.Json.(
    nodes
    |> Map.get(nodeId)
    |?> Map.ofJson
    // Remove all my descendants
    |?> Map.get(NodeKeys.children)
    |?> List.ofJson
    |?>> List.foldLeft(
           (nodesAcc, childIdJson) =>
             switch (childIdJson |> asString) {
             | Some(childId) =>
               removeSubtreeFromNodes(~nodeId=childId, nodesAcc)
             | None => nodesAcc
             },
           nodes,
         )
    |? nodes
    // Remove myself
    |> updateKey(nodeId, node =>
         node
         |> Map.ofJson
         |?>> Map.add(NodeKeys.deleted, bool(true))
         |?>> Map.toJson
       )
  );
};

let deleteChild = (~parentId as _, ~childId, t) =>
  t
  |> PM.Crdt.change("Delete child", root =>
       PM.Crdt.Json.(
         root
         |> updateKey(RootKeys.nodes, nodes =>
              nodes
              |> Map.ofJson
              |?>> removeSubtreeFromNodes(~nodeId=childId)
              |?>> Map.toJson
            )
       )
     );

let rec foldPredecessors: (('a, node) => 'a, 'a, string, PM.Crdt.t) => 'a =
  (f, acc, nodeId, crdt) => {
    switch (crdt |> findNodeByIdSafe(nodeId)) {
    | Some(node) =>
      let acc = f(acc, node);
      switch (node.parentId) {
      | Some(parentId) => foldPredecessors(f, acc, parentId, crdt)
      | None => acc
      };

    | None => acc
    };
  };

let updateNodeText = (nodeId, text, t) =>
  t
  |> PM.Crdt.change("Change text", root =>
       PM.Crdt.Json.(
         root
         |> updateKey(RootKeys.nodes, nodes =>
              nodes
              |> Map.ofJson
              |?>> updateKey(nodeId, node =>
                     node
                     |> Map.ofJson
                     |?>> Map.add(NodeKeys.text, string(text))
                     |?>> Map.toJson
                   )
              |?>> (
                nodes =>
                  foldPredecessors(
                    (nodes, node) => {
                      Js.log("folding node " ++ node.id);
                      nodes
                      |> updateKey(node.id, nodeJson =>
                           nodeJson
                           |> Map.ofJson
                           |?>> Map.add(NodeKeys.deleted, bool(true))
                           |?>> Map.add(NodeKeys.deleted, bool(false))
                           |?>> Map.toJson
                         );
                    },
                    nodes,
                    nodeId,
                    t,
                  )
              )
              |?>> Map.toJson
            )
       )
     );

let getRootNodeId = crdt =>
  PocketMeshPeer.Crdt.(
    crdt |> root |> Json.Map.get(RootKeys.rootNodeId) |?> Json.asString
  );

let getRootNode = crdt =>
  crdt |> getRootNodeId |?> (rootNodeId => crdt |> findNodeById(rootNodeId));

let rec foldNodes: (('a, node) => 'a, 'a, string, PM.Crdt.t) => 'a =
  (f, acc, nodeId, crdt) => {
    switch (crdt |> findNodeByIdSafe(nodeId)) {
    | Some(node) when node.deleted == false =>
      let acc = f(acc, node);
      node.children
      ->Belt.List.reduce(acc, (acc, childId) =>
          foldNodes(f, acc, childId, crdt)
        );
    | Some(_)
    | None => acc
    };
  };

let logNodes = (text, crdt) =>
  Js.log2(text, crdt |> PM.Crdt.root |> nodes |> Js.Json.stringifyAny);