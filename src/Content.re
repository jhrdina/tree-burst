open Infix;

type nodeId = string;

let nodesKey = "nodes";
let rootNodeIdKey = "rootNodeId";

module NodeKeys = {
  let text = "text";
  let children = "children";
};

let nodes = (root: PM.Crdt.Json.Map.t) =>
  PM.Crdt.Json.(root |> Map.get(nodesKey) |?> Map.ofJson);

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

let findNodeById = (nodeId, t) =>
  PM.Crdt.(t |> root |> nodes |?> Json.Map.get(nodeId) |?> Json.Map.ofJson);

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
              nodesKey,
              Map.create()
              |> Map.add(
                   nodeId,
                   Map.create()
                   |> Map.add(NodeKeys.text, string(text))
                   |> Map.toJson,
                 )
              |> Map.toJson,
            )
         |> Map.add(rootNodeIdKey, string(nodeId))
       )
     );

let addChild = (~parentId, ~childId, ~text, t) =>
  t
  |> PM.Crdt.change("Add child", root =>
       PM.Crdt.Json.(
         root
         |> updateKey(nodesKey, nodes =>
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
                     |> Map.add(NodeKeys.text, string(text))
                     |> Map.toJson,
                   )
              |?>> Map.toJson
            )
       )
     );

let updateNodeText = (nodeId, text, t) =>
  t
  |> PM.Crdt.change("Change text", root =>
       PM.Crdt.Json.(
         root
         |> updateKey(nodesKey, nodes =>
              nodes
              |> Map.ofJson
              |?>> updateKey(nodeId, node =>
                     node
                     |> Map.ofJson
                     |?>> Map.add(NodeKeys.text, string(text))
                     |?>> Map.toJson
                   )
              |?>> Map.toJson
            )
       )
     );

let getRootNodeId = crdt =>
  PocketMeshPeer.Crdt.(
    crdt |> root |> Json.Map.get(rootNodeIdKey) |?> Json.asString
  );