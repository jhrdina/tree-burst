open Infix;

module NodeIdMap = Map.Make(String);
let findOpt = (key, map) =>
  switch (NodeIdMap.find(key, map)) {
  | value => Some(value)
  | exception Not_found => None
  };

type editedNode = {
  id: string,
  text: string,
};

type state = {
  editedNode: option(editedNode),
  nodesDimensions: NodeIdMap.t((int, int)),
};

type action =
  | UpdateNodeText(string)
  | ChangedNodeSize(string, (int, int));

/*
 new CRDT content -> update prop NodeView.value [RENDER 1] -> detect value change in NodeView, fire onDimensionsChange -> update model with new dimensions -> model change detected so run layout (compute new positions) -> update prop NodeView.pos [RENDER 2]
 */

/*
 new CRDT content -> update prop NodeView.value [RENDER 1] -> detect value change in NodeView, fire onDimensionsChange -> update model with new dimensions -> model change detected so run layout (compute new positions) -> update prop NodeView.pos [RENDER 2]
 */

// let layoutSubtree = (node) => {
//   node
// }

type rect = {
  x: int,
  y: int,
  width: int,
  height: int,
};

type line = {
  start: (int, int),
  end_: (int, int),
};

type subtreeLayout = {
  positions: NodeIdMap.t((int, int)),
  lines: list(line),
  rect,
};

let nodesHSpace = 100;
let nodesVSpace = 16;

let rec layoutSubtree =
        (
          (x, y),
          crdt,
          nodesDimensions: NodeIdMap.t((int, int)),
          subtreesLayout,
          node: Content.node,
        ) => {
  let (thisNodeWidth, thisNodeHeight) =
    nodesDimensions |> findOpt(node.id) |? (0, 0);
  let subtreesVBoxPos = (x + thisNodeWidth + nodesHSpace, y);
  // Layout children
  node.children
  ->Belt.List.keepMap(nodeId => crdt |> Content.findNodeByIdSafe(nodeId))
  ->Belt.List.reduce(
      (
        // Starting subtree pos
        subtreesVBoxPos,
        // Line endpoints
        [],
        {
          ...subtreesLayout,
          rect: {
            x: subtreesVBoxPos |> fst,
            y: subtreesVBoxPos |> fst,
            width: 0,
            height: 0,
          },
        },
      ),
      (((x, y) as subtreePos, endsOfLines, subtreesLayout), node) => {
        let subtreeLayout =
          layoutSubtree(
            subtreePos,
            crdt,
            nodesDimensions,
            subtreesLayout,
            node,
          );
        // Next iteration
        (
          (x, y + subtreeLayout.rect.height + nodesVSpace),
          [(x, y + subtreeLayout.rect.height / 2), ...endsOfLines],
          {
            ...subtreeLayout,
            rect: {
              ...subtreesLayout.rect,
              width: max(subtreesLayout.rect.width, subtreeLayout.rect.width),
              height:
                subtreesLayout.rect.height
                + subtreeLayout.rect.height
                + nodesVSpace,
            },
          },
        );
      },
    )
  ->(
      ((_, endsOfLines, subtreesLayout)) => {
        let subtreesHeight =
          subtreesLayout.rect.height > 0 ?
            subtreesLayout.rect.height - nodesVSpace :
            subtreesLayout.rect.height;

        let width =
          subtreesLayout.rect.width > 0 ?
            thisNodeWidth + nodesHSpace + subtreesLayout.rect.width :
            thisNodeWidth;
        let height = max(thisNodeHeight, subtreesHeight);

        let linesStart = (thisNodeWidth, y + height / 2);
        let linesToBeAdded =
          endsOfLines
          |> List.map(lineEnd => {start: linesStart, end_: lineEnd});
        let lines = List.append(linesToBeAdded, subtreesLayout.lines);

        {
          positions:
            subtreesLayout.positions
            |> NodeIdMap.add(node.id, (x, y + (height - thisNodeHeight) / 2)),
          lines,
          rect: {
            x,
            y,
            width,
            height,
          },
        };
      }
    );
};

let useStyles =
  MuiStylesHooks.make([
    {
      name: "root",
      styles:
        ReactDOMRe.Style.make(
          ~position="relative",
          ~backgroundColor="#eeeeee",
          ~flexGrow="1",
          ~display="flex",
          ~overflow="auto",
          (),
        ),
    },
    {
      name: "zoomWrapper",
      styles:
        ReactDOMRe.Style.make(
          // ~border="2px solid #00ff00",
          ~transformOrigin="top left",
          ~flex="1",
          (),
        ),
    },
  ]);

let withIdentity = p2pState =>
  switch (p2pState |> PM.State.classify) {
  | HasIdentity(dbState, runtimeState) => Some((dbState, runtimeState))
  | _ => None
  };

let rootNodeOfModel = (groupId, model: RootModel.model) =>
  switch (model.p2p |> PM.State.classify) {
  | HasIdentity(dbState, _runtimeState) =>
    dbState
    |> PM.DbState.groups
    |> PM.PeersGroups.findOpt(groupId)
    |?>> PM.PeersGroup.content
    |?> (
      content =>
        content
        |> Content.getRootNodeId
        |?> (
          rootNodeId =>
            content
            |> Content.findNodeByIdSafe(rootNodeId)
            |?>> (rootNode => (content, rootNodeId, rootNode))
        )
    )
  | WaitingForDbAndIdentity(_) => None
  };

let component = ReasonReact.reducerComponent("TreeView");

let make = (~groupId, ~model: RootModel.model, ~pushMsg, _children) => {
  ...component,

  initialState: () => {
    nodesDimensions: NodeIdMap.empty,
    editedNode:
      model.p2p
      |> withIdentity
      |?>> fst
      |?>> PM.DbState.groups
      |?> PM.PeersGroups.findOpt(groupId)
      |?>> PM.PeersGroup.content
      |?> Content.getRootNode
      |?> (
        node =>
          PM.Crdt.Json.(
            switch (
              node |> Map.get(Content.NodeKeys.id) |?> asString,
              node |> Map.get(Content.NodeKeys.text) |?> asString,
            ) {
            | (Some(id), Some(text)) => Some({id, text})
            | _ => None
            }
          )
      ),
  },

  reducer: (action: action, s: state) =>
    switch (action) {
    | ChangedNodeSize(nodeId, size) =>
      if (s.nodesDimensions |> findOpt(nodeId) |?>> (!=)(size) |? true) {
        ReasonReact.Update({
          ...s,
          nodesDimensions: s.nodesDimensions |> NodeIdMap.add(nodeId, size),
        });
      } else {
        ReasonReact.NoUpdate;
      }
    | UpdateNodeText(_) => ReasonReact.NoUpdate
    // ReasonReact.Update(
    //   switch (action) {
    //   // | UpdateNodeText(text) => {...s, editedNode: text}
    //   },
    // ),
    },

  render: self => {
    <UseHook
      hook=useStyles
      render={classes =>
        <div className=classes##root id="mScroller">
          <div className=classes##zoomWrapper id="mScaler">
            {model
             |> rootNodeOfModel(groupId)
             |?>> (
               ((content, rootNodeId, rootNode)) =>
                 {let layout =
                    layoutSubtree(
                      (16, 16),
                      content,
                      self.state.nodesDimensions,
                      {
                        positions: NodeIdMap.empty,
                        lines: [],
                        rect: {
                          x: 16,
                          y: 16,
                          width: 0,
                          height: 0,
                        },
                      },
                      rootNode,
                    )
                  content
                  |> Content.foldNodes(
                       (arr, node) => {
                         let nodeEl =
                           <Node
                             key={node.id}
                             pos={
                               layout.positions |> findOpt(node.id) |? (0, 0)
                             }
                             selected=true
                             text={node.text}
                             onChange={(_e, v) =>
                               pushMsg(
                                 RootModel.P2PMsg(
                                   PM.Msg.updateGroupContent(
                                     groupId,
                                     content
                                     |> Content.updateNodeText(node.id, v),
                                   ),
                                 ),
                               )
                             }
                             onAddSibling={() => Js.log("adding sibling")}
                             onAddChild={() =>
                               pushMsg(
                                 RootModel.P2PMsg(
                                   PM.Msg.updateGroupContent(
                                     groupId,
                                     content
                                     |> Content.addChild(
                                          ~parentId=node.id,
                                          ~childId=
                                            Js.Date.now() |> string_of_float,
                                          ~text="",
                                        ),
                                   ),
                                 ),
                               )
                             }
                             onSizeChange={size =>
                               self.send(ChangedNodeSize(node.id, size))
                             }
                           />;
                         Array.append(arr, [|nodeEl|]);
                       },
                       [||],
                       rootNodeId,
                     )
                  |> ReasonReact.array}
             )
             |? ReasonReact.null}
          </div>
        </div>
      }
    />;
  },
};