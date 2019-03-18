open Infix;

type editedNode = {
  id: string,
  text: string,
};

type visibleNode = {
  id: string,
  pos: (int, int),
  dimensions: option((int, int)),
};

// We map content.nodes |> Map.t(visibleNode)

type state = {editedNode: option(editedNode)};

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

module NodeIdMap = Map.Make(String);
let findOpt = (key, map) =>
  switch (NodeIdMap.find(key, map)) {
  | value => Some(value)
  | exception Not_found => None
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
          (x, y) as topLeft,
          nodes,
          nodesDimensions: NodeIdMap.t((int, int)),
          subtreesLayout,
          node: Content.node,
        ) => {
  let (thisNodeWidth, thisNodeHeight) =
    nodesDimensions |> findOpt(node.id) |? (0, 0);
  let subtreesVBoxPos = (x + thisNodeWidth + nodesHSpace, y);
  // Layout children
  node.children
  ->Belt.List.keepMap(nodeId => nodes |> findOpt(nodeId))
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
            nodes,
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

type action =
  | UpdateNodeText(string);

let useStyles =
  MuiStylesHooks.makeWithTheme(theme =>
    [
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
    ]
  );

let withIdentity = p2pState =>
  switch (p2pState |> PM.State.classify) {
  | HasIdentity(dbState, runtimeState) => Some((dbState, runtimeState))
  | _ => None
  };

let component = ReasonReact.reducerComponent("TreeView");

let make = (~groupId, ~model: RootModel.model, ~pushMsg, _children) => {
  ...component,

  initialState: () => {
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
    // ReasonReact.Update(
    //   switch (action) {
    //   // | UpdateNodeText(text) => {...s, editedNode: text}
    //   },
    // ),
    ReasonReact.NoUpdate,

  render: self => {
    <UseHook
      hook=useStyles
      render={classes =>
        <div className=classes##root id="mScroller">
          <div className=classes##zoomWrapper id="mScaler">
            {switch (model.p2p |> PM.State.classify) {
             | HasIdentity(dbState, runtimeState) =>
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
                       |?>> (
                         node =>
                           {<Node
                              pos=(100, 170)
                              selected=true
                              text={node.text}
                              pushMsg
                              onChange={(e, v) =>
                                pushMsg(
                                  RootModel.P2PMsg(
                                    PM.Msg.updateGroupContent(
                                      groupId,
                                      content
                                      |> Content.updateNodeText(rootNodeId, v),
                                    ),
                                  ),
                                )
                              }
                              onSizeChange={s => Js.log(s)}
                            />}
                       )
                   )
               )
               |? ReasonReact.null
             | WaitingForDbAndIdentity(_) => ReasonReact.null
             }}
          </div>
        </div>
      }
    />;
  },
};