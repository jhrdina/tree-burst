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
  | SelectedNode(string)
  | DeselectedNode(string)
  | ChangedNodeText(string, string)
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
  key: string,
  start: (int, int),
  end_: (int, int),
};

type subtreeLayout = {
  positions: NodeIdMap.t((int, int)),
  lines: list(line),
  rect,
};

let nodesHSpace = 80;
let nodesVSpace = 12;

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
  ->Belt.List.keepMap(nodeId =>
      crdt
      |> Content.findNodeByIdSafe(nodeId)
      |?> (node => node.deleted ? None : Some(node))
    )
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
          [
            (node.id, (x, y + subtreeLayout.rect.height / 2)),
            ...endsOfLines,
          ],
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
          subtreesLayout.rect.height > 0
            ? subtreesLayout.rect.height - nodesVSpace
            : subtreesLayout.rect.height;

        let width =
          subtreesLayout.rect.width > 0
            ? thisNodeWidth + nodesHSpace + subtreesLayout.rect.width
            : thisNodeWidth;
        let height = max(thisNodeHeight, subtreesHeight);

        let linesStart = (x + thisNodeWidth, y + height / 2);
        let linesToBeAdded =
          endsOfLines
          |> List.map(((key, lineEnd)) =>
               {start: linesStart, end_: lineEnd, key}
             );
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

let renderCurve =
    (~classes, ~strength=?, ~key, ~start as (x1, y1), ~end_ as (x2, y2), ()) => {
  let s = string_of_int;
  let strength = strength |? (x2 - x1) / 2;
  <path
    key
    d={
      [
        "M",
        s(x1),
        ",",
        s(y1),
        " ",
        "C",
        s(x1 + strength),
        ",",
        s(y1),
        " ",
        s(x2 - strength),
        ",",
        s(y2),
        " ",
        s(x2),
        ",",
        s(y2),
      ]
      |> String.concat("")
    }
    className=classes##edge
  />;
};

let renderLines = (~classes, layout) => {
  <svg
    key="treeBurst-lines"
    style={ReactDOMRe.Style.make(
      ~position="absolute",
      ~width=Utils.pxOfInt(layout.rect.width),
      ~height=Utils.pxOfInt(layout.rect.height),
      ~marginBottom="60px",
      ~paddingRight="24px",
      ~boxSizing="content-box",
      (),
    )}>
    {layout.lines
     |> List.fold_left(
          (arr, line) =>
            Array.append(
              arr,
              [|
                renderCurve(
                  ~classes,
                  ~start=line.start,
                  ~end_=line.end_,
                  ~key=line.key,
                  (),
                ),
              |],
            ),
          [||],
        )
     |> ReasonReact.array}
  </svg>;
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
          ~padding="24px",
          (),
        ),
    },
    {
      name: "edge",
      styles:
        ReactDOMRe.Style.make(
          ~fill="none",
          ~stroke="#8b8b8b",
          ~strokeWidth="2px",
          (),
        ),
    },
    {name: "node", styles: ReactDOMRe.Style.make(~position="absolute", ())},
  ]);

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

  initialState: () => {nodesDimensions: NodeIdMap.empty, editedNode: None},

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

    | SelectedNode(nodeId) =>
      let editedNode =
        model.p2p
        |> RootModel.p2pMatchWithIdentity
        |?>> fst
        |?>> PM.DbState.groups
        |?> PM.PeersGroups.findOpt(groupId)
        |?>> PM.PeersGroup.content
        |?> Content.findNodeByIdSafe(nodeId)
        |?>> (node => {id: nodeId, text: node.text.value});

      ReasonReact.Update({...s, editedNode});

    | DeselectedNode(nodeId) =>
      switch (s.editedNode) {
      | Some(editedNode) when editedNode.id == nodeId =>
        ReasonReact.Update({...s, editedNode: None})
      | Some(_)
      | None => ReasonReact.NoUpdate
      }

    | ChangedNodeText(nodeId, text) =>
      switch (s.editedNode) {
      | Some(editedNode) when editedNode.id == nodeId =>
        ReasonReact.Update({...s, editedNode: Some({...editedNode, text})})
      | Some(_)
      | None => ReasonReact.NoUpdate
      }
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
                      (0, 0),
                      content,
                      self.state.nodesDimensions,
                      {
                        positions: NodeIdMap.empty,
                        lines: [],
                        rect: {
                          x: 0,
                          y: 0,
                          width: 0,
                          height: 0,
                        },
                      },
                      rootNode,
                    )

                  content
                  |> Content.foldNodes(
                       (arr, node) => {
                         let text =
                           switch (self.state.editedNode) {
                           | Some(editedNode) when editedNode.id == node.id =>
                             editedNode.text
                           | Some(_)
                           | None => node.text.value
                           };
                         let handleBlur =
                             (_e, self: ReasonReact.self('a, 'b, 'c)) =>
                           {self.send(DeselectedNode(node.id))
                            if (node.text.value != text) {
                              pushMsg(
                                RootModel.P2PMsg(
                                  PM.Msg.updateGroupContent(
                                    groupId,
                                    content
                                    |> Content.updateNodeText(node.id, text),
                                  ),
                                ),
                              );
                            }};

                         let hasConflict =
                           !(node.text.conflicts |> PM.Peer.Id.Map.is_empty);

                         let (posX, posY) =
                           layout.positions |> findOpt(node.id) |? (0, 0);

                         let nodeEl =
                           <Node
                             className=classes##node
                             style={ReactDOMRe.Style.make(
                               ~transform=
                                 "translate("
                                 ++ Utils.pxOfInt(posX)
                                 ++ ", "
                                 ++ Utils.pxOfInt(posY)
                                 ++ ")",
                               (),
                             )}
                             key={node.id}
                             text
                             hasConflict
                             onChange={(_e, v) =>
                               self.send(ChangedNodeText(node.id, v))
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
                                          ~childId=PM.IdGenerator.generate(),
                                          ~text="",
                                        ),
                                   ),
                                 ),
                               )
                             }
                             onDelete={() =>
                               switch (node.parentId) {
                               | Some(parentId) =>
                                 self.send(DeselectedNode(node.id));
                                 pushMsg(
                                   RootModel.P2PMsg(
                                     PM.Msg.updateGroupContent(
                                       groupId,
                                       content
                                       |> Content.deleteChild(
                                            ~parentId,
                                            ~childId=node.id,
                                          ),
                                     ),
                                   ),
                                 );
                               | None => ()
                               }
                             }
                             onConflictClick={_ =>
                               pushMsg(
                                 Route.Change(
                                   Conflict(groupId, Text(node.id)),
                                 ),
                               )
                             }
                             onFocus={_ => self.send(SelectedNode(node.id))}
                             onBlur={self.handle(handleBlur)}
                             onSizeChange={size =>
                               self.send(ChangedNodeSize(node.id, size))
                             }
                           />;
                         Array.append(arr, [|nodeEl|]);
                       },
                       [|renderLines(~classes, layout)|],
                       rootNodeId,
                     )
                  |> ReasonReact.array}
             )
             |? <MaterialUi.Typography variant=`Body2>
                  {"Waiting for initial replica from local storage or from peers..."
                   |> ReasonReact.string}
                  <br />
                  <small>
                    {"(make sure there is at least one peer with write permissions added in the group, so that we can get the data from it as soon as he goes online)"
                     |> ReasonReact.string}
                  </small>
                </MaterialUi.Typography>}
          </div>
        </div>
      }
    />;
  },
};