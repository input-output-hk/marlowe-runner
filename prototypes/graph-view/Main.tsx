import * as React from "react"
// import { createRoot } from "react-dom/client"
import {
  ReactFlow,
  Node,
  Edge,
  Background,
  BackgroundVariant,
  Handle,
  Position,
  NodeTypes,
  EdgeTypes,
  EdgeProps,
  BaseEdge,
  getBezierPath,
} from "reactflow"

import 'reactflow/dist/style.css'

type Address = string
type TokeName = string
type CurrencySymbol = string
type ValueId = string
type Timeout = number

type ChoiceId = { choice_name: string, choice_owner: Party }

type Payee
  = { account: AccountId }
  | { party: Party }

type Party
  = { address: Address }
  | { role_token: TokeName }

type AccountId = Party

type Token = { currency_symbol: CurrencySymbol, token_name: TokeName }

type Value
  = { amount_of_token: Token, in_account: AccountId }
  | { negate: Value }
  | { add: Value, and: Value }
  | { value: Value, minus: Value }
  | { multiply: Value, times: Value }
  | { divide: Value, by: Value }
  | { value_of_choice: ChoiceId }
  | { use_value: ValueId }
  | { if: Observation, then: Value, else: Value }
  | "time_interval_start"
  | "time_interval_end"
  | number

type Observation
  = { both: Observation, and: Observation }
  | { either: Observation, or: Observation }
  | { not: Observation }
  | { chose_something_for: ChoiceId }
  | { value: Value, ge_than: Value }
  | { value: Value, gt: Value }
  | { value: Value, lt: Value }
  | { value: Value, le_than: Value }
  | { value: Value, equal_to: Value }
  | true
  | false

type Bound = { from: number, to: number }

type Action
  = { party: Party, deposits: Value, of_token: Token, into_account: AccountId }
  | { choose_between: Bound[], for_choice: ChoiceId }
  | { notify_if: Observation }

type Case
  = { case: Action, then: Contract }
  | { case: Action, merkleized_then: string }

type Contract
  = "close"
  | { pay: Value, token: Token, to: Payee, from_account: AccountId, then: Contract }
  | { if: Observation, then: Contract, else: Contract }
  | { when: Case[], timout: Timeout, timeout_continuation: Contract }
  | { let: ValueId, be: Value, then: Contract }
  | { assert: Observation, then: Contract }

const X_OFFSET = 200
const Y_OFFSET = 40
const NODE_HEIGHT = 30

const contractNodeStyle: React.CSSProperties = {
  display: "flex",
  flexFlow: "column",
  justifyContent: "center",
  padding: "0px",
  alignItems: "center",
  height: `${NODE_HEIGHT}px`,
  width: "150px",
  background: "white",
  border: "1px solid black",
  borderRadius: "3px",
  fontFamily: "monospace",
}

type ContractNodeType
  = "close"
  | { pay: Value, token: Token, to: Payee, from_account: AccountId }
  | { if: Observation }
  | { when: ({ case: Action } | { case: Action, merkleized_then: string })[], timout: Timeout }
  | { let: ValueId, be: Value }
  | { assert: Observation }

const PATH_TAKEN = Symbol("PATH_TAKEN")
const PATH_FUTURE = Symbol("PATH_FUTURE")
const PATH_PRUNED = Symbol("PATH_PRUNED")

type PathState = typeof PATH_TAKEN | typeof PATH_FUTURE | typeof PATH_PRUNED

type ContractNodeData = {
  type: ContractNodeType,
  state: PathState
}

type ContractEdgeData = {
  state: PathState
}

const nodeTypes: NodeTypes = {
  ContractNode({ data: { type, state } }: { data: ContractNodeData }): JSX.Element {
    const style_: React.CSSProperties =
      state === PATH_PRUNED ? { ...contractNodeStyle, opacity: "30%" } :
        state === PATH_TAKEN ? { ...contractNodeStyle, borderWidth: "2px" } :
          contractNodeStyle
    if (type === "close")
      return <div style={style_}>
        Close
        <Handle type="target" position={Position.Left} />
      </div>

    if ("if" in type)
      return <div style={style_}>
        If (...)
        <Handle type="target" position={Position.Left} id="continuation" />
        <Handle type="source" position={Position.Right} style={{ top: "8px" }} id="then" />
        <Handle type="source" position={Position.Right} style={{ top: "24px" }} id="else" />
      </div>

    if ("pay" in type)
      return <div style={style_}>
        Pay (...)
        <Handle type="target" position={Position.Left} id="continuation" />
        <Handle type="source" position={Position.Right} id="then" />
      </div>

    if ("let" in type)
      return <div style={style_}>
        Let (...)
        <Handle type="target" position={Position.Left} id="continuation" />
        <Handle type="source" position={Position.Right} id="then" />
      </div>

    if ("assert" in type)
      return <div style={style_}>
        Assert (...)
        <Handle type="target" position={Position.Left} id="continuation" />
        <Handle type="source" position={Position.Right} id="then" />
      </div>

    if ("when" in type) {
      const height = (type.when.length + 1) * Y_OFFSET - (Y_OFFSET - NODE_HEIGHT)
      const { alignItems: _, ...style } = {
        ...style_,
        height: `${height}px`,
        justifyContent: "space-around",
        paddingLeft: "5px",
      }
      return <div style={style}>
        <div>When</div>
        {type.when.map((x, i) => {
          if ("merkleized_then" in x) {
            if ("deposits" in x.case)
              return <div key={i}>
                Deposit (...)
              </div>
            if ("choose_between" in x.case)
              return <div key={i}>
                Choice (...)
              </div>
            if ("notify_if" in x.case)
              return <div key={i}>
                Notify (...)
              </div>
          } else {
            if ("deposits" in x.case)
              return <div key={i}>
                Deposit (...)
                <Handle type="source" position={Position.Right} style={{ top: `${i * 32 + 47}px` }} id={`${i}`} />
              </div>
            if ("choose_between" in x.case)
              return <div key={i}>
                Choice (...)
                <Handle type="source" position={Position.Right} style={{ top: `${i * 32 + 47}px` }} id={`${i}`} />
              </div>
            if ("notify_if" in x.case)
              return <div key={i}>
                Notify (...)
                <Handle type="source" position={Position.Right} style={{ top: `${i * 32 + 47}px` }} id={`${i}`} />
              </div>
          }
        })}
        <div>on timeout:</div>
        <Handle type="target" position={Position.Left} style={{ top: `${NODE_HEIGHT / 2}px` }} id="continuation" />
        <Handle type="source" position={Position.Right} style={{ top: `${height - NODE_HEIGHT / 2}px` }} id="timeout_continuation" />
      </div>
    }

    return <div style={style_}>Not implemented!</div>
  }
}

const edgeTypes: EdgeTypes = {
  ContractEdge({ data, sourceX, sourceY, sourcePosition, targetX, targetY, targetPosition, markerEnd, style = {} }: EdgeProps<ContractEdgeData>): JSX.Element {
    const [edgePath] = getBezierPath({ sourceX, sourceY, sourcePosition, targetX, targetY, targetPosition })
    const style_: React.CSSProperties =
      data && data.state === PATH_PRUNED ? { ...style, strokeOpacity: "30%" } :
        data && data.state === PATH_TAKEN ? {
          ...style,
          strokeWidth: 2,
          stroke: "black",
          // strokeDasharray: "5",
          // strokeDashoffset: "0",
          // animation: "edgeflow 1000ms linear infinite",
        } : style
    return <BaseEdge path={edgePath} markerEnd={markerEnd} style={style_} />
  }
}

type ContractPathHistory = ReadonlyArray<number | null>

const nodeWithState = (node: Node<ContractNodeData>, state: PathState): Node<ContractNodeData> =>
  ({ ...node, data: { ...node.data, state } })

const edgeWithState = (edge: Edge<ContractEdgeData>, state: PathState): Edge<ContractEdgeData> =>
  ({ ...edge, data: edge.data && { ...edge.data, state } })

const contract2NodesAndEdges = (contract: Contract, id: string, x: number, y: number, path: ContractPathHistory): { nodes: Node<ContractNodeData>[], edges: Edge<ContractEdgeData>[], max_y: number } => {
  if (contract === "close")
    return {
      nodes: [{ id, position: { x, y }, data: { type: "close", state: PATH_TAKEN }, type: "ContractNode" }],
      edges: [],
      max_y: y,
    }

  if ("if" in contract) {
    const [index, ...indices] = path
    const { else: else_, then, ...type } = contract
    const then_id = `1-${id}`
    const else_id = `2-${id}`
    const { max_y: then_max_y, nodes: then_nodes, edges: then_edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y, indices)
    const { max_y, nodes: else_nodes, edges: else_edges } = contract2NodesAndEdges(else_, else_id, x + X_OFFSET, then_max_y + Y_OFFSET, indices)
    return {
      nodes: [
        ...then_nodes.map(node => index === 0 ? node : index === undefined ? nodeWithState(node, PATH_FUTURE) : nodeWithState(node, PATH_PRUNED)),
        ...else_nodes.map(node => index === 1 ? node : index === undefined ? nodeWithState(node, PATH_FUTURE) : nodeWithState(node, PATH_PRUNED)),
        { id, position: { x, y }, data: { type, state: PATH_TAKEN }, type: "ContractNode" },
      ],
      edges: [
        ...then_edges.map(edge => index === 0 ? edge : index === undefined ? edgeWithState(edge, PATH_FUTURE) : edgeWithState(edge, PATH_PRUNED)),
        ...else_edges.map(edge => index === 1 ? edge : index === undefined ? edgeWithState(edge, PATH_FUTURE) : edgeWithState(edge, PATH_PRUNED)),
        { id: `then-${then_id}-${id}`, source: id, target: then_id, sourceHandle: "then", type: "ContractEdge", data: { state: index === 0 ? PATH_TAKEN : index === undefined ? PATH_FUTURE : PATH_PRUNED } },
        { id: `else-${else_id}-${id}`, source: id, target: else_id, sourceHandle: "else", type: "ContractEdge", data: { state: index === 1 ? PATH_TAKEN : index === undefined ? PATH_FUTURE : PATH_PRUNED } },
      ],
      max_y,
    }
  }

  if ("pay" in contract) {
    const { then, ...type } = contract
    const then_id = `1-${id}`
    const { max_y, nodes, edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y, path)
    return {
      nodes: [
        ...nodes,
        { id, position: { x, y }, data: { type, state: PATH_TAKEN }, type: "ContractNode" },
      ],
      edges: [
        ...edges,
        { id: `then-${id}`, source: id, target: then_id, type: "ContractEdge", data: { state: PATH_TAKEN } },
      ],
      max_y,
    }
  }

  if ("let" in contract) {
    const { then, ...type } = contract
    const then_id = `1-${id}`
    const { max_y, nodes, edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y, path)
    return {
      nodes: [
        ...nodes,
        { id, position: { x, y }, data: { type, state: PATH_TAKEN }, type: "ContractNode" },
      ],
      edges: [
        ...edges,
        { id: `then-${id}`, source: id, target: then_id, type: "ContractEdge", data: { state: PATH_TAKEN } },
      ],
      max_y,
    }
  }

  if ("assert" in contract) {
    const { then, ...type } = contract
    const then_id = `1-${id}`
    const { max_y, nodes, edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y, path)
    return {
      nodes: [
        ...nodes,
        { id, position: { x, y }, data: { type, state: PATH_TAKEN }, type: "ContractNode" },
      ],
      edges: [
        ...edges,
        { id: `then-${id}`, source: id, target: then_id, type: "ContractEdge", data: { state: PATH_TAKEN } },
      ],
      max_y,
    }
  }

  if ("when" in contract) {
    const [index, ...indices] = path
    const { timeout_continuation, ...type } = contract
    const { nodes, edges, max_y } = type.when.reduce<{ nodes: Node<ContractNodeData>[], edges: Edge<ContractEdgeData>[], max_y: number }>((acc, on, i) => {
      if ("then" in on) {
        const then_id = `${i}-${id}`
        const { nodes, edges, max_y } = contract2NodesAndEdges(on.then, then_id, x + X_OFFSET, acc.max_y + Y_OFFSET, indices)
        return {
          nodes: [
            ...nodes.map(node => index === i ? node : index === undefined ? nodeWithState(node, PATH_FUTURE) : nodeWithState(node, PATH_PRUNED)),
            ...acc.nodes,
          ],
          edges: [
            ...edges.map(edge => index === i ? edge : index === undefined ? edgeWithState(edge, PATH_FUTURE) : edgeWithState(edge, PATH_PRUNED)),
            ...acc.edges,
            { id: `then-${then_id}-${id}`, source: id, target: then_id, sourceHandle: `${i}`, type: "ContractEdge", data: { state: index === i ? PATH_TAKEN : index === undefined ? PATH_FUTURE : PATH_PRUNED } },
          ],
          max_y
        }
      }
      return acc
    }, {
      nodes: [
        { id, position: { x, y }, data: { type, state: PATH_TAKEN }, type: "ContractNode" },
      ],
      edges: [],
      max_y: y - Y_OFFSET
    })
    const timeout_then_id = `${type.when.length}-${id}`
    const timeout_graph = contract2NodesAndEdges(timeout_continuation, timeout_then_id, x + X_OFFSET, max_y + Y_OFFSET, indices)
    return {
      nodes: [
        ...timeout_graph.nodes.map(node => index === null ? node : index === undefined ? nodeWithState(node, PATH_FUTURE) : nodeWithState(node, PATH_PRUNED)),
        ...nodes,
      ],
      edges: [
        ...timeout_graph.edges.map(edge => index === null ? edge : index === undefined ? edgeWithState(edge, PATH_FUTURE) : edgeWithState(edge, PATH_PRUNED)),
        ...edges,
        { id: `timeout_continuation-${id}`, source: id, target: timeout_then_id, sourceHandle: "timeout_continuation", type: "ContractEdge", data: { state: index === null ? PATH_TAKEN : index === undefined ? PATH_FUTURE : PATH_PRUNED } },
      ],
      max_y: timeout_graph.max_y
    }
  }

  return {
    nodes: [],
    edges: [],
    max_y: y,
  }
}

export const MarloweGraphView = ({ contract, path }: { contract: Contract, path?: ContractPathHistory }): JSX.Element => {
  const { nodes, edges } = contract2NodesAndEdges(contract, "1", 0, 0, path || [])
  return <div style={{ width: "95vw", height: "95vh" }}>
    <ReactFlow nodes={nodes} edges={edges} nodeTypes={nodeTypes} edgeTypes={edgeTypes}>
      <Background variant={BackgroundVariant.Dots} gap={12} size={1} />
    </ReactFlow>
  </div>
}

// const contract: Contract = {
//   when: [
//     {
//       case: {
//         deposits: 500,
//         party: { role_token: "Party A" },
//         of_token: { currency_symbol: "", token_name: "" },
//         into_account: { role_token: "Party B" }
//       },
//       then: { if: true, then: "close", else: "close" }
//     },
//     {
//       case: { notify_if: false },
//       then: {
//         when: [
//           {
//             case: {
//               deposits: 200,
//               party: { role_token: "Party A" },
//               of_token: { currency_symbol: "", token_name: "" },
//               into_account: { role_token: "Party B" }
//             },
//             then: { if: true, then: { assert: true, then: "close" }, else: "close" }
//           },
//         ],
//         timout: 2001,
//         timeout_continuation: "close"
//       }
//     },
//     {
//       case: {
//         for_choice:
//         {
//           choice_name: "The way",
//           choice_owner: { role_token: "Party C" }
//         },
//         choose_between: [
//           { from: 1, to: 3 },
//           { from: 5, to: 8 }
//         ]
//       },
//       merkleized_then: "huey-dewey-louie"
//     },
//     {
//       case: { notify_if: true },
//       then: { let: "x", be: 13, then: "close" }
//     }
//   ],
//   timout: 1985,
//   timeout_continuation: {
//     if: true,
//     then: {
//       if: false,
//       then: "close",
//       else: {
//         pay: 1234,
//         token: { currency_symbol: "", token_name: "" },
//         to: { party: { role_token: "Owner" } },
//         from_account: { role_token: "Participant" },
//         then: "close",
//       }
//     },
//     else: { let: "x", be: 13, then: { assert: true, then: "close" } },
//   }
// }

// const root = createRoot(document.getElementById("app") as HTMLElement)

// root.render(<MarloweGraphView contract={contract} path={[]} />)
// root.render(<MarloweGraphView contract={contract} path={[1, 0]} />)

// https://reactflow.dev/docs/concepts/terms-and-definitions
// https://reactflow.dev/docs/guides/custom-nodes
// https://css-tricks.com/snippets/css/a-guide-to-flexbox
