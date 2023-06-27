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

type ContractNodeData = {
  type: ContractNodeType,
  disabled: boolean,
}

const nodeTypes: NodeTypes = {
  ContractNode({ data: { type, disabled } }: { data: ContractNodeData }): JSX.Element {
    const style_: React.CSSProperties = { ...contractNodeStyle, opacity: disabled ? "30%" : "100%" }
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

type ContractEdgeData = {
  disabled: boolean | null;
}

const edgeTypes: EdgeTypes = {
  ContractEdge({ data, sourceX, sourceY, sourcePosition, targetX, targetY, targetPosition, markerEnd, style = {} }: EdgeProps<ContractEdgeData>): JSX.Element {
    const [edgePath] = getBezierPath({ sourceX, sourceY, sourcePosition, targetX, targetY, targetPosition })
    const style_: React.CSSProperties =
      data && data.disabled
        ? { ...style, strokeOpacity: "30%" }
        : {
          ...style, strokeOpacity: "100%",
          strokeWidth: 3,
          // strokeDasharray: "5",
          // strokeDashoffset: "0",
          // animation: "edgeflow 1000ms linear infinite",
        }
    return <BaseEdge path={edgePath} markerEnd={markerEnd} style={style_} />
  }
}

type ContractPathHistory = ReadonlyArray<number | null>

const contract2NodesAndEdges = (contract: Contract, id: string, x: number, y: number, path: ContractPathHistory): { nodes: Node<ContractNodeData>[], edges: Edge<ContractEdgeData>[], max_y: number } => {
  if (contract === "close")
    return {
      nodes: [{ id, position: { x, y }, data: { type: "close", disabled: false }, type: "ContractNode" }],
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
        ...then_nodes.map(node => index === 0 || index === undefined ? node : { ...node, data: { ...node.data, disabled: true } }),
        ...else_nodes.map(node => index === 1 || index === undefined ? node : { ...node, data: { ...node.data, disabled: true } }),
        { id, position: { x, y }, data: { type, disabled: false }, type: "ContractNode" },
      ],
      edges: [
        ...then_edges.map(edge => index === 0 || index === undefined ? edge : { ...edge, data: edge.data && { ...edge.data, disabled: true } }),
        ...else_edges.map(edge => index === 1 || index === undefined ? edge : { ...edge, data: edge.data && { ...edge.data, disabled: true } }),
        { id: `then-${id}`, source: id, target: then_id, sourceHandle: "then", type: "ContractEdge", data: { disabled: !(index === 0 || index === undefined) } },
        { id: `else-${id}`, source: id, target: else_id, sourceHandle: "else", type: "ContractEdge", data: { disabled: !(index === 1 || index === undefined) } },
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
        { id, position: { x, y }, data: { type, disabled: false }, type: "ContractNode" },
      ],
      edges: [
        ...edges,
        { id: `then-${id}`, source: id, target: then_id, type: "ContractEdge", data: { disabled: false } },
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
        { id, position: { x, y }, data: { type, disabled: false }, type: "ContractNode" },
      ],
      edges: [
        ...edges,
        { id: `then-${id}`, source: id, target: then_id, type: "ContractEdge", data: { disabled: false } },
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
        { id, position: { x, y }, data: { type, disabled: false }, type: "ContractNode" },
      ],
      edges: [
        ...edges,
        { id: `then-${id}`, source: id, target: then_id, type: "ContractEdge", data: { disabled: false } },
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
            ...nodes.map(node => index === i || index === undefined ? node : { ...node, data: { ...node.data, disabled: true } }),
            ...acc.nodes,
          ],
          edges: [
            ...edges.map(edge => index === i || index === undefined ? edge : { ...edge, data: edge.data && { ...edge.data, disabled: true } }),
            ...acc.edges,
            { id: `then-${then_id}-${id}`, source: id, target: then_id, sourceHandle: `${i}`, type: "ContractEdge", data: { disabled: !(index === i || index === undefined) } },
          ],
          max_y
        }
      }
      return acc
    }, {
      nodes: [
        { id, position: { x, y }, data: { type, disabled: false }, type: "ContractNode" },
      ],
      edges: [],
      max_y: y - Y_OFFSET
    })
    const timeout_then_id = `${type.when.length}-${id}`
    const timeout_graph = contract2NodesAndEdges(timeout_continuation, timeout_then_id, x + X_OFFSET, max_y + Y_OFFSET, indices)
    return {
      nodes: [
        ...timeout_graph.nodes.map(node => index === null || index === undefined ? node : { ...node, data: { ...node.data, disabled: true } }),
        ...nodes,
      ],
      edges: [
        ...timeout_graph.edges.map(edge => index === null || index === undefined ? edge : { ...edge, data: edge.data && { ...edge.data, disabled: true } }),
        ...edges,
        { id: `timeout_continuation-${id}`, source: id, target: timeout_then_id, sourceHandle: "timeout_continuation", type: "ContractEdge", data: { disabled: !(index === null || index === undefined) } },
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

export const _MarloweGraph = ({ contract, path }: { contract: Contract, path?: ContractPathHistory }): JSX.Element => {
  const { nodes, edges } = contract2NodesAndEdges(contract, "1", 0, 0, path || [])
  return <ReactFlow nodes={nodes} edges={edges} nodeTypes={nodeTypes} edgeTypes={edgeTypes}><Background variant={BackgroundVariant.Dots} gap={12} size={1} /></ReactFlow>;
}
