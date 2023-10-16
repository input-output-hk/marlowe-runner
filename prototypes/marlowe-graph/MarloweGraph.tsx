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
import { Action, AccountId, Contract, Observation, Payee, Timeout, Token, ValueId, Value } from "./MarloweGraph/Marlowe"

const X_OFFSET = 200
const Y_OFFSET = 40
const NODE_HEIGHT = 30

const contractNodeStyle: React.CSSProperties = {
  display: "flex",
  flexFlow: "column",
  justifyContent: "center",
  fontSize: "14px",
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
  | { when: ({ case: Action } | { case: Action, merkleized_then: string })[], timeout: Timeout }
  | { let: ValueId, be: Value }
  | { assert: Observation }

type ContractNodeData = {
  type: ContractNodeType,
  status: ContractNodeStatus,
}

const nodeTypes: NodeTypes = {
  ContractNode({ data: { type, status } }: { data: ContractNodeData }): JSX.Element {
    const style_: React.CSSProperties = statusToNodeStyle(status, contractNodeStyle);
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
// executed: black, opacity 100%, width 3px
// still-possible: gray, width 3px
// skipped: opacity 30%, width 1px
type ContractEdgeStatus = 'executed' | 'skipped' | 'still-possible';

type ContractNodeStatus = 'executed' | 'awaiting-input' | 'skipped' | 'still-possible';

// export type ContractPathHistory = ReadonlyArray<number | null>

export type ContractPathHistory
  = ReadonlyArray<number | null>   // We are on the path. When the list is empty we are in the last selected
                                   // node and should switch to `still-possible`.
  | 'still-possible'               // No branch chosen yet or parent chosen already.
  | 'skipped';                     // Parent or edge skipped.

const contractPathHistoryToNodeStatus = (contract: Contract, history: ContractPathHistory): ContractNodeStatus => {
  if (history === 'still-possible')
    return 'still-possible';
  if (history === 'skipped')
    return 'skipped';
  if (history.length === 0 && contract !== "close" && "when" in contract)
    return 'awaiting-input';
  // Array case means we are on the path or at the end of the path.
  return 'executed';
}

// In both functions the value of `index` value has following semantics:
// * `undefined` - we are inside the node which is not branching node (`Assert`, `Let`, `Pay`)
// * `null` - we are in when and checking continuation for timeout
// * `number` - we are in some branching node and picking the branch
const contractPathHistoryToEdgeStatus = (index: number | null | undefined, history: ContractPathHistory): ContractEdgeStatus => {
  if (history === 'still-possible')
    return 'still-possible';
  if (history === 'skipped')
    return 'skipped';
  if (index === undefined)
    return 'executed';
  if (history.length === 0 && index !== undefined)
    return 'still-possible';
  return (index === history[0])? 'executed' : 'skipped';
}

const contractPathHistoryContinuation = (index: number | null | undefined, history: ContractPathHistory): ContractPathHistory => {
  if (history == 'still-possible')
    return 'still-possible';
  if (history == 'skipped')
    return 'skipped';
  if (index === undefined)
    return history;
  if(history.length === 0)
    return 'still-possible';
  return index === history[0] ? history.slice(1) : 'skipped';
}

type ContractEdgeData = {
  status: ContractEdgeStatus,
}

const executedStrokeStyle = { strokeOpacity: "100%", strokeWidth: 3, stroke: "black" }
const stillPossibleStrokeStyle = { strokeOpacity: "100%", strokeWidth: 1, stroke: "gray" }
const skippedStrokeStyle = { strokeOpacity: "30%", strokeWidth: 1, stroke: "black" }


const executedDivStyle = { border: "3px solid black"}
const stillPossibleDivStyle = { border: "1px solid gray"}
const awaitingDivStyle = { border: "2px solid gray"}
const skippedDivStyle = { border: "1px solid black"}


const statusToEdgeStyle = (status: ContractEdgeStatus, defaultStyle: React.CSSProperties): React.CSSProperties => {
  switch (status) {
    case 'executed':
      return { ...defaultStyle, ...executedStrokeStyle }
    case 'skipped':
      return { ...defaultStyle, ...skippedStrokeStyle }
    case 'still-possible':
      return { ...defaultStyle, ...stillPossibleStrokeStyle }
  }
}

const statusToNodeStyle = (status: ContractNodeStatus, defaultStyle: React.CSSProperties): React.CSSProperties => {
  switch (status) {
    case 'executed':
      return { ...defaultStyle, ...executedDivStyle }
    case 'awaiting-input':
      return { ...defaultStyle, ...awaitingDivStyle }
    case 'still-possible':
      return { ...defaultStyle, ...stillPossibleDivStyle }
    case 'skipped':
      return { ...defaultStyle, ...skippedDivStyle, opacity: "30%" }
  }
}


const edgeTypes: EdgeTypes = {
  ContractEdge({ data, sourceX, sourceY, sourcePosition, targetX, targetY, targetPosition, markerEnd, style = {} }: EdgeProps<ContractEdgeData>): JSX.Element {
    const [edgePath] = getBezierPath({ sourceX, sourceY, sourcePosition, targetX, targetY, targetPosition })
    const style_: React.CSSProperties = data?statusToEdgeStyle(data.status, style):style;
    return <BaseEdge path={edgePath} markerEnd={markerEnd} style={style_} />
  }
}

const contract2NodesAndEdges = (contract: Contract, id: string, x: number, y: number, path: ContractPathHistory): { nodes: Node<ContractNodeData>[], edges: Edge<ContractEdgeData>[], max_y: number } => {
  var nodeStatus:ContractNodeStatus = contractPathHistoryToNodeStatus(contract, path);
  if (contract === "close")
    return {
      nodes: [{ id, position: { x, y }, data: { type: "close", status: nodeStatus }, type: "ContractNode" }],
      edges: [],
      max_y: y,
    }

  if ("if" in contract) {
    const if_status = contractPathHistoryToEdgeStatus(0, path);
    const else_status = contractPathHistoryToEdgeStatus(1, path);

    const if_path = contractPathHistoryContinuation(0, path);
    const else_path = contractPathHistoryContinuation(1, path);

    const { else: else_, then, ...type } = contract
    const then_id = `1-${id}`
    const else_id = `2-${id}`
    const { max_y: then_max_y, nodes: then_nodes, edges: then_edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y, if_path);
    const { max_y, nodes: else_nodes, edges: else_edges } = contract2NodesAndEdges(else_, else_id, x + X_OFFSET, then_max_y + Y_OFFSET, else_path);
    return {
      nodes: [
        ...then_nodes, // .map(node => index === 0 || index === undefined ? node : { ...node, data: { ...node.data, disabled: true } }),
        ...else_nodes, // .map(node => index === 1 || index === undefined ? node : { ...node, data: { ...node.data, disabled: true } }),
        { id, position: { x, y }, data: { type, status: nodeStatus }, type: "ContractNode" },
      ],
      edges: [
        ...then_edges, //.map(edge => index === 0 || index === undefined ? edge : { ...edge, data: edge.data && { ...edge.data, disabled: true } }),
        ...else_edges, // .map(edge => index === 1 || index === undefined ? edge : { ...edge, data: edge.data && { ...edge.data, disabled: true } }),
        { id: `then-${id}`, source: id, target: then_id, sourceHandle: "then", type: "ContractEdge", data: { status: if_status } },
        { id: `else-${id}`, source: id, target: else_id, sourceHandle: "else", type: "ContractEdge", data: { status: else_status } },
      ],
      max_y,
    }
  }

  if ("pay" in contract) {
    const status = contractPathHistoryToEdgeStatus(undefined, path);
    const { then, ...type } = contract
    const then_id = `1-${id}`
    const { max_y, nodes, edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y, path)
    return {
      nodes: [
        ...nodes,
        { id, position: { x, y }, data: { type, status: nodeStatus }, type: "ContractNode" },
      ],
      edges: [
        ...edges,
        { id: `then-${id}`, source: id, target: then_id, type: "ContractEdge", data: { status }}
      ],
      max_y,
    }
  }

  if ("let" in contract) {
    const status = contractPathHistoryToEdgeStatus(undefined, path);
    const { then, ...type } = contract
    const then_id = `1-${id}`
    const { max_y, nodes, edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y, path)
    return {
      nodes: [
        ...nodes,
        { id, position: { x, y }, data: { type, status: nodeStatus }, type: "ContractNode" },
      ],
      edges: [
        ...edges,
        { id: `then-${id}`, source: id, target: then_id, type: "ContractEdge", data: { status } },
      ],
      max_y,
    }
  }

  if ("assert" in contract) {
    const status = contractPathHistoryToEdgeStatus(undefined, path);

    const { then, ...type } = contract
    const then_id = `1-${id}`
    const { max_y, nodes, edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y, path)
    return {
      nodes: [
        ...nodes,
        { id, position: { x, y }, data: { type, status: nodeStatus }, type: "ContractNode" },
      ],
      edges: [
        ...edges,
        { id: `then-${id}`, source: id, target: then_id, type: "ContractEdge", data: { status }},
      ],
      max_y,
    }
  }

  if ("when" in contract) {
    const { timeout_continuation, ...type } = contract
    const { nodes, edges, max_y } = type.when.reduce<{ nodes: Node<ContractNodeData>[], edges: Edge<ContractEdgeData>[], max_y: number }>((acc, on:any, i:number) => {
      if ("then" in on) {
        const edgeStatus = contractPathHistoryToEdgeStatus(i, path);
        const pathContinuation = contractPathHistoryContinuation(i, path);
        const then_id = `${i}-${id}`
        const { nodes, edges, max_y } = contract2NodesAndEdges(on.then, then_id, x + X_OFFSET, acc.max_y + Y_OFFSET, pathContinuation);
        return {
          nodes: [
            ...nodes, // .map(node => index === i || index === undefined ? node : { ...node, data: { ...node.data, disabled: true } }),
            ...acc.nodes,
          ],
          edges: [
            ...edges, // .map(edge => index === i || index === undefined ? edge : { ...edge, data: edge.data && { ...edge.data, disabled: true } }),
            ...acc.edges,
            { id: `then-${then_id}-${id}`, source: id, target: then_id, sourceHandle: `${i}`, type: "ContractEdge", data: { status: edgeStatus }}
          ],
          max_y
        }
      }
      return acc
    }, {
      nodes: [
        { id, position: { x, y }, data: { type, status: nodeStatus}, type: "ContractNode" },
      ],
      edges: [],
      max_y: y - Y_OFFSET
    })
    const timeout_then_id = `${type.when.length}-${id}`
    const timeout_status = contractPathHistoryToEdgeStatus(null, path);
    const timeout_indices = contractPathHistoryContinuation(null, path);
    const timeout_graph = contract2NodesAndEdges(timeout_continuation, timeout_then_id, x + X_OFFSET, max_y + Y_OFFSET, timeout_indices);
    return {
      nodes: [
        ...timeout_graph.nodes, // .nodes.map(node => index === null || index === undefined ? node : { ...node, data: { ...node.data, disabled: true } }),
        ...nodes,
      ],
      edges: [
        ...timeout_graph.edges, //.edges.map(edge => index === null || index === undefined ? edge : { ...edge, data: edge.data && { ...edge.data, disabled: true } }),
        ...edges,
        { id: `timeout_continuation-${id}`, source: id, target: timeout_then_id, sourceHandle: "timeout_continuation", type: "ContractEdge", data: { status: timeout_status } },
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

export const _MarloweGraph = ({ contract, path, onInit }: { contract: Contract, path: ContractPathHistory, onInit: any }): JSX.Element => {
  const { nodes, edges } = contract2NodesAndEdges(contract, "1", 0, 0, path)
  return <ReactFlow onInit={onInit} nodes={nodes} edges={edges} nodeTypes={nodeTypes} edgeTypes={edgeTypes}><Background variant={BackgroundVariant.Dots} gap={12} size={1} /></ReactFlow>;
}
