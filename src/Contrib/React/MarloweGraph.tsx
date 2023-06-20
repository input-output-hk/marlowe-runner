import * as React from "react"
import { ReactFlow, Node, Edge, Background, BackgroundVariant, Handle, Position } from "reactflow";

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

const myNodeStyle: React.CSSProperties = {
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

type ContractNodeData
  = "close"
  | { pay: Value, token: Token, to: Payee, from_account: AccountId }
  | { if: Observation }
  | { when: ({ case: Action } | { case: Action, merkleized_then: string })[], timout: Timeout }
  | { let: ValueId, be: Value }
  | { assert: Observation }

const ContractNode = ({ data }: { data: ContractNodeData }): JSX.Element => {
  if (data === "close")
    return <div style={myNodeStyle}>
      Close
      <Handle type="target" position={Position.Left} />
    </div>

  if ("if" in data)
    return <div style={myNodeStyle}>
      If (...)
      <Handle type="target" position={Position.Left} id="continuation" />
      <Handle type="source" position={Position.Right} style={{ top: "8px" }} id="then" />
      <Handle type="source" position={Position.Right} style={{ top: "24px" }} id="else" />
    </div>

  if ("pay" in data)
    return <div style={myNodeStyle}>
      Pay (...)
      <Handle type="target" position={Position.Left} id="continuation" />
      <Handle type="source" position={Position.Right} id="then" />
    </div>

  if ("let" in data)
    return <div style={myNodeStyle}>
      Let (...)
      <Handle type="target" position={Position.Left} id="continuation" />
      <Handle type="source" position={Position.Right} id="then" />
    </div>

  if ("assert" in data)
    return <div style={myNodeStyle}>
      Assert (...)
      <Handle type="target" position={Position.Left} id="continuation" />
      <Handle type="source" position={Position.Right} id="then" />
    </div>

  if ("when" in data) {
    const height = (data.when.length + 1) * Y_OFFSET - (Y_OFFSET - NODE_HEIGHT)
    const { ...style } = {
      ...myNodeStyle,
      height: `${height}px`,
      justifyContent: "space-around",
      paddingLeft: "5px",
    }
    return <div style={style}>
      <div>When</div>
      {data.when.map((x, i) => {
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

  return <div style={myNodeStyle}>Not implemented!</div>
}

const nodeTypes = {
  ContractNode,
}

const contract2NodesAndEdges = (contract: Contract, id: string, x: number, y: number): { nodes: Node<ContractNodeData>[], edges: Edge<null>[], max_y: number } => {
  if (contract === "close")
    return {
      nodes: [{ id, position: { x, y }, data: "close", type: "ContractNode" }],
      edges: [],
      max_y: y,
    }

  if ("if" in contract) {
    const { else: else_, then, ...data } = contract
    const then_id = `1-${id}`
    const else_id = `2-${id}`
    const { max_y: then_max_y, nodes: then_nodes, edges: then_edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y)
    const { max_y, nodes: else_nodes, edges: else_edges } = contract2NodesAndEdges(else_, else_id, x + X_OFFSET, then_max_y + Y_OFFSET)
    return {
      nodes: [
        ...then_nodes,
        ...else_nodes,
        { id, position: { x, y }, data, type: "ContractNode" },
      ],
      edges: [
        ...then_edges,
        ...else_edges,
        { id: `then-${id}`, source: id, target: then_id, sourceHandle: "then" },
        { id: `else-${id}`, source: id, target: else_id, sourceHandle: "else" },
      ],
      max_y,
    }
  }

  if ("pay" in contract) {
    const { then, ...data } = contract
    const then_id = `1-${id}`
    const { max_y, nodes, edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y)
    return {
      nodes: [
        ...nodes,
        { id, position: { x, y }, data, type: "ContractNode" },
      ],
      edges: [
        ...edges,
        { id: `then-${id}`, source: id, target: then_id },
      ],
      max_y,
    }
  }

  if ("let" in contract) {
    const { then, ...data } = contract
    const then_id = `1-${id}`
    const { max_y, nodes, edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y)
    return {
      nodes: [
        ...nodes,
        { id, position: { x, y }, data, type: "ContractNode" },
      ],
      edges: [
        ...edges,
        { id: `then-${id}`, source: id, target: then_id },
      ],
      max_y,
    }
  }

  if ("assert" in contract) {
    const { then, ...data } = contract
    const then_id = `1-${id}`
    const { max_y, nodes, edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y)
    return {
      nodes: [
        ...nodes,
        { id, position: { x, y }, data, type: "ContractNode" },
      ],
      edges: [
        ...edges,
        { id: `then-${id}`, source: id, target: then_id },
      ],
      max_y,
    }
  }

  if ("when" in contract) {
    const { timeout_continuation, ...data } = contract
    const { nodes, edges, max_y } = data.when.reduce<{ nodes: Node<ContractNodeData>[], edges: Edge<null>[], max_y: number }>((acc, on, i) => {
      if ("then" in on) {
        const then_id = `${i}-${id}`
        const { nodes, edges, max_y } = contract2NodesAndEdges(on.then, then_id, x + X_OFFSET, acc.max_y + Y_OFFSET)
        return {
          nodes: [
            ...nodes,
            ...acc.nodes,
          ],
          edges: [
            ...edges,
            ...acc.edges,
            { id: `then-${then_id}-${id}`, source: id, target: then_id, sourceHandle: `${i}` },
          ],
          max_y
        }
      }
      return acc
    }, {
      nodes: [
        { id, position: { x, y }, data, type: "ContractNode" },
      ],
      edges: [],
      max_y: y - Y_OFFSET
    })
    const timeout_then_id = `${data.when.length}-${id}`
    const timeout_graph = contract2NodesAndEdges(timeout_continuation, timeout_then_id, x + X_OFFSET, max_y + Y_OFFSET)
    return {
      nodes: [
        ...timeout_graph.nodes,
        ...nodes,
      ],
      edges: [
        ...timeout_graph.edges,
        ...edges,
        { id: `timeout_continuation-${id}`, source: id, target: timeout_then_id, sourceHandle: "timeout_continuation" },
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

export const _MarloweGraph = ({ contract }: { contract: Contract }): JSX.Element => {
  const { nodes, edges } = contract2NodesAndEdges(contract, "1", 0, 0)
  return <ReactFlow nodes={nodes} edges={edges} nodeTypes={nodeTypes}>
            <Background variant={BackgroundVariant.Dots} gap={12} size={1} />
         </ReactFlow>
}

