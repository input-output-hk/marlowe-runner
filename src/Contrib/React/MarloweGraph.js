var __rest = (this && this.__rest) || function (s, e) {
    var t = {};
    for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p) && e.indexOf(p) < 0)
        t[p] = s[p];
    if (s != null && typeof Object.getOwnPropertySymbols === "function")
        for (var i = 0, p = Object.getOwnPropertySymbols(s); i < p.length; i++) {
            if (e.indexOf(p[i]) < 0 && Object.prototype.propertyIsEnumerable.call(s, p[i]))
                t[p[i]] = s[p[i]];
        }
    return t;
};
import * as React from "react";
import { ReactFlow, Background, BackgroundVariant, Handle, Position } from "reactflow";
const X_OFFSET = 200;
const Y_OFFSET = 40;
const NODE_HEIGHT = 30;
const myNodeStyle = {
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
};
const ContractNode = ({ data }) => {
    if (data === "close")
        return React.createElement("div", { style: myNodeStyle },
            "Close",
            React.createElement(Handle, { type: "target", position: Position.Left }));
    if ("if" in data)
        return React.createElement("div", { style: myNodeStyle },
            "If (...)",
            React.createElement(Handle, { type: "target", position: Position.Left, id: "continuation" }),
            React.createElement(Handle, { type: "source", position: Position.Right, style: { top: "8px" }, id: "then" }),
            React.createElement(Handle, { type: "source", position: Position.Right, style: { top: "24px" }, id: "else" }));
    if ("pay" in data)
        return React.createElement("div", { style: myNodeStyle },
            "Pay (...)",
            React.createElement(Handle, { type: "target", position: Position.Left, id: "continuation" }),
            React.createElement(Handle, { type: "source", position: Position.Right, id: "then" }));
    if ("let" in data)
        return React.createElement("div", { style: myNodeStyle },
            "Let (...)",
            React.createElement(Handle, { type: "target", position: Position.Left, id: "continuation" }),
            React.createElement(Handle, { type: "source", position: Position.Right, id: "then" }));
    if ("assert" in data)
        return React.createElement("div", { style: myNodeStyle },
            "Assert (...)",
            React.createElement(Handle, { type: "target", position: Position.Left, id: "continuation" }),
            React.createElement(Handle, { type: "source", position: Position.Right, id: "then" }));
    if ("when" in data) {
        const height = (data.when.length + 1) * Y_OFFSET - (Y_OFFSET - NODE_HEIGHT);
        const style = __rest(Object.assign(Object.assign({}, myNodeStyle), { height: `${height}px`, justifyContent: "space-around", paddingLeft: "5px" }), []);
        return React.createElement("div", { style: style },
            React.createElement("div", null, "When"),
            data.when.map((x, i) => {
                if ("merkleized_then" in x) {
                    if ("deposits" in x.case)
                        return React.createElement("div", { key: i }, "Deposit (...)");
                    if ("choose_between" in x.case)
                        return React.createElement("div", { key: i }, "Choice (...)");
                    if ("notify_if" in x.case)
                        return React.createElement("div", { key: i }, "Notify (...)");
                }
                else {
                    if ("deposits" in x.case)
                        return React.createElement("div", { key: i },
                            "Deposit (...)",
                            React.createElement(Handle, { type: "source", position: Position.Right, style: { top: `${i * 32 + 47}px` }, id: `${i}` }));
                    if ("choose_between" in x.case)
                        return React.createElement("div", { key: i },
                            "Choice (...)",
                            React.createElement(Handle, { type: "source", position: Position.Right, style: { top: `${i * 32 + 47}px` }, id: `${i}` }));
                    if ("notify_if" in x.case)
                        return React.createElement("div", { key: i },
                            "Notify (...)",
                            React.createElement(Handle, { type: "source", position: Position.Right, style: { top: `${i * 32 + 47}px` }, id: `${i}` }));
                }
            }),
            React.createElement("div", null, "on timeout:"),
            React.createElement(Handle, { type: "target", position: Position.Left, style: { top: `${NODE_HEIGHT / 2}px` }, id: "continuation" }),
            React.createElement(Handle, { type: "source", position: Position.Right, style: { top: `${height - NODE_HEIGHT / 2}px` }, id: "timeout_continuation" }));
    }
    return React.createElement("div", { style: myNodeStyle }, "Not implemented!");
};
const nodeTypes = {
    ContractNode,
};
const contract2NodesAndEdges = (contract, id, x, y) => {
    if (contract === "close")
        return {
            nodes: [{ id, position: { x, y }, data: "close", type: "ContractNode" }],
            edges: [],
            max_y: y,
        };
    if ("if" in contract) {
        const { else: else_, then } = contract, data = __rest(contract, ["else", "then"]);
        const then_id = `1-${id}`;
        const else_id = `2-${id}`;
        const { max_y: then_max_y, nodes: then_nodes, edges: then_edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y);
        const { max_y, nodes: else_nodes, edges: else_edges } = contract2NodesAndEdges(else_, else_id, x + X_OFFSET, then_max_y + Y_OFFSET);
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
        };
    }
    if ("pay" in contract) {
        const { then } = contract, data = __rest(contract, ["then"]);
        const then_id = `1-${id}`;
        const { max_y, nodes, edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y);
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
        };
    }
    if ("let" in contract) {
        const { then } = contract, data = __rest(contract, ["then"]);
        const then_id = `1-${id}`;
        const { max_y, nodes, edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y);
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
        };
    }
    if ("assert" in contract) {
        const { then } = contract, data = __rest(contract, ["then"]);
        const then_id = `1-${id}`;
        const { max_y, nodes, edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y);
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
        };
    }
    if ("when" in contract) {
        const { timeout_continuation } = contract, data = __rest(contract, ["timeout_continuation"]);
        const { nodes, edges, max_y } = data.when.reduce((acc, on, i) => {
            if ("then" in on) {
                const then_id = `${i}-${id}`;
                const { nodes, edges, max_y } = contract2NodesAndEdges(on.then, then_id, x + X_OFFSET, acc.max_y + Y_OFFSET);
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
                };
            }
            return acc;
        }, {
            nodes: [
                { id, position: { x, y }, data, type: "ContractNode" },
            ],
            edges: [],
            max_y: y - Y_OFFSET
        });
        const timeout_then_id = `${data.when.length}-${id}`;
        const timeout_graph = contract2NodesAndEdges(timeout_continuation, timeout_then_id, x + X_OFFSET, max_y + Y_OFFSET);
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
        };
    }
    return {
        nodes: [],
        edges: [],
        max_y: y,
    };
};
export const _MarloweGraph = ({ contract }) => {
    const { nodes, edges } = contract2NodesAndEdges(contract, "1", 0, 0);
    return React.createElement(ReactFlow, { nodes: nodes, edges: edges, nodeTypes: nodeTypes },
        React.createElement(Background, { variant: BackgroundVariant.Dots, gap: 12, size: 1 }));
};
