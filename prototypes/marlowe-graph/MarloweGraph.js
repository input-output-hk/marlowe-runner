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
import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
// import { createRoot } from "react-dom/client"
import { ReactFlow, Background, BackgroundVariant, Handle, Position, BaseEdge, getBezierPath, } from "reactflow";
import 'reactflow/dist/style.css';
const X_OFFSET = 200;
const Y_OFFSET = 40;
const NODE_HEIGHT = 30;
const contractNodeStyle = {
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
};
const nodeTypes = {
    ContractNode({ data: { type, status } }) {
        const style_ = statusToNodeStyle(status, contractNodeStyle);
        if (type === "close")
            return _jsxs("div", { style: style_, children: ["Close", _jsx(Handle, { type: "target", position: Position.Left })] });
        if ("if" in type)
            return _jsxs("div", { style: style_, children: ["If (...)", _jsx(Handle, { type: "target", position: Position.Left, id: "continuation" }), _jsx(Handle, { type: "source", position: Position.Right, style: { top: "8px" }, id: "then" }), _jsx(Handle, { type: "source", position: Position.Right, style: { top: "24px" }, id: "else" })] });
        if ("pay" in type)
            return _jsxs("div", { style: style_, children: ["Pay (...)", _jsx(Handle, { type: "target", position: Position.Left, id: "continuation" }), _jsx(Handle, { type: "source", position: Position.Right, id: "then" })] });
        if ("let" in type)
            return _jsxs("div", { style: style_, children: ["Let (...)", _jsx(Handle, { type: "target", position: Position.Left, id: "continuation" }), _jsx(Handle, { type: "source", position: Position.Right, id: "then" })] });
        if ("assert" in type)
            return _jsxs("div", { style: style_, children: ["Assert (...)", _jsx(Handle, { type: "target", position: Position.Left, id: "continuation" }), _jsx(Handle, { type: "source", position: Position.Right, id: "then" })] });
        if ("when" in type) {
            // FIXME: an ugly workaround to fix the styling of empty `When` node
            const height = ((type.when.length === 0) ? 1.5 : (type.when.length + 1)) * Y_OFFSET - (Y_OFFSET - NODE_HEIGHT);
            const _a = Object.assign(Object.assign({}, style_), { height: `${height}px`, justifyContent: "space-around", paddingLeft: "5px" }), { alignItems: _ } = _a, style = __rest(_a, ["alignItems"]);
            return _jsxs("div", { style: style, children: [_jsx("div", { children: "When" }), type.when.map((x, i) => {
                        if ("merkleized_then" in x) {
                            if ("deposits" in x.case)
                                return _jsx("div", { children: "Deposit (...)" }, i);
                            if ("choose_between" in x.case)
                                return _jsx("div", { children: "Choice (...)" }, i);
                            if ("notify_if" in x.case)
                                return _jsx("div", { children: "Notify (...)" }, i);
                        }
                        else {
                            if ("deposits" in x.case)
                                return _jsxs("div", { children: ["Deposit (...)", _jsx(Handle, { type: "source", position: Position.Right, style: { top: `${i * 32 + 47}px` }, id: `${i}` })] }, i);
                            if ("choose_between" in x.case)
                                return _jsxs("div", { children: ["Choice (...)", _jsx(Handle, { type: "source", position: Position.Right, style: { top: `${i * 32 + 47}px` }, id: `${i}` })] }, i);
                            if ("notify_if" in x.case)
                                return _jsxs("div", { children: ["Notify (...)", _jsx(Handle, { type: "source", position: Position.Right, style: { top: `${i * 32 + 47}px` }, id: `${i}` })] }, i);
                        }
                    }), _jsx("div", { children: "on timeout:" }), _jsx(Handle, { type: "target", position: Position.Left, style: { top: `${NODE_HEIGHT / 2}px` }, id: "continuation" }), _jsx(Handle, { type: "source", position: Position.Right, style: { top: `${height - NODE_HEIGHT / 2}px` }, id: "timeout_continuation" })] });
        }
        return _jsx("div", { style: style_, children: "Not implemented!" });
    }
};
const contractPathHistoryToNodeStatus = (contract, history) => {
    if (history === 'still-possible')
        return 'still-possible';
    if (history === 'skipped')
        return 'skipped';
    if (history.indices.length === 0 && contract !== "close" && "when" in contract)
        return 'awaiting-input';
    const selecting = history.selecting || history.indices[0] === 'start-selection';
    if (selecting)
        return 'selected';
    return 'executed';
};
// In both functions the value of `index` value has following semantics:
// * `undefined` - we are inside the node which is not branching node (`Assert`, `Let`, `Pay`)
// * `null` - we are in when and checking continuation for timeout
// * `number` - we are in some branching node and picking the branch
const contractPathHistoryToEdgeStatus = (index, history) => {
    if (history === 'still-possible')
        return 'still-possible';
    if (history === 'skipped')
        return 'skipped';
    if (history.indices.length === 0 && index !== undefined)
        return 'still-possible';
    const indices = history.indices[0] === 'start-selection' ? history.indices.slice(1) : history.indices;
    const selecting = history.selecting || history.indices[0] === 'start-selection';
    if (index === indices[0] || index === undefined)
        if (selecting)
            return 'selected';
        else
            return 'executed';
    return 'skipped';
};
const contractPathHistoryContinuation = (index, history) => {
    if (history == 'still-possible')
        return 'still-possible';
    if (history == 'skipped')
        return 'skipped';
    if (index === undefined)
        return history;
    if (history.indices.length === 0)
        return 'still-possible';
    if (history.indices[0] === 'start-selection')
        return contractPathHistoryContinuation(index, { indices: history.indices.slice(1), selecting: true });
    if (index === history.indices[0])
        return { indices: history.indices.slice(1), selecting: history.selecting };
    return 'skipped';
};
const executedStrokeStyle = { strokeOpacity: "100%", strokeWidth: 3, stroke: "black" };
const selectedStrokeStyle = { strokeOpacity: "100%", strokeWidth: 3, stroke: "#aaa" };
const stillPossibleStrokeStyle = { strokeOpacity: "100%", strokeWidth: 1, stroke: "gray" };
const skippedStrokeStyle = { strokeOpacity: "30%", strokeWidth: 1, stroke: "black" };
const executedDivStyle = { border: "3px solid black" };
const selectedDivStyle = { border: "3px solid #aaa" };
const stillPossibleDivStyle = { border: "1px solid gray" };
const awaitingDivStyle = { border: "2px solid gray" };
const skippedDivStyle = { border: "1px solid black" };
const statusToEdgeStyle = (status, defaultStyle) => {
    switch (status) {
        case 'executed':
            return Object.assign(Object.assign({}, defaultStyle), executedStrokeStyle);
        case 'selected':
            return Object.assign(Object.assign({}, defaultStyle), selectedStrokeStyle);
        case 'skipped':
            return Object.assign(Object.assign({}, defaultStyle), skippedStrokeStyle);
        case 'still-possible':
            return Object.assign(Object.assign({}, defaultStyle), stillPossibleStrokeStyle);
    }
};
const statusToNodeStyle = (status, defaultStyle) => {
    switch (status) {
        case 'executed':
            return Object.assign(Object.assign({}, defaultStyle), executedDivStyle);
        case 'selected':
            return Object.assign(Object.assign({}, defaultStyle), selectedDivStyle);
        case 'awaiting-input':
            return Object.assign(Object.assign({}, defaultStyle), awaitingDivStyle);
        case 'still-possible':
            return Object.assign(Object.assign({}, defaultStyle), stillPossibleDivStyle);
        case 'skipped':
            return Object.assign(Object.assign(Object.assign({}, defaultStyle), skippedDivStyle), { opacity: "30%" });
    }
};
const edgeTypes = {
    ContractEdge({ data, sourceX, sourceY, sourcePosition, targetX, targetY, targetPosition, markerEnd, style = {} }) {
        const [edgePath] = getBezierPath({ sourceX, sourceY, sourcePosition, targetX, targetY, targetPosition });
        const style_ = data ? statusToEdgeStyle(data.status, style) : style;
        return _jsx(BaseEdge, { path: edgePath, markerEnd: markerEnd, style: style_ });
    }
};
const contract2NodesAndEdges = (contract, id, x, y, path) => {
    var nodeStatus = contractPathHistoryToNodeStatus(contract, path);
    if (contract === "close")
        return {
            nodes: [{ id, position: { x, y }, data: { type: "close", status: nodeStatus }, type: "ContractNode" }],
            edges: [],
            max_y: y,
        };
    if ("if" in contract) {
        const if_status = contractPathHistoryToEdgeStatus(0, path);
        const else_status = contractPathHistoryToEdgeStatus(1, path);
        const if_path = contractPathHistoryContinuation(0, path);
        const else_path = contractPathHistoryContinuation(1, path);
        const { else: else_, then } = contract, type = __rest(contract, ["else", "then"]);
        const then_id = `1-${id}`;
        const else_id = `2-${id}`;
        const { max_y: then_max_y, nodes: then_nodes, edges: then_edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y, if_path);
        const { max_y, nodes: else_nodes, edges: else_edges } = contract2NodesAndEdges(else_, else_id, x + X_OFFSET, then_max_y + Y_OFFSET, else_path);
        return {
            nodes: [
                ...then_nodes,
                ...else_nodes,
                { id, position: { x, y }, data: { type, status: nodeStatus }, type: "ContractNode" },
            ],
            edges: [
                ...then_edges,
                ...else_edges,
                { id: `then-${id}`, source: id, target: then_id, sourceHandle: "then", type: "ContractEdge", data: { status: if_status } },
                { id: `else-${id}`, source: id, target: else_id, sourceHandle: "else", type: "ContractEdge", data: { status: else_status } },
            ],
            max_y,
        };
    }
    if ("pay" in contract) {
        const status = contractPathHistoryToEdgeStatus(undefined, path);
        const { then } = contract, type = __rest(contract, ["then"]);
        const then_id = `1-${id}`;
        const { max_y, nodes, edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y, path);
        return {
            nodes: [
                ...nodes,
                { id, position: { x, y }, data: { type, status: nodeStatus }, type: "ContractNode" },
            ],
            edges: [
                ...edges,
                { id: `then-${id}`, source: id, target: then_id, type: "ContractEdge", data: { status } }
            ],
            max_y,
        };
    }
    if ("let" in contract) {
        const status = contractPathHistoryToEdgeStatus(undefined, path);
        const { then } = contract, type = __rest(contract, ["then"]);
        const then_id = `1-${id}`;
        const { max_y, nodes, edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y, path);
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
        };
    }
    if ("assert" in contract) {
        const status = contractPathHistoryToEdgeStatus(undefined, path);
        const { then } = contract, type = __rest(contract, ["then"]);
        const then_id = `1-${id}`;
        const { max_y, nodes, edges } = contract2NodesAndEdges(then, then_id, x + X_OFFSET, y, path);
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
        };
    }
    if ("when" in contract) {
        const { timeout_continuation } = contract, type = __rest(contract, ["timeout_continuation"]);
        const { nodes, edges, max_y } = type.when.reduce((acc, on, i) => {
            if ("then" in on) {
                const edgeStatus = contractPathHistoryToEdgeStatus(i, path);
                const pathContinuation = contractPathHistoryContinuation(i, path);
                const then_id = `${i}-${id}`;
                const { nodes, edges, max_y } = contract2NodesAndEdges(on.then, then_id, x + X_OFFSET, acc.max_y + Y_OFFSET, pathContinuation);
                return {
                    nodes: [
                        ...nodes,
                        ...acc.nodes,
                    ],
                    edges: [
                        ...edges,
                        ...acc.edges,
                        { id: `then-${then_id}-${id}`, source: id, target: then_id, sourceHandle: `${i}`, type: "ContractEdge", data: { status: edgeStatus } }
                    ],
                    max_y
                };
            }
            return acc;
        }, {
            nodes: [
                { id, position: { x, y }, data: { type, status: nodeStatus }, type: "ContractNode" },
            ],
            edges: [],
            max_y: y - Y_OFFSET
        });
        const timeout_then_id = `${type.when.length}-${id}`;
        const timeout_status = contractPathHistoryToEdgeStatus(null, path);
        const timeout_indices = contractPathHistoryContinuation(null, path);
        const timeout_graph = contract2NodesAndEdges(timeout_continuation, timeout_then_id, x + X_OFFSET, max_y + Y_OFFSET, timeout_indices);
        return {
            nodes: [
                ...timeout_graph.nodes,
                ...nodes,
            ],
            edges: [
                ...timeout_graph.edges,
                ...edges,
                { id: `timeout_continuation-${id}`, source: id, target: timeout_then_id, sourceHandle: "timeout_continuation", type: "ContractEdge", data: { status: timeout_status } },
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
export const _MarloweGraph = ({ contract, path, onInit }) => {
    const contractPath = { indices: path, selecting: false };
    const { nodes, edges } = contract2NodesAndEdges(contract, "1", 0, 0, contractPath);
    return _jsx(ReactFlow, { onInit: onInit, nodes: nodes, edges: edges, nodeTypes: nodeTypes, edgeTypes: edgeTypes, children: _jsx(Background, { variant: BackgroundVariant.Dots, gap: 12, size: 1 }) });
};
