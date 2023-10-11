import { jsx as _jsx } from "react/jsx-runtime";
import { _MarloweGraph } from './MarloweGraph';
import { createRoot } from 'react-dom/client';
const contract = {
    when: [
        {
            case: {
                deposits: 500,
                party: { role_token: "Party A" },
                of_token: { currency_symbol: "", token_name: "" },
                into_account: { role_token: "Party B" }
            },
            then: { if: true, then: "close", else: "close" }
        },
        {
            case: { notify_if: false },
            then: {
                when: [
                    {
                        case: {
                            deposits: 200,
                            party: { role_token: "Party A" },
                            of_token: { currency_symbol: "", token_name: "" },
                            into_account: { role_token: "Party B" }
                        },
                        then: { if: true, then: { assert: true, then: "close" }, else: "close" }
                    },
                ],
                timeout: 2001,
                timeout_continuation: "close",
                nodeEvents: {
                    onClick(state) {
                        console.log(`hey ho: ${state}`);
                    },
                }
            },
            thenEdgeEvents: { onClick() { console.log("yo yo!"); } }
        },
        {
            case: {
                for_choice: {
                    choice_name: "The way",
                    choice_owner: { role_token: "Party C" }
                },
                choose_between: [
                    { from: 1, to: 3 },
                    { from: 5, to: 8 }
                ]
            },
            merkleized_then: "huey-dewey-louie"
        },
        {
            case: { notify_if: true },
            then: { let: "x", be: 13, then: "close" }
        }
    ],
    timeout: 1985,
    timeout_continuation: {
        if: true,
        then: {
            if: false,
            then: "close",
            else: {
                pay: 1234,
                token: { currency_symbol: "", token_name: "" },
                to: { party: { role_token: "Owner" } },
                from_account: { role_token: "Participant" },
                then: "close",
            }
        },
        else: { let: "x", be: 13, then: { assert: true, then: "close" } },
        elseEdgeEvents: {
            onMouseOver() { console.log("lolcz!"); }
        }
    }
};
const noop = () => { return; };
export const MarloweGraphView = ({ contract, path, onInit }) => {
    return (_jsx("div", { style: { width: "95vw", height: "95vh" }, children: _MarloweGraph({ contract, path, onInit }) }));
};
console.log("TES");
console.log(document.getElementById("app"));
const root = createRoot(document.getElementById("app"));
root.render(_jsx(MarloweGraphView, { contract: contract, path: [1, 0], onInit: noop }));
