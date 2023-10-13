import { ContractPathHistory, _MarloweGraph } from './MarloweGraph';
import { Contract } from './MarloweGraph/Marlowe';
import { createRoot } from 'react-dom/client';

const contract: Contract = {
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
          onClick(state:any) {
            console.log(`hey ho: ${state}`)
          },
        }
      },
      thenEdgeEvents: { onClick() { console.log("yo yo!") } }
    },
    {
      case: {
        for_choice:
        {
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
      onMouseOver() { console.log("lolcz!") }
    }
  }
}

const noop = () => { return; }

export const MarloweGraphView = ({ contract, path, onInit }: { contract: Contract, path?: ContractPathHistory, onInit: any }): JSX.Element => {
  return (
    <div style={{ width: "95vw", height: "95vh" }}>
      { _MarloweGraph({contract, path, onInit}) }
    </div>
  );
}

const root = createRoot(document.getElementById("app") as HTMLElement);

const contract2: Contract = {
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
  ],
  timeout: 1985,
  timeout_continuation: "close"
}

root.render(
  <div>
    <MarloweGraphView contract={contract2} path={[0, 0]} onInit={noop} />
    <MarloweGraphView contract={contract} path={[1]} onInit={noop} />
    <MarloweGraphView contract={contract} path={[1, 0]} onInit={noop} />
    <MarloweGraphView contract={contract} path={[1, 0, 0]} onInit={noop} />
  </div>
);



// https://reactflow.dev/docs/concepts/terms-and-definitions
// https://reactflow.dev/docs/guides/custom-nodes
// https://css-tricks.com/snippets/css/a-guide-to-flexboix
