import { When } from '@cucumber/cucumber';
import { ScenarioWorld } from './world.js';
import {
  Contract,
  datetoTimeout,
} from "@marlowe.io/language-core-v1";
import { MarloweJSON } from "@marlowe.io/adapter/codec";
import { Bech32 } from '../cardano.js';

type ContractName = "SimpleDeposit" | "SimpleChoice" | "TimedOutSimpleChoice" | "SimpleNotify";

const mkSimpleDeposit = (address: Bech32): Contract => {
  const twentyMinutesInMilliseconds = 20 * 60 * 1000;
  const inTwentyMinutes = datetoTimeout(new Date(Date.now() + twentyMinutesInMilliseconds));
  return {
    timeout: inTwentyMinutes,
    timeout_continuation: "close",
    when: [
      { case: {
          party: {address: address.toString()},
          deposits: 1n,
          of_token: { currency_symbol: "", token_name: "" },
          into_account: {address: address.toString()}
        },
        then: "close",
      },
    ]
  };
}

const mkSimpleChoice = (address: Bech32): Contract => {
  const twentyMinutesInMilliseconds = 20 * 60 * 1000;
  const inTwentyMinutes = datetoTimeout(new Date(Date.now() + twentyMinutesInMilliseconds));
  return {
    timeout: inTwentyMinutes,
    timeout_continuation: "close",
    when: [
      { case: {
          choose_between:
            [{
              from: 1n,
              to: 2n
            }],
          for_choice: {
            choice_owner: {address: address.toString()},
            choice_name: "simpleChoice",
          }
        },
        then: "close",
      },
    ]
  };
}

const mkTimedOutSimpleChoice = (address: Bech32): Contract => {
  const twentyMinutesInMilliseconds = 20 * 60 * 1000;
  const inTwentyMinutes = datetoTimeout(new Date(Date.now() - twentyMinutesInMilliseconds));
  return {
    timeout: inTwentyMinutes,
    timeout_continuation: "close",
    when: [
      { case: {
          choose_between:
            [{
              from: 1n,
              to: 2n
            }],
          for_choice: {
            choice_owner: {address: address.toString()},
            choice_name: "simpleChoice",
          }
        },
        then: "close",
      },
    ]
  };
}
const mkSimpleNotify = (): Contract => {
  const twentyMinutesInMilliseconds = 20 * 60 * 1000;
  const inTwentyMinutes = datetoTimeout(new Date(Date.now() - twentyMinutesInMilliseconds));
  return {
    timeout: inTwentyMinutes,
    timeout_continuation: "close",
    when: [
      { case: {
          notify_if: true,
        },
        then: "close",
      },
    ]
  };
}


// // And I generate the contract "SimpleDeposit" and write it to "/tmp/deposit.json"
When(
  /^I generate the contract "([^"]*)" and write it to "([^"]*)"/,
  async function(this: ScenarioWorld, contractName: ContractName, fileName: string) {
    const walletAddress = await this.getWalletAddress();
    const { globalStateManager } = this;
    switch (contractName) {
      case "SimpleDeposit":
        const contract1 = mkSimpleDeposit(walletAddress);
        console.log("contract1: ")
        console.log(contract1)
        console.log("contract1 JSON: ")
        console.log(MarloweJSON.stringify(contract1, null, 4))
        globalStateManager.appendValue(fileName, MarloweJSON.stringify(contract1, null, 4))
        break;
      case "SimpleChoice":
        const contract2 = mkSimpleChoice(walletAddress);
        globalStateManager.appendValue(fileName, MarloweJSON.stringify(contract2, null, 4))
        break;
      case "TimedOutSimpleChoice":
        const contract3 = mkTimedOutSimpleChoice(walletAddress);
        globalStateManager.appendValue(fileName, MarloweJSON.stringify(contract3, null, 4))
        break;
      case "SimpleNotify":
        const contract4 = mkSimpleNotify();
        globalStateManager.appendValue(fileName, MarloweJSON.stringify(contract4, null, 4))
        break;
      default:
        throw new Error("Unknown contract type: " + contractName);
    }
  }
);


