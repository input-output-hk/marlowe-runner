import playwright from 'playwright';
import * as fs from 'fs';
import { When } from '@cucumber/cucumber';
import { ScenarioWorld } from './setup/world.js';
import { ValidAccessibilityRoles } from '../env/global.js';
import { waitFor } from "../support/wait-for-behavior.js";
import {
  inputValue,
} from '../support/html-behavior.js';
import {
  Address,
  Contract,
  datetoTimeout,
} from "@marlowe.io/language-core-v1";
import { MarloweJSON } from "@marlowe.io/adapter/codec";

type ContractName = "SimpleDeposit";

const mkSimpleDeposit = (address: string): Contract => {
  const twentyMinutesInMilliseconds = 20 * 60 * 1000;
  const inTwentyMinutes = datetoTimeout(new Date(Date.now() + twentyMinutesInMilliseconds));
  return {
    timeout: inTwentyMinutes,
    timeout_continuation: "close",
    when: [
      { case: {
          party: {address: address},
          deposits: 1n,
          of_token: { currency_symbol: "", token_name: "" },
          into_account: {address: address}
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
    const {
      globalStateManager
    } = this;
    const walletAddress = globalStateManager.getValue("wallet-address");
    switch (contractName) {
      case "SimpleDeposit":
        const contract = mkSimpleDeposit(walletAddress);
        globalStateManager.appendValue(fileName, MarloweJSON.stringify(contract, null, 4))
        break;
    }
  }
);