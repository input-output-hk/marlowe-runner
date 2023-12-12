import { When } from '@cucumber/cucumber';
import { ScenarioWorld } from '../world.js';
import {
  Contract,
  datetoTimeout,
} from "@marlowe.io/language-core-v1";
import { Bech32 } from '../../cardano.js';


const mkDoubleDeposit = (address1: Bech32, address2: Bech32): Contract => {
  const twentyMinutesInMilliseconds = 20 * 60 * 1000;
  const inTwentyMinutes = datetoTimeout(new Date(Date.now() + twentyMinutesInMilliseconds));
  return {
    timeout: inTwentyMinutes,
    timeout_continuation: "close",
    when: [
      { case: {
          party: {address: address1.toString()},
          deposits: 1000000n,
          of_token: { currency_symbol: "", token_name: "" },
          into_account: {address: address1.toString()}
        },
        then: "close",
      },
      { case: {
          party: {address: address2.toString()},
          deposits: 2000000n,
          of_token: { currency_symbol: "", token_name: "" },
          into_account: {address: address2.toString()}
        },
        then: "close",
      },
    ]
  };
}

When(
  /^I generate "DoubleDeposit" contract with "([^"]*)" as a first depositor and "([^"]*)" as a second depositor and call it "([^"]*)"$/,
  async function(this: ScenarioWorld, first: string, second: string, contractNickname: string) {
    const firstAddress = await this.getWalletAddress(first);
    const secondAddress = await this.getWalletAddress(second);
    const contract = mkDoubleDeposit(firstAddress, secondAddress);
    this.setContractInfo(contractNickname, { contract: contract, contractId: undefined });
  }
);

