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

const mkSimpleDeposit = (address: Address): Contract => {
  const twentyMinutesInMilliseconds = 20 * 60 * 1000;
  const inTwentyMinutes = datetoTimeout(new Date(Date.now() + twentyMinutesInMilliseconds));
  return {
    timeout: inTwentyMinutes,
    timeout_continuation: "close",
    when: [
      { case: {
          party: address,
          deposits: 1n,
          of_token: { currency_symbol: "", token_name: "" },
          into_account: address
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
    const walletAddress = globalStateManager.getValue("wallet-address") as Address;
    switch (contractName) {
      case "SimpleDeposit":
        const contract = mkSimpleDeposit(walletAddress);
        fs.writeFileSync(fileName, MarloweJSON.stringify(contract, null, 4));
        break;
    }
  }
);


When(
  /^I click the "([^"]*)" with "([^"]*)" text And sign the transaction$/,
  async function(this: ScenarioWorld, role: ValidAccessibilityRoles,  name: string) {
    const {
      screen: { page },
      globalStateManager
    } = this;

    let newPagePromise;

    newPagePromise = new Promise(resolve => page.context().once('page', resolve));

    await waitFor(async() => {
      const locator = await page.getByRole(role, { name, exact: true });
      const result = await locator.isVisible();
      if (result) {
        await locator.click();
        return result;
      }
    });

    // Await for new page to popup
    const newPage = await newPagePromise as playwright.Page;

    await waitFor(async() => {
      await newPage.reload();
      return true;
    });

    await waitFor(async() => {
      const buttonName = "Confirm"
      const locator = await newPage.getByRole("button", { name: buttonName, exact: true });
      const result = await locator.isVisible();
      if (result) {
        await locator.click();
        return result;
      }
    });

    await waitFor(async() => {
      // const locator = await page.getByRole(role as ValidAccessibilityRoles, { name })
      // const locator = await newPage.getByRole("textbox", { name: "Password" })  
      const locator = await newPage.getByTestId("password-input")
      const result = await locator.isVisible();

      if (result) {
        const password = process.env.LACE_WALLET_PASSWORD as string
        await inputValue(locator, password);
        return result;
      }
    });

    await waitFor(async() => {
      const buttonName = "Confirm"
      const locator = await newPage.getByRole("button", { name: buttonName, exact: true });
      const result = await locator.isVisible();
      if (result) {
        await locator.click();
        return result;
      }
    });

    await waitFor(async() => {
      const buttonName = "Close"
      const locator = await newPage.getByRole("button", { name: buttonName, exact: true });
      const result = await locator.isVisible();
      if (result) {
        await locator.click();
        return result;
      }
    });


  }
);


When(
  /^I click the new tab "link" with "([^"]*)" text$/,
  async function(this: ScenarioWorld,  name: string) {
    const {
      screen: { page },
      globalStateManager
    } = this;

    let newPagePromise;

    newPagePromise = new Promise(resolve => page.context().once('page', resolve));

    await waitFor(async() => {
      const locator = await page.getByRole("link", { name, exact: true });
      const result = await locator.isVisible();
      if (result) {
        await locator.click();
        return result;
      }
    });

    // Await for new page to popup and store it in the global state manager
    const newPage = await newPagePromise as playwright.Page;
    globalStateManager.appendValue(name, newPage);
  }
);



When(
  /^I click the "([^"]*)" (?:button|link)$/,
  async function(this: ScenarioWorld, name: string, role: ValidAccessibilityRoles) {
    const {
      screen: { page },
    } = this;

    await waitFor(async() => {
      const locator = await page.getByRole(role, { name, exact: true });
      const result = await locator.isVisible();
      if (result) {
        await locator.click();
        return result;
      }
    });
  }
)
