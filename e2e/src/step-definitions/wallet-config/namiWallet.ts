import playwright from 'playwright';
import { When } from '@cucumber/cucumber';
import { ScenarioWorld } from '../setup/world.js';
import { waitFor } from "../../support/wait-for-behavior.js";
import { testWallet } from "../../support/walletConfiguration.js";
import { inputValue } from '../../support/html-behavior.js';
import * as fs from 'fs';

function sleep(seconds: number): Promise<void> {
  return new Promise(resolve => setTimeout(resolve, seconds * 1000));
}

When(
  /^I authorize my nami wallet$/,
  async function(this: ScenarioWorld) {
    const {
      screen: { page },
      globalStateManager
    } = this;

    let newPagePromise;

    newPagePromise = new Promise(resolve => page.context().once('page', resolve));

    const name = "nami";
    await waitFor(async() => {
      const locator = await page.getByRole("button", { name, exact: true });
      const result = await locator.isVisible();
      if (result) {
        await locator.click();
        return result;
      }
    });

    const newPage = await newPagePromise as playwright.Page;

    await waitFor(async() => {
      await newPage.reload();
      return true;
    });

    await sleep(200)
    await waitFor(async() => {
      const buttonName = "Authorize"
      const locator = await newPage.getByRole("button", { name: buttonName, exact: true });
      const result = await locator.isVisible();
      if (result) {
        await locator.click();
        return result;
      }
    });

    await waitFor(async() => {
      const buttonName = "Always"
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
  /^I configure my nami wallet$/,
  async function(this: ScenarioWorld) {
    const {
      screen: { page },
      globalStateManager
    } = this;

    const mnemonic = fs.readFileSync('artifacts/mnemonic.txt', 'utf-8');
    const words = mnemonic.trim().split(' ');

    const EXTENSION_URL = 'chrome-extension://aopgffgkiplghkmanlffiacfjokdigob';
    const newPage = await page.context().newPage();
    await newPage.goto(`${EXTENSION_URL}/createWalletTab.html?type=import&length=24`);

    const inputField = async (name, value) => {
      const locator = await newPage.getByRole("textbox", { name: name, exact: true });
      const result = await locator.isVisible();
      if (result) {
        await inputValue(locator, value);
        return result;
      }
    }

    for (let i = 0; i < 24; i++) {
      await inputField(`Word ${i+1}`, words[i]);
    }

    await waitFor(async() => {
      const buttonName = "Next"
      const locator = await newPage.getByRole("button", { name: buttonName, exact: true });
      const result = await locator.isVisible();
      if (result) {
        await locator.click();
        return result;
      }
    });

    await inputField("Enter account name", "Runner test");
    await inputField("Enter password", "Runner test");
    await inputField("Confirm password", "Runner test");

    await waitFor(async() => {
      const buttonName = "Create"
      const locator = await newPage.getByRole("button", { name: buttonName, exact: true });
      const result = await locator.isVisible();
      if (result) {
        await locator.click();
        return result;
      }
    });

    await newPage.waitForTimeout(2000);

    await waitFor(async() => {
      const buttonName = "Close"
      const locator = await newPage.getByRole("button", { name: buttonName, exact: true });
      const result = await locator.isVisible();
      if (result) {
        await locator.click();
        return result;
      }
    });

    await page.reload();
});
