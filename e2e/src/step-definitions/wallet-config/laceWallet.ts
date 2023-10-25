import playwright from 'playwright';
import { When } from '@cucumber/cucumber';
import { ScenarioWorld } from '../setup/world.js';
import { waitFor } from "../../support/wait-for-behavior.js";
import { testWallet } from "../../support/walletConfiguration.js";
import { ValidAccessibilityRoles } from '../../env/global.js';
import {
  inputValue,
} from '../../support/html-behavior.js';

function sleep(seconds: number): Promise<void> {
  return new Promise(resolve => setTimeout(resolve, seconds * 1000));
}

When(
  /^I authorize my lace wallet$/,
  async function(this: ScenarioWorld) {
    const {
      screen: { page },
      globalStateManager
    } = this;

    let newPagePromise;

    newPagePromise = new Promise(resolve => page.context().once('page', resolve));

    const name = "lace";
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

    sleep(30)

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

    await waitFor(async() => {
      const locator = await page.getByTestId("wallet-info");
      const result = await locator.isVisible();
      if (result) {
        const address = await locator.getAttribute("data-wallet-address");
        globalStateManager.appendValue("wallet-address", address);
        return result;
      }
    });
  }
);

When(
  /^I configure my lace wallet$/,
  async function(this: ScenarioWorld) {
    const {
      screen: { page },
      globalStateManager
    } = this;


    const EXTENSION_URL = 'chrome-extension://gafhhkghbfjjkeiendhlofajokpaflmk';

    await page.goto(`${EXTENSION_URL}/app.html`);
    await page.addInitScript((testWallet) => {
      window.localStorage.setItem('lock', testWallet.lock);
      window.localStorage.setItem('analyticsAccepted', testWallet.analyticsAccepted);
      window.localStorage.setItem('showDappBetaModal', testWallet.showDappBetaModal);
      window.localStorage.setItem('wallet', testWallet.wallet);
      const keyAgentData = JSON.parse(testWallet?.backgroundStorage?.keyAgentsByChain);
      const mnemonicData = JSON.parse(testWallet?.backgroundStorage?.mnemonic);
      const backgroundStorage = {
        mnemonic: mnemonicData,
        keyAgentsByChain: keyAgentData,
        MIGRATION_STATE: { state: 'up-to-date' }
      };
      window.localStorage.setItem('BACKGROUND_STORAGE', JSON.stringify(backgroundStorage));
      window.localStorage.setItem('appSettings', testWallet.appSettings);
      window.localStorage.setItem('keyAgentData', testWallet.keyAgentData);
    }, testWallet);
    await page.goto(`${EXTENSION_URL}/app.html`);
    await page.waitForTimeout(5000);
    await page.reload();
});


When(
  /^I click the "([^"]*)" with "([^"]*)" text And sign the transaction with lace wallet$/,
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