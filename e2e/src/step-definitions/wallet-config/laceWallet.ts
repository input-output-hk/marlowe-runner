import playwright from 'playwright';
import { When } from '@cucumber/cucumber';
import { ScenarioWorld } from '../setup/world.js';
import { waitFor } from "../../support/wait-for-behavior.js";
import { testWallet } from "../../support/walletConfiguration.js";

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
