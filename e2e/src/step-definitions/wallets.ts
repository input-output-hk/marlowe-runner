import { When } from '@cucumber/cucumber';
import { ScenarioWorld, WalletType } from './setup/world.js';
import { waitFor } from "../support/wait-for-behavior.js";
import * as nami from "./wallets/nami.js";
import * as lace from "./wallets/lace.js";
import { Page } from 'playwright';

When(
  /^I use ([^ ]*) (nami|lace) browser$/,
  async function(this: ScenarioWorld, walletName: string, walletType: string) {
    const { screens } = this;
    switch (walletType) {
      case 'nami':
        this.screen = await screens('nami', walletName);
        break;
      case 'lace':
        this.screen = await screens('lace', walletName);
        break;
      default:
        throw new Error('Unknown wallet type');
    }
});

const mkTriggerAuthorization = (page: Page, walletType: WalletType) => async () => {
  let buttonName:string;
  switch (walletType) {
    case 'nami':
      buttonName = "Nami";
      break;
    case 'lace':
      buttonName = "lace";
      break;
    default:
      throw new Error('Unknown wallet type');
  }

  await waitFor(async() => {
    const locator = page.getByRole("button", { name: buttonName, exact: true });
    const result = await locator.isVisible();
    if (result) {
      await locator.click();
      return result;
    }
  }, { label: `${buttonName} button` });
}



When(
  /^I authorize the wallet$/,
  async function(this: ScenarioWorld) {
    const { page, wallet } = this.getScreen();
    const isAuthorizedCheck = (page: Page) => {
      return page.getByTestId("wallet-info").isVisible()
    };

    const triggerAuthorization = mkTriggerAuthorization(page, wallet.type);
    switch (wallet.type) {
      case 'nami':
        await nami.authorizeApp(page, triggerAuthorization, isAuthorizedCheck);
        break;
      case 'lace':
        await lace.authorizeApp(page, triggerAuthorization, isAuthorizedCheck);
        break;
      default:
        throw new Error('Unknown wallet type');
    }


    // await nami.authorizeApp(page, (page) => { return page.getByTestId("wallet-info").isVisible() });

    // await waitFor(async() => {
    //   const locator = page.getByTestId("wallet-info");
    //   const result = await locator.isVisible();
    //   if (result) {
    //     const address = await locator.getAttribute("data-wallet-address");
    //     globalStateManager.appendValue("wallet-address", address);
    //     return result;
    //   }
    // }, { label: "Wallet Address visible" });
  }
);
