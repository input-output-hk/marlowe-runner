import { When } from '@cucumber/cucumber';
import { ScenarioWorld, WalletType } from './world.js';
import { AccessibilityRole, waitForRoleVisible } from "../support/wait-for-behavior.js";
import * as nami from "./wallets/nami.js";
import * as lace from "./wallets/lace.js";
import { Page } from 'playwright';
import { WalletPopup } from './wallets/walletPopup.js';

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
  const locator = await waitForRoleVisible(page, "button", buttonName);
  await locator.click();
}

When(
  /^I authorize the app$/,
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
  }
);

When(
  /^I click the "([^"]*)" with "([^"]*)" text And sign the transaction$/,
  async function(this: ScenarioWorld, role: AccessibilityRole,  name: string) {
    const { page, wallet } = this.getScreen();
    const triggerSign = async () => {
      const locator = await waitForRoleVisible(page, role, name);
      await locator.click();
    }
    const walletPopup = await WalletPopup.fromTrigger(page, triggerSign);
    switch (wallet.type) {
      case 'nami':
        await nami.signTx(walletPopup);
        break;
      case 'lace':
        await lace.signTx(walletPopup);
        break;
      default:
        throw new Error('Unknown wallet type');
    }
  }
);

When(
  /^I click the "([^"]*)" with "([^"]*)" text And grab wallet popup$/,
  async function(this: ScenarioWorld, role: AccessibilityRole,  name: string) {
    const { page } = this.getScreen();
    const triggerSign = async () => {
      const locator = await waitForRoleVisible(page, role, name);
      await locator.click();
    }
    const walletPopup = await WalletPopup.fromTrigger(page, triggerSign);
    this.setWalletPopup(walletPopup);
  }
);

When(
  /^I switch to "([^ ]*)" (nami|lace) browser and finally sign the transaction$/,
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
    const walletPopup:WalletPopup|undefined = this.getScreen().walletPopup;
    if(walletPopup === undefined) {
      throw new Error("Wallet popup was probably already closed");
    }

    switch (walletType) {
      case 'nami':
        await nami.signTx(walletPopup);
        break;
      case 'lace':
        await lace.signTx(walletPopup);
        break;
      default:
        throw new Error('Unknown wallet type');
    }
});
