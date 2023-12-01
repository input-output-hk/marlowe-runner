import playwright, { Page } from 'playwright';
import { waitFor } from "../../support/wait-for-behavior.js";
import { inputValue } from '../../support/html-behavior.js';
import { sleep } from '../../promise.js';
import { Bech32 } from '../../cardano.js';

// Configure a nami wallet spcified by the mnemonic if it is not already configured.
export const configure = async function (page: Page, mnemonic: string[], walletURL: string): Promise<Bech32> {
  await page.goto(`${walletURL}/mainPopup.html`);

  const readAddress = async () => await waitFor(async (): Promise<Bech32> => {
    const button = page.locator('button', { hasText: "Receive"});
    await button.click();
    const addressContainer = page.locator('.chakra-text', { hasText: /addr.*/ });
    const address:string = await addressContainer.innerText();
    try {
      return Bech32.fromString(address);
    } catch (error) {
      console.log(`Address ${address} is not a valid bech32 address`);
      throw error;
    }
  }, { label: "Wallet Address visible" });

  // Check if we are already logged in
  var buttonNew = page.getByRole("button", { name: "New Wallet", exact: true });
  var buttonNewVisible = await buttonNew.isVisible();
  if(!buttonNewVisible) {
    return await readAddress();
  }

  await page.goto(`${walletURL}/createWalletTab.html?type=import&length=24`);

  for (let i = 0; i < 24; i++) {
    const name = `Word ${i+1}`;
    await waitFor(async () => {
      const locator = page.getByRole("textbox", { name: name, exact: true });
        const result = await locator.isVisible();

        if (result) {
          await inputValue(locator, mnemonic[i]);
          return result;
        }
      }, { label: "Word " + i });
  }

  await waitFor(async () => {
    const buttonName = "Next"
    const locator = page.getByRole("button", { name: buttonName, exact: true });
    const result = await locator.isVisible();
    if (result) {
      await locator.click();
      return result;
    }
  }, { label: "Next 1 Button" });

  await waitFor(async () => {
    const name = "Enter account name";
    const locator = page.getByRole("textbox", { name: name, exact: true });
    const result = await locator.isVisible();

    if (result) {
      await inputValue(locator, "Runner test");
      return result;
    }
  }, { label: "Enter account name" });

  await waitFor(async () => {
    const name = "Enter password";
    const locator = page.getByRole("textbox", { name: name, exact: true });
    const result = await locator.isVisible();

    if (result) {
      await inputValue(locator, "Runner test");
      return result;
    }
  }, { label: "Enter password" });

  await waitFor(async () => {
    const name = "Confirm password";
    const locator = page.getByRole("textbox", { name: name, exact: true });
    const result = await locator.isVisible();

    if (result) {
      await inputValue(locator, "Runner test");
      return result;
    }
  }, { label: "Confirm password" });

  await waitFor(async () => {
    const buttonName = "Create"
    const locator = page.getByRole("button", { name: buttonName, exact: true });
    const result = await locator.isVisible();
    if (result) {
      await locator.click();
      return result;
    }
  }), { label: "Create Button" };

  await waitFor(async () => {
    const buttonName = "Close"
    const locator = page.getByRole("button", { name: buttonName, exact: true });
    const result = await locator.isVisible();
    if (result) {
      return result;
    }
  }, { label: "Close Button" });

  await page.goto(`${walletURL}/mainPopup.html`);

  const clickEmptyButton = async () => waitFor(async () => {
    const buttonName = ""
    const locator = page.getByRole("button", { name: buttonName, exact: true });
    const result = await locator.isVisible();
    if (result) {
      await locator.click();
      return result;
    }
  });

  await clickEmptyButton();

  // await page.reload();

  await waitFor(async () => {
    const buttonName = "Settings"
    const locator = page.getByRole("menuitem", { name: buttonName, exact: true });
    const result = await locator.isVisible();
    if (result) {
      await locator.click();
      return result;
    }
  }, { label: "Settings Button" });

  await waitFor(async () => {
    const buttonName = "Network"
    const locator = page.getByRole("button", { name: buttonName, exact: true });
    const result = await locator.isVisible();
    if (result) {
      await locator.click();
      return result;
    }
  }, { label: "Network Button" });

  await waitFor(async () => {
    const network = "Preprod"
    const locator = page.getByRole("combobox");
    const result = await locator.isVisible();
    if (result) {
      await locator.selectOption(network);
      return result;
    }
  }, { label: "Preprod network" });

  await waitFor(async () => {
    const locator = page.locator(".chakra-text").last();
    const innerText = await locator.innerText();
    return (innerText === "Preprod");
  }, { label: "Preprod network text" });

  // Nami is BUGGY. Jumping directly or indirectly to subpages
  // is error prone. We need to do it step by step by slowly clicking.
  // await page.goto(`${walletURL}/wallet`); // this doesn't work

  await sleep(1);
  // back button in this case
  await clickEmptyButton();

  // back button in this case
  await sleep(1);
  await clickEmptyButton();

  return await readAddress();
};

// After action to connect wallet we can have redirect to a wallet page or we can jump directly
// to the app internals.
// This function helps to handle both cases by accepting a callback that checks if we are already
// authorized.
// It should be called after connect action.
export const authorizeApp = async function (page: Page, triggerAuthorization: () => Promise<void>, isAuthorizedCheck: (page: Page) => Promise<boolean>) {
  const walletPopupPromise:Promise<Page> = new Promise(resolve => page.context().once('page', resolve));
  await triggerAuthorization();
  const grantAccess:Promise<boolean> = (async () => {
    const page = await walletPopupPromise;
    await page.reload();
    return await waitFor(async ():Promise<boolean> => {
      const accessButton = page.getByText("Access")
      const accessButtonVisible = await accessButton.isVisible();
      if (accessButtonVisible) {
        await accessButton.click();
        return true;
      }
      return false;
    }, { label: "Access button" });
  })();

  const isAuthorized: () => Promise<boolean> = async () => waitFor(async () => {
    const result = await isAuthorizedCheck(page);
    return result;
  }, { label: "App authorized" });

  await Promise.any([isAuthorized(), grantAccess]);
  await isAuthorized();
}

// await waitFor(async () => {
//   const locator = page.getByTestId("wallet-info");
//   const result = await locator.isVisible();
//   if (result) {
//     const address = await locator.getAttribute("data-wallet-address");
//     globalStateManager.appendValue("wallet-address", address);
//     return result;
//   }
// }, { label: "Wallet Address visible" });
