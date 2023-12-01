import { Page } from 'playwright';
import { ScenarioWorld } from '../setup/world.js';
import { waitFor } from "../../support/wait-for-behavior.js";
import { inputValue } from '../../support/html-behavior.js';
import { Bech32 } from '../../cardano.js';

export const configure = async function(page: Page, mnemonic: string[], walletURL: string):Promise<Bech32> {
  await page.goto(`${walletURL}/app.html`);

  const buttonRestore = page.getByRole("button", { name: "Restore", exact: true });
  const buttonRestoreVisible = await buttonRestore.isVisible();

  const readAddress = async () => await waitFor(async (): Promise<Bech32> => {
    const button = page.locator('button', { hasText: "Receive"});
    await button.click();
    //data-testid="address-card-address"
    const addressContainer = page.getByTestId("address-card-address");
    const address:string = await addressContainer.innerText();
    try {
      return Bech32.fromString(address);
    } catch (error) {
      console.log(`Address ${address} is not a valid bech32 address`);
      throw error;
    }
  }, { label: "Wallet Address visible" });

  if(!buttonRestoreVisible) {
    return await readAddress();
  } else {
    await buttonRestore.click();
  }

  await waitFor(async() => {
    const buttonName = "OK"
    const locator = page.getByRole("button", { name: buttonName, exact: true });
    const result = await locator.isVisible();
    if (result) {
      await locator.click();
      return result;
    }
  }, { label: "OK button" });

  await waitFor(async() => {
    const buttonName = "I accept the Terms of Use"
    const locator = page.getByRole("checkbox", { name: buttonName, exact: true });
    const result = await locator.isVisible();
    if (result) {
      await locator.click();
      return result;
    }
  }, { label: "Terms of Use checkbox" });

  await waitFor(async() => {
    const buttonName = "Next"
    const locator = page.getByRole("button", { name: buttonName, exact: true });
    const result = await locator.isVisible();
    if (result) {
      await locator.click();
      return result;
    }
  }, { label: "Next button after Terms of Use" });

  await waitFor(async() => {
    const buttonName = "Agree"
    const locator = page.getByRole("button", { name: buttonName, exact: true });
    const result = await locator.isVisible();
    if (result) {
      await locator.click();
      return result;
    }
  }, { label: "Agree button" });

  await waitFor(async() => {
    const locator = page.getByTestId("wallet-setup-register-name-input")
    const result = await locator.isVisible();

    if (result) {
      await inputValue(locator, "Runner test");
      return result;
    }
  }, { label: "Name input" });

  await waitFor(async() => {
    const buttonName = "Next"
    const locator = page.getByRole("button", { name: buttonName, exact: true });
    const result = await locator.isVisible();
    if (result) {
      await locator.click();
      return result;
    }
  }, { label: "Next button after name input" });

  await waitFor(async() => {
    const locator = page.getByTestId("wallet-setup-password-step-password")
    const result = await locator.isVisible();

    if (result) {
      await inputValue(locator, "RunnerE2ETest");
      return result;
    }
  }, { label: "Password input" });

  await waitFor(async() => {
    const locator = page.getByTestId("wallet-setup-password-step-confirm-password")
    const result = await locator.isVisible();

    if (result) {
      await inputValue(locator, "RunnerE2ETest");
      return result;
    }
  }, { label: "Confirm password input" });

  await waitFor(async() => {
    const buttonName = "Next"
    const locator = page.getByRole("button", { name: buttonName, exact: true });
    const result = await locator.isVisible();
    if (result) {
      await locator.click();
      return result;
    }
  }, { label: "Next button after password input" });

  await waitFor(async() => {
    const buttonName = "Next"
    const locator = page.getByRole("button", { name: buttonName, exact: true });
    const result = await locator.isVisible();
    if (result) {
      await locator.click();
      return result;
    }
  }, { label: "Next button after password confirmation" });

  for (let i = 0; i < 8; i++) {
    await waitFor(async() => {
      const locator = page.locator(`#mnemonic-word-${i+1}`)
      const result = await locator.isVisible();

      if (result) {
        await inputValue(locator, mnemonic[i]);
        return result;
      }
    }, { label: `Mnemonic word ${i+1}` });
  }

  await waitFor(async() => {
    const buttonName = "Next"
    const locator = page.getByRole("button", { name: buttonName, exact: true });
    const result = await locator.isVisible();
    if (result) {
      await locator.click();
      return result;
    }
  }, { label: "Next button after first mnemonic section" });

  for (let i = 8; i < 16; i++) {
    await waitFor(async() => {
      const locator = page.locator(`#mnemonic-word-${i+1}`)
      const result = await locator.isVisible();

      if (result) {
        await inputValue(locator, mnemonic[i]);
        return result;
      }
    }, { label: `Mnemonic word ${i+1}` });
  }

  await waitFor(async() => {
    const buttonName = "Next"
    const locator = page.getByRole("button", { name: buttonName, exact: true });
    const result = await locator.isVisible();
    if (result) {
      await locator.click();
      return result;
    }
  }, { label: "Next button after second mnemonic section" });

  for (let i = 16; i < 24; i++) {
    await waitFor(async() => {
      const locator = page.locator(`#mnemonic-word-${i+1}`)
      const result = await locator.isVisible();

      if (result) {
        await inputValue(locator, mnemonic[i]);
        return result;
      }
    }, { label: `Mnemonic word ${i+1}` });
  }

  await waitFor(async() => {
    const buttonName = "Next"
    const locator = page.getByRole("button", { name: buttonName, exact: true });
    const result = await locator.isVisible();
    if (result) {
      await locator.click();
      return result;
    }
  }, { label: "Next button after third mnemonic section" });

  await waitFor(async() => {
    const buttonName = "Go to my wallet"
    const locator = page.getByRole("button", { name: buttonName, exact: true });
    const result = await locator.isVisible();
    if (result) {
      await locator.click();
      return result;
    }
  }, { label: "Go to my wallet button" });

  await waitFor(async() => {
    const buttonName = "Got it"
    const locator = page.getByRole("button", { name: buttonName, exact: true });
    const result = await locator.isVisible();
    if (result) {
      await locator.click();
      return result;
    }
  }, { label: '"Got it" button' });

  await waitFor(async() => {
    const locator = page.getByTestId("user-avatar")
    const result = await locator.isVisible();

    if (result) {
      await locator.click();
      return result;
    }
  }, { label: "User avatar" });

  await waitFor(async() => {
    const locator = page.getByTestId('header-menu').getByTestId('header-menu-network-choice-label')
    const result = await locator.isVisible();

    if (result) {
      await locator.click();
      return result;
    }
  }, { label: "Network choice label" });

  await waitFor(async() => {
    const locator = page.getByTestId('header-menu').getByTestId("network-preprod-radio-button")
    const result = await locator.isVisible();

    if (result) {
      await locator.click();
      return result;
    }
  }, { label: "Preprod network radio button" });

  // Let's confirm that the network was fully reloaded
  await waitFor(async() => {
    const networkPill = page.getByTestId('network-pill');
    const innerText = await networkPill.innerText();
    console.log("innerText");
    console.log(innerText);
    if (innerText == 'Preprod') {
      return true;
    }
  }, { label: "Preprod network pill" });

  return await readAddress();
};


export const authorizeApp = async function (page: Page, triggerAuthorization: () => Promise<void>, isAuthorizedCheck: (page: Page) => Promise<boolean>) {

  // const newPagePromise:Promise<Page> = new Promise(resolve => page.context().once('page', resolve));
  // const name = "lace";
  // await waitFor(async() => {
  //   const locator = page.getByRole("button", { name, exact: true });
  //   const result = await locator.isVisible();
  //   if (result) {
  //     await locator.click();
  //     return result;
  //   }
  // });

  const walletPopupPromise:Promise<Page> = new Promise(resolve => page.context().once('page', resolve));
  await triggerAuthorization();
  const grantAccess:Promise<void> = (async () => {
    const page = await walletPopupPromise;
    await page.reload();
    await waitFor(async ():Promise<boolean> => {
      const locator = page.getByRole("button", { name: "Authorize", exact: true });
      const result = await locator.isVisible();
      if (result) {
        await locator.click();
        return result;
      }
      return true
    }, { label: "Access button" });

    await waitFor(async() => {
      const locator = page.getByRole("button", { name: "Always", exact: true });
      const result = await locator.isVisible();
      if (result) {
        await locator.click();
        return result;
      }
    }, { label: "Always button" });
  })();

  await Promise.any([isAuthorizedCheck(page), grantAccess]);
  await isAuthorizedCheck(page);
}
