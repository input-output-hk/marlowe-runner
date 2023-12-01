import playwright, { Page } from 'playwright';
import { When } from '@cucumber/cucumber';
import { ScenarioWorld } from '../setup/world.js';
import { waitFor } from "../../support/wait-for-behavior.js";
import { inputValue } from '../../support/html-behavior.js';
import * as fs from 'fs';
import { ValidAccessibilityRoles } from '../../env/global.js';

When(
  /^I configure the wallet$/,
  async function(this: ScenarioWorld, walletName: string) {
    const { screens } = this;

    const mnemonic = fs.readFileSync('artifacts/mnemonics/' + walletName, 'utf-8');
    const screen = await screens("nami", walletName);
    const { page, wallet } = screen;
    this.screen = screen;
    const words = mnemonic.trim().split(' ');

    await page.goto(`${wallet.url}/mainPopup.html`);

    // Check if we are already logged in
    var buttonNew = page.getByRole("button", { name: "New Wallet", exact: true });
    var buttonNewVisible = await buttonNew.isVisible();
    if(!buttonNewVisible) {
      return true;
    }

    await page.goto(`${wallet.url}/createWalletTab.html?type=import&length=24`);

    for (let i = 0; i < 24; i++) {
      const name = `Word ${i+1}`;
      await waitFor(async() => {
        const locator = page.getByRole("textbox", { name: name, exact: true });
          const result = await locator.isVisible();

          if (result) {
            await inputValue(locator, words[i]);
            return result;
          }
        }, { label: "Word " + i });
    }

    await waitFor(async() => {
      const buttonName = "Next"
      const locator = page.getByRole("button", { name: buttonName, exact: true });
      const result = await locator.isVisible();
      if (result) {
        await locator.click();
        return result;
      }
    }, { label: "Next 1 Button" });

    await waitFor(async() => {
      const name = "Enter account name";
      const locator = page.getByRole("textbox", { name: name, exact: true });
      const result = await locator.isVisible();

      if (result) {
        await inputValue(locator, "Runner test");
        return result;
      }
    }, { label: "Enter account name" });

    await waitFor(async() => {
      const name = "Enter password";
      const locator = page.getByRole("textbox", { name: name, exact: true });
      const result = await locator.isVisible();

      if (result) {
        await inputValue(locator, "Runner test");
        return result;
      }
    }, { label: "Enter password" });

    await waitFor(async() => {
      const name = "Confirm password";
      const locator = page.getByRole("textbox", { name: name, exact: true });
      const result = await locator.isVisible();

      if (result) {
        await inputValue(locator, "Runner test");
        return result;
      }
    }, { label: "Confirm password" });

    await waitFor(async() => {
      const buttonName = "Create"
      const locator = page.getByRole("button", { name: buttonName, exact: true });
      const result = await locator.isVisible();
      if (result) {
        await locator.click();
        return result;
      }
    }), { label: "Create Button" };

    await waitFor(async() => {
      const buttonName = "Close"
      const locator = page.getByRole("button", { name: buttonName, exact: true });
      const result = await locator.isVisible();
      if (result) {
        return result;
      }
    }, { label: "Close Button" });

    await page.goto(`${wallet.url}/mainPopup.html`);

    await waitFor(async() => {
      const buttonName = ""
      const locator = page.getByRole("button", { name: buttonName, exact: true });
      const result = await locator.isVisible();
      if (result) {
        await locator.click();
        return result;
      }
    });

    await waitFor(async() => {
      const buttonName = "Settings"
      const locator = page.getByRole("menuitem", { name: buttonName, exact: true });
      const result = await locator.isVisible();
      if (result) {
        await locator.click();
        return result;
      }
    });

    await waitFor(async() => {
      const buttonName = "Network"
      const locator = page.getByRole("button", { name: buttonName, exact: true });
      const result = await locator.isVisible();
      if (result) {
        await locator.click();
        return result;
      }
    });

    await waitFor(async() => {
      const network = "Preprod"
      const locator = page.getByRole("combobox");
      const result = await locator.isVisible();
      if (result) {
        await locator.selectOption(network);
        return result;
      }
    });
});



// When(
//   /^I authorize the wallet$/,
//   async function(this: ScenarioWorld) {
//     const {
//       screen: { page },
//       globalStateManager
//     } = this;
// 
//     const name = "Nami";
//     await waitFor(async() => {
//       const locator = page.getByRole("button", { name, exact: true });
//       const result = await locator.isVisible();
//       if (result) {
//         await locator.click();
//         return result;
//       }
//     }, { label: "Nami Button" });
// 
//     await waitFor(async() => {
//       const accessButton = page.getByText("Access")
//       const walletInfo = page.getByTestId("wallet-info");
//       const accessButtonVisible = await accessButton.isVisible();
//       const walletInfoVisible = await walletInfo.isVisible();
//       if (accessButtonVisible || walletInfoVisible) {
//         if(accessButtonVisible) {
//           await accessButton.click();
//         }
//         return true;
//       }
//     }, { label: "Access Button" });
// 
//     await waitFor(async() => {
//       const locator = page.getByTestId("wallet-info");
//       const result = await locator.isVisible();
//       if (result) {
//         const address = await locator.getAttribute("data-wallet-address");
//         globalStateManager.appendValue("wallet-address", address);
//         return result;
//       }
//     }, { label: "Wallet Address visible" });
//   }
// );
// 
// 
// When(
//   /^I click the "([^"]*)" with "([^"]*)" text And sign the transaction with nami wallet$/,
//   async function(this: ScenarioWorld, role: ValidAccessibilityRoles,  name: string) {
//     const {
//       screen: { page },
//     } = this;
// 
//     const newPagePromise:Promise<Page> = new Promise(resolve => page.context().once('page', resolve));
// 
//     await waitFor(async() => {
//       const locator = page.getByRole(role, { name, exact: true });
//       const result = await locator.isVisible();
//       if (result) {
//         await locator.click();
//         return result;
//       }
//     });
// 
//     // Await for new page to popup
//     const newPage = await newPagePromise as playwright.Page;
// 
//     await waitFor(async() => {
//       await newPage.reload();
//       return true;
//     });
// 
//     await waitFor(async() => {
//       const buttonName = "Sign"
//       const locator = newPage.getByRole("button", { name: buttonName, exact: true });
//       const result = await locator.isVisible();
//       if (result) {
//         await locator.click();
//         return result;
//       }
//     });
// 
//     await waitFor(async() => {
//       const locator = newPage.getByRole("textbox", { name: "Enter password", exact: true });
//       const result = await locator.isVisible();
// 
//       if (result) {
//         const password = process.env.NAMI_WALLET_PASSWORD as string
//         await inputValue(locator, password);
//         return result;
//       }
//     });
// 
//     await waitFor(async() => {
//       const buttonName = "Confirm"
//       const locator = newPage.getByRole("button", { name: buttonName, exact: true });
//       const result = await locator.isVisible();
//       if (result) {
//         await locator.click();
//         return result;
//       }
//     });
//   }
// );
