import playwright, { Page } from 'playwright';
import { When } from '@cucumber/cucumber';
import { ScenarioWorld } from '../setup/world.js';
import { waitFor } from "../../support/wait-for-behavior.js";
import { ValidAccessibilityRoles } from '../../env/global.js';
import * as fs from 'fs';
import {
  inputValue,
} from '../../support/html-behavior.js';

function sleep(seconds: number): Promise<void> {
  return new Promise(resolve => setTimeout(resolve, seconds * 1000));
}

When(
  /^I authorize my lace wallet$/,
  async function(this: ScenarioWorld) {
    const { page } = this.getScreen();
    const { globalStateManager } = this;

    const newPagePromise:Promise<Page> = new Promise(resolve => page.context().once('page', resolve));
    const name = "lace";
    await waitFor(async() => {
      const locator = page.getByRole("button", { name, exact: true });
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

    // sleep(30)

    await waitFor(async() => {
      const buttonName = "Authorize"
      const locator = newPage.getByRole("button", { name: buttonName, exact: true });
      const result = await locator.isVisible();
      if (result) {
        await locator.click();
        return result;
      }
    });

    await waitFor(async() => {
      const buttonName = "Always"
      const locator = newPage.getByRole("button", { name: buttonName, exact: true });
      const result = await locator.isVisible();
      if (result) {
        await locator.click();
        return result;
      }
    });

    await waitFor(async() => {
      const locator = page.getByTestId("wallet-info");
      const result = await locator.isVisible();
      if (result) {
        const address = await locator.getAttribute("data-wallet-address");
        globalStateManager.appendValue("wallet-address", address);
        return result;
      }
    });
  }
);

// When(
//   /^I configure my lace wallet$/,
//   async function(this: ScenarioWorld) {
//     const {
//       screen: { laceURL, page },
//     } = this;
// 
//     await page.goto(`${laceURL}/app.html`);
//     sleep(60)
// 
//     const mnemonic = fs.readFileSync('artifacts/lace-mnemonic.txt', 'utf-8');
//     const words = mnemonic.trim().split(' ');
//     await page.goto(`${laceURL}/app.html`);
// 
//     await waitFor(async() => {
//       const buttonName = "Restore"
//       const locator = page.getByRole("button", { name: buttonName, exact: true });
//       const result = await locator.isVisible();
//       if (result) {
//         await locator.click();
//         return result;
//       }
//     }, { label: "Restore button" });
// 
//     await waitFor(async() => {
//       const buttonName = "OK"
//       const locator = page.getByRole("button", { name: buttonName, exact: true });
//       const result = await locator.isVisible();
//       if (result) {
//         await locator.click();
//         return result;
//       }
//     }, { label: "OK button" });
// 
//     await waitFor(async() => {
//       const buttonName = "I accept the Terms of Use"
//       const locator = page.getByRole("checkbox", { name: buttonName, exact: true });
//       const result = await locator.isVisible();
//       if (result) {
//         await locator.click();
//         return result;
//       }
//     }, { label: "Terms of Use checkbox" });
// 
//     await waitFor(async() => {
//       const buttonName = "Next"
//       const locator = page.getByRole("button", { name: buttonName, exact: true });
//       const result = await locator.isVisible();
//       if (result) {
//         await locator.click();
//         return result;
//       }
//     }, { label: "Next button after Terms of Use" });
// 
//     await waitFor(async() => {
//       const buttonName = "Agree"
//       const locator = page.getByRole("button", { name: buttonName, exact: true });
//       const result = await locator.isVisible();
//       if (result) {
//         await locator.click();
//         return result;
//       }
//     }, { label: "Agree button" });
// 
//     await waitFor(async() => {
//       const locator = page.getByTestId("wallet-setup-register-name-input")
//       const result = await locator.isVisible();
// 
//       if (result) {
//         await inputValue(locator, "Runner test");
//         return result;
//       }
//     }, { label: "Name input" });
// 
//     await waitFor(async() => {
//       const buttonName = "Next"
//       const locator = page.getByRole("button", { name: buttonName, exact: true });
//       const result = await locator.isVisible();
//       if (result) {
//         await locator.click();
//         return result;
//       }
//     }, { label: "Next button after name input" });
// 
//     await waitFor(async() => {
//       const locator = page.getByTestId("wallet-setup-password-step-password")
//       const result = await locator.isVisible();
// 
//       if (result) {
//         await inputValue(locator, "RunnerE2ETest");
//         return result;
//       }
//     }, { label: "Password input" });
// 
//     await waitFor(async() => {
//       const locator = page.getByTestId("wallet-setup-password-step-confirm-password")
//       const result = await locator.isVisible();
// 
//       if (result) {
//         await inputValue(locator, "RunnerE2ETest");
//         return result;
//       }
//     }, { label: "Confirm password input" });
// 
//     await waitFor(async() => {
//       const buttonName = "Next"
//       const locator = page.getByRole("button", { name: buttonName, exact: true });
//       const result = await locator.isVisible();
//       if (result) {
//         await locator.click();
//         return result;
//       }
//     }, { label: "Next button after password input" });
// 
//     await waitFor(async() => {
//       const buttonName = "Next"
//       const locator = page.getByRole("button", { name: buttonName, exact: true });
//       const result = await locator.isVisible();
//       if (result) {
//         await locator.click();
//         return result;
//       }
//     }, { label: "Next button after password confirmation" });
// 
//     for (let i = 0; i < 8; i++) {
//       await waitFor(async() => {
//         const locator = page.locator(`#mnemonic-word-${i+1}`)
//         const result = await locator.isVisible();
// 
//         if (result) {
//           await inputValue(locator, words[i]);
//           return result;
//         }
//       }, { label: `Mnemonic word ${i+1}` });
//     }
// 
//     await waitFor(async() => {
//       const buttonName = "Next"
//       const locator = page.getByRole("button", { name: buttonName, exact: true });
//       const result = await locator.isVisible();
//       if (result) {
//         await locator.click();
//         return result;
//       }
//     }, { label: "Next button after first mnemonic section" });
// 
//     for (let i = 8; i < 16; i++) {
//       await waitFor(async() => {
//         const locator = page.locator(`#mnemonic-word-${i+1}`)
//         const result = await locator.isVisible();
// 
//         if (result) {
//           await inputValue(locator, words[i]);
//           return result;
//         }
//       }, { label: `Mnemonic word ${i+1}` });
//     }
// 
//     await waitFor(async() => {
//       const buttonName = "Next"
//       const locator = page.getByRole("button", { name: buttonName, exact: true });
//       const result = await locator.isVisible();
//       if (result) {
//         await locator.click();
//         return result;
//       }
//     }, { label: "Next button after second mnemonic section" });
// 
//     for (let i = 16; i < 24; i++) {
//       await waitFor(async() => {
//         const locator = page.locator(`#mnemonic-word-${i+1}`)
//         const result = await locator.isVisible();
// 
//         if (result) {
//           await inputValue(locator, words[i]);
//           return result;
//         }
//       }, { label: `Mnemonic word ${i+1}` });
//     }
// 
//     await waitFor(async() => {
//       const buttonName = "Next"
//       const locator = page.getByRole("button", { name: buttonName, exact: true });
//       const result = await locator.isVisible();
//       if (result) {
//         await locator.click();
//         return result;
//       }
//     }, { label: "Next button after third mnemonic section" });
// 
//     await waitFor(async() => {
//       const buttonName = "Go to my wallet"
//       const locator = page.getByRole("button", { name: buttonName, exact: true });
//       const result = await locator.isVisible();
//       if (result) {
//         await locator.click();
//         return result;
//       }
//     }, { label: "Go to my wallet button" });
// 
//     await waitFor(async() => {
//       const buttonName = "Got it"
//       const locator = page.getByRole("button", { name: buttonName, exact: true });
//       const result = await locator.isVisible();
//       if (result) {
//         await locator.click();
//         return result;
//       }
//     }, { label: '"Got it" button' });
// 
//     await waitFor(async() => {
//       const locator = page.getByTestId("user-avatar")
//       const result = await locator.isVisible();
// 
//       if (result) {
//         await locator.click();
//         return result;
//       }
//     }, { label: "User avatar" });
// 
//     await waitFor(async() => {
//       const locator = page.getByTestId('header-menu').getByTestId('header-menu-network-choice-label')
//       const result = await locator.isVisible();
// 
//       if (result) {
//         await locator.click();
//         return result;
//       }
//     }, { label: "Network choice label" });
// 
//     await waitFor(async() => {
//       const locator = page.getByTestId('header-menu').getByTestId("network-preprod-radio-button")
//       const result = await locator.isVisible();
// 
//       if (result) {
//         await locator.click();
//         return result;
//       }
//     }, { label: "Preprod network radio button" });
// 
//     // Let's confirm that the network was fully reloaded
//     await waitFor(async() => {
//       const networkPill = page.getByTestId('network-pill');
//       const innerText = await networkPill.innerText();
//       console.log("innerText");
//       console.log(innerText);
//       if (innerText == 'Preprod') {
//         return true;
//       }
//     }, { label: "Preprod network pill" });
// });
// 
// 
// When(
//   /^I click the "([^"]*)" with "([^"]*)" text And sign the transaction with lace wallet$/,
//   async function(this: ScenarioWorld, role: ValidAccessibilityRoles,  name: string) {
//     const {
//       screen: { page }
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
//       const buttonName = "Confirm"
//       const locator = newPage.getByRole("button", { name: buttonName, exact: true });
//       const result = await locator.isVisible();
//       if (result) {
//         await locator.click();
//         return result;
//       }
//     });
// 
//     await waitFor(async() => {
//       const locator = newPage.getByTestId("password-input")
//       const result = await locator.isVisible();
// 
//       if (result) {
//         const password = process.env.LACE_WALLET_PASSWORD as string
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
// 
//     await waitFor(async() => {
//       const buttonName = "Close"
//       const locator = newPage.getByRole("button", { name: buttonName, exact: true });
//       const result = await locator.isVisible();
//       if (result) {
//         await locator.click();
//         return result;
//       }
//     });
//   }
// );
