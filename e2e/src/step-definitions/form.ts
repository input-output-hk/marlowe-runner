import { When } from "@cucumber/cucumber";
import { ScenarioWorld } from './setup/world';
import { getElementLocator } from '../support/web-element-helper';
import { ElementKey } from '../env/global';
import { ValidAccessibilityRoles } from '../env/global';
import { waitFor } from "../support/wait-for-behavior";
import {
  inputValue,
} from '../support/html-behavior';

When(
  /^I fill in the "([^"]*)" input with "([^"]*)"$/,
  async function(this: ScenarioWorld, elementKey: ElementKey, input: string) {
    const {
      screen: { page },
      globalConfig
    } = this;

    const elementIdentifier = getElementLocator(page, elementKey, globalConfig);
    const { role, name } = elementIdentifier;

    await waitFor(async() => {
      const locator = await page.getByRole(role as ValidAccessibilityRoles, { name })
      const result = await locator.isVisible();

      if (result) {
        await inputValue(locator, input);
        return result;
      }
    });
  }
)

When(
  /^I unblur the "([^"]*)" input$/,
  async function(this: ScenarioWorld, elementKey: ElementKey) {
    const {
      screen: { page },
      globalConfig
    } = this;

    const elementIdentifier = getElementLocator(page, elementKey, globalConfig);
    const { role, name } = elementIdentifier;

    await waitFor(async() => {
      const locator = await page.getByRole(role as ValidAccessibilityRoles, { name })
      const result = await locator.isVisible();

      if (result) {
        await locator.blur();
        return result;
      }
    });
  }
)

When(
  /^I pause the page$/,
  async function(this: ScenarioWorld) {
    const {
      screen: { page },
    } = this;
    await page.pause();
  }
)

When(
  /^I reload the page$/,
  async function(this: ScenarioWorld) {
    const {
      screen: { page },
    } = this;
    await waitFor(async() => {
      await page.reload();
      return true;
    });
  }
)


When('I find the line in the {string} editor containing {string}',
  async function (this: ScenarioWorld, editorName: string, codeExample: string) {
    const {
      screen: { page },
    } = this;
    await waitFor(async() => {
        await page.getByRole('code').locator('div').filter({ hasText: codeExample }).nth(4).click();
        return true;
    });
});

When('I press {string} on the keyboard {string} times', async function (this: ScenarioWorld, keyName: string, numberOfTimes: string) {
  const {
    screen: { page },
  } = this;


  await waitFor(async() => {
    const times = parseInt(numberOfTimes, 10);
    for (let i = 0; i < times; i++) {
      await page.keyboard.press(keyName);
    }
    return true;
  });
});