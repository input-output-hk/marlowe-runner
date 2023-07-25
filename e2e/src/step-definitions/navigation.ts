import playwright from 'playwright';
import { Given, Then } from '@cucumber/cucumber';
import { PageId } from '../env/global';
import {
  navigateToPage,
  currentPathMatchesPageId,
} from '../support/navigation-behavior';
import { ScenarioWorld } from './setup/world';
import { waitFor } from '../support/wait-for-behavior'

Given(
  /^I am on the "([^"]*)" page$/,
  async function(this: ScenarioWorld, pageId: PageId) {
    // Anything we pull off from `this` variable is defined in cucumber world
    const {
      screen: { page },
      globalConfig,
    } = this;
    console.log(`I am on the ${pageId} page application`);

    await navigateToPage(page, pageId, globalConfig);

    await waitFor(() => currentPathMatchesPageId(page, pageId, globalConfig))
  }
);

Then('a new browser tab should open for {string} at {string} url', async function (this: ScenarioWorld, name: string, expectedUrl: string) {
    const { globalStateManager } = this;
  const newPage: playwright.Page = globalStateManager.getValue(name);

  await waitFor(async() => {
    // Wait for the new page to load
    await newPage.waitForLoadState();

    // Get the URL of the new page
    const actualUrl = newPage.url();


    // Close the new page
    await newPage.close();
    // Check the URL is the expected URL
    return actualUrl.includes(expectedUrl);
  });
});