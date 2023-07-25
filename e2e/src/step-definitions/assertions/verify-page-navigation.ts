import { Then } from '@cucumber/cucumber';
import { PageId } from '../../env/global';
import { ScenarioWorld } from '../setup/world'
import { waitFor } from '../../support/wait-for-behavior';
import {
  currentPathMatchesPageId,
} from '../../support/navigation-behavior';

Then(
  /^I should be on the "([^"]*)" page$/,
  async function(this: ScenarioWorld, pageId: PageId) {
    const {
      screen: { page },
      globalConfig,
    } = this;

  await waitFor(() => currentPathMatchesPageId(page, pageId, globalConfig))
});