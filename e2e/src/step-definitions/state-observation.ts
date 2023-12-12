import { When } from '@cucumber/cucumber';
import { ScenarioWorld } from './world.js';
import { waitFor } from '../support/wait-for-behavior.js';
import moment from 'moment';

When(/^I observe the "([^"]*)" time$/,
async function (this: ScenarioWorld, timeLabel: string) {
    const { page } = this.getScreen();
    const {
      globalConfig: { simulatorDateFormat },
      globalStateManager
    } = this;

    await waitFor(async() => {
      const name = `${timeLabel} time`;
      const locator = page.getByRole("heading", { name, exact: true });
      const dateString = await locator.textContent();
      if (dateString) {
        const originalTime = moment(dateString, simulatorDateFormat).toDate();
        globalStateManager.appendValue(name, originalTime);
        return true;
      }

      return false;
    });
});
