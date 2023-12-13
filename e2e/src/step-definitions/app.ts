import { When } from "@cucumber/cucumber";
import { ScenarioWorld } from './world.js';
import { waitForTestIdVisible } from "../support/wait-for-behavior.js";

type ToastType = "error" | "success";

When(
  /^I should see (error|success) toast$/,
  async function(this: ScenarioWorld, toastType: ToastType) {
    const { page } = this.getScreen();
    await waitForTestIdVisible(page, "toast-" + toastType + "-msg", 600000);
  }
)

When(
  /^I should see (error|success) toast and close it$/,
  async function(this: ScenarioWorld, toastType: ToastType) {
    const { page } = this.getScreen();
    const toast = await waitForTestIdVisible(page, "toast-" + toastType + "-msg", 60000);
    const closeButton = toast.locator(".btn-close");
    await closeButton.click();
  }
)
