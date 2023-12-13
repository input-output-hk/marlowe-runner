import { Page } from "playwright";

export const grabPopup = async function (page: Page, triggerPopup: () => Promise<void>): Promise<Page> {
  const popupPromise:Promise<Page> = new Promise(resolve => page.context().once('page', resolve));
  await triggerPopup();
  return await popupPromise;
}
