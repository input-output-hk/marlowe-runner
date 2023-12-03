import { Locator, Page } from "playwright";

export type WaitForOptions = {
  timeout?: number;
  wait?: number;
  label?: string;
  onTimeout?: () => any;
};

export const waitFor = async <T>(
  predicate: () => T | Promise<T> | undefined,
  options?: WaitForOptions
): Promise<T> => {
  const { timeout = 60000, wait=200 } = options || {};
  const sleep = (ms: number) => new Promise(resolve => setTimeout(resolve, ms));
  const startDate = new Date();
  const onTimeout = options?.onTimeout || (() => {
    throw new Error(`Wait time of ${timeout}ms exceeded`)
  });

  while (new Date().getTime() - startDate.getTime() < timeout) {
    const result = await predicate();
    if (result) {
      return result;
    }

    await sleep(wait)
    if(!options?.label) {
      console.log(`Waiting ${wait}ms`);
    } else {
      console.log(`Waiting ${wait}ms for ${options.label}`);
    }
  }
  return onTimeout();
}

type Role = "alert"|"alertdialog"|"application"|"article"|"banner"|"blockquote"|"button"|"caption"|"cell"|"checkbox"|"code"|"columnheader"|"combobox"|"complementary"|"contentinfo"|"definition"|"deletion"|"dialog"|"directory"|"document"|"emphasis"|"feed"|"figure"|"form"|"generic"|"grid"|"gridcell"|"group"|"heading"|"img"|"insertion"|"link"|"list"|"listbox"|"listitem"|"log"|"main"|"marquee"|"math"|"meter"|"menu"|"menubar"|"menuitem"|"menuitemcheckbox"|"menuitemradio"|"navigation"|"none"|"note"|"option"|"paragraph"|"presentation"|"progressbar"|"radio"|"radiogroup"|"region"|"row"|"rowgroup"|"rowheader"|"scrollbar"|"search"|"searchbox"|"separator"|"slider"|"spinbutton"|"status"|"strong"|"subscript"|"superscript"|"switch"|"tab"|"table"|"tablist"|"tabpanel"|"term"|"textbox"|"time"|"timer"|"toolbar"|"tooltip"|"tree"|"treegrid"|"treeitem";

export const waitForSelectorVisible = async (
  page: Page,
  selector: string,
  timeout?: number,
): Promise<Locator> => {
    const locator = page.locator(selector);
    console.log(`Waiting for ${selector} to be visible`);
    await locator.waitFor({state: "visible", timeout: timeout!==undefined?timeout:10000});
    return locator;
}

export const waitForRoleVisible = async (
  page: Page,
  role: Role,
  name?: string,
  timeout?: number,
): Promise<Locator> => {
    const locator = page.getByRole(role, { name, exact: true });
    console.log(`Waiting for ${role} with ${name} text to be visible`);
    await locator.waitFor({state: "visible", timeout: timeout!==undefined?timeout:10000});
    return locator;
}

export const waitForTestIdVisible = async (
  page: Page,
  testId: string,
  timeout?: number,
): Promise<Locator> => {
    const locator = page.getByTestId(testId);
    console.log(`Waiting for ${testId} to be visible`);
    await locator.waitFor({state: "visible", timeout: timeout!==undefined?timeout:10000});
    return locator;
}
