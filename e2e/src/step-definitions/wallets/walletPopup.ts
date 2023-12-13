import { Page } from 'playwright';
import { grabPopup } from '../popup.js';

// TODO: move the module level api which we have
// here and turn this into an interface.
export class WalletPopup {
  private page: Page;

  constructor(page: Page) {
    this.page = page;
  }

  public getPage(): Page | undefined {
    if(!this.page.isClosed()) {
      return this.page;
    }
  }

  public static async fromTrigger(page: Page, triggerPopup: () => Promise<void>): Promise<WalletPopup> {
    const walletPopup:Page = await grabPopup(page, triggerPopup);
    await walletPopup.reload();
    return new WalletPopup(walletPopup);
  }
}


