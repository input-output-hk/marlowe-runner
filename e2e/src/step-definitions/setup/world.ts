
import playwright, {
  BrowserContextOptions,
  Page,
  Browser,
  BrowserContext,
  BrowserType
} from 'playwright';
import { env } from '../../env/parseEnv'
import { World, IWorldOptions, setWorldConstructor } from "@cucumber/cucumber";
import { GlobalConfig } from '../../env/global';
import GlobalStateManager from "../../support/globalStateManager";
import { testWallet } from "../../support/walletConfiguration";

export type Screen = {
  context: BrowserContext;
  page: Page;
}

export class ScenarioWorld extends World {
  constructor(options: IWorldOptions) {
    super(options)

    this.globalConfig = options.parameters as GlobalConfig;
  }

  globalConfig: GlobalConfig;

  screen!: Screen;
  globalStateManager = new GlobalStateManager();

  async init(contextOptions?: BrowserContextOptions): Promise<Screen> {
    await this.screen?.page?.close();
    await this.screen?.context?.close();

    const context = await this.newContext();
    const page = await context.newPage();

    const EXTENSION_URL = 'chrome-extension://gafhhkghbfjjkeiendhlofajokpaflmk';

    await page.goto(`${EXTENSION_URL}/app.html`);
    await page.addInitScript((testWallet) => {
      window.localStorage.setItem('lock', testWallet.lock);
      window.localStorage.setItem('analyticsAccepted', testWallet.analyticsAccepted);
      window.localStorage.setItem('showDappBetaModal', testWallet.showDappBetaModal);
      window.localStorage.setItem('wallet', testWallet.wallet);
      const keyAgentData = JSON.parse(testWallet?.backgroundStorage?.keyAgentsByChain);
      const mnemonicData = JSON.parse(testWallet?.backgroundStorage?.mnemonic);
      const backgroundStorage = {
        mnemonic: mnemonicData,
        keyAgentsByChain: keyAgentData,
        MIGRATION_STATE: { state: 'up-to-date' }
      };
      window.localStorage.setItem('BACKGROUND_STORAGE', JSON.stringify(backgroundStorage));
      window.localStorage.setItem('appSettings', testWallet.appSettings);
      window.localStorage.setItem('keyAgentData', testWallet.keyAgentData);
    }, testWallet);
    await page.goto(`${EXTENSION_URL}/app.html`);
    await page.waitForTimeout(5000);
    await page.reload();

    // authorize Dapp
    // await page.getByRole('button').filter({ hasText: 'Lace' }).click();

    // const popupPromise = context.waitForEvent('page');
    // await Promise.race([
    //   new Promise((resolve) => setTimeout(resolve, 5000)),
    //   page.evaluate(async () => await window['cardano']['lace'].enable())
    // ]);
    // const popup = await popupPromise;
    // await popup.waitForLoadState();
    // await popup.waitForTimeout(5000);

    // await page.getByRole('button').filter({ hasText: 'Authorize' }).click();
    // await page.getByRole('button').filter({ hasText: 'Always' }).click();

    // await page.goto(`${EXTENSION_URL}/app.html`);
    // await page.waitForTimeout(5000);
    // await page.reload();

    this.screen = { context, page };

    return this.screen
  }


  private newContext = async (): Promise<BrowserContext> => {
    const automationBrowsers = ['chromium', 'firefox', 'webkit']
    type AutomationBrowser = typeof automationBrowsers[number]
    const automationBrowser = env('UI_AUTOMATION_BROWSER') as AutomationBrowser
    const pathToExtension = env('LACE_WALLET_EXTENSION_PATH') as AutomationBrowser

    const browserType: BrowserType = playwright[automationBrowser];
    const context = await browserType.launchPersistentContext('', {
      devtools: process.env.DEVTOOLS !== 'false',
      headless: process.env.HEADLESS !== 'false',
      args: [
          '--no-sandbox',
          '--disable-gpu',
          '--disable-notifications',
          '--enable-automation',
          '--no-first-run',
          '--no-default-browser-check',
          `--disable-extensions-except=${pathToExtension}`,
          `--load-extension=${pathToExtension}`,
          '--disable-web-security',
          '--allow-insecure-localhost',
          '--window-size=1920,1080',
          '--allow-file-access-from-files',
          '--disable-dev-shm-usage',
          '--remote-allow-origins=*',
          '--disable-features=IsolateOrigins, site-per-process'
        ]
    })

    return context;
  }
}

setWorldConstructor(ScenarioWorld);
