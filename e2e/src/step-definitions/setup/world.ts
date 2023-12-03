
import playwright, {
  BrowserContextOptions,
  Page,
  BrowserContext,
  BrowserType
} from 'playwright';
import { env } from '../../env/parseEnv.js'
import * as fs from 'fs';
import * as nami from '../wallets/nami.js';
import * as lace from '../wallets/lace.js';
import { World, IWorldOptions, setWorldConstructor } from "@cucumber/cucumber";
import { GlobalConfig } from '../../env/global.js';
import GlobalStateManager from "../../support/globalStateManager.js";
import { Bech32 } from '../../cardano.js';

export type WalletType = "nami" | "lace";

type Screen = {
  page: Page
  // TODO: Hide contect and expose only page related methods
  context: BrowserContext,
  // getDefaultPage: () => Promise<Page>
  // getNewPage: () => Promise<Page>
  wallet: {
    address: Bech32,
    name: string,
    mnemonic: string[],
    url: string,
    type: WalletType,
  }
};

type Screens = (walletType: WalletType, walletName:string) => Promise<Screen>;

const walletURLs = async (context: BrowserContext) => {
  let background = context.serviceWorkers();
  while (background.length != 2) {
    await context.waitForEvent('serviceworker');
    background = context.serviceWorkers();
  };
  const laceId = background[0].url().split('/')[2];
  const laceURL = 'chrome-extension://' + laceId;

  const namiId = background[1].url().split('/')[2];
  const namiURL = 'chrome-extension://' + namiId;

  return { laceURL, namiURL };
}

export class ScenarioWorld extends World {
  constructor(options: IWorldOptions) {
    super(options)

    this.globalConfig = options.parameters as GlobalConfig;
  }

  globalConfig: GlobalConfig;

  screen: Screen | null = null;
  screens!: Screens;
  globalStateManager = new GlobalStateManager();
  screensCache = {
    lace: {},
    nami: {}
  };

  async init(contextOptions?: BrowserContextOptions): Promise<void> {
    this.screens = await this.mkLazyScreens();
    // I used this default to simplify migration
    // FIXME: we should start here with null
    // this.screen = await this.screens("nami", "alice");
    // return this.screen;
  }

  public getScreen(): Screen {
    if(this.screen == null) {
      throw new Error("Screen is not set. Please use \"I use {walletName} {walletType} browser\" step to set it up.");
    }
    return this.screen;
  }

  private mkLazyScreens = async (): Promise<Screens> => {
    // This was probably copied - currenltly we use chromium only.
    const automationBrowsers = ['chromium', 'firefox', 'webkit']
    type AutomationBrowser = typeof automationBrowsers[number]
    const automationBrowser = env('UI_AUTOMATION_BROWSER') as AutomationBrowser
    const pathToLaceExtension = env('LACE_WALLET_EXTENSION_PATH') as AutomationBrowser
    const pathToNamiExtension = env('NAMI_WALLET_EXTENSION_PATH') as AutomationBrowser
    const browserType: BrowserType = playwright[automationBrowser];

    const cache = this.screensCache;
    const screens = async function(walletType: WalletType, walletName:string): Promise<Screen> {
      if(cache[walletType][walletName]) {
        return cache[walletType][walletName];
      }
      const persistentContextName = '/tmp/' + walletName + '-' + walletType;
      const context = await browserType.launchPersistentContext(persistentContextName, {
        devtools: process.env.DEVTOOLS !== 'false',
        headless: process.env.HEADLESS !== 'false',
        args: [
            '--no-sandbox',
            '--disable-gpu',
            '--disable-notifications',
            '--enable-automation',
            '--no-first-run',
            '--no-default-browser-check',
            `--disable-extensions-except=${pathToLaceExtension},${pathToNamiExtension}`,
            '--disable-web-security',
            '--allow-insecure-localhost',
            '--window-size=1920,1080',
            '--allow-file-access-from-files',
            '--disable-dev-shm-usage',
            '--remote-allow-origins=*',
            '--disable-features=IsolateOrigins, site-per-process'
          ]
      })
      const getDefaultPage = async function(): Promise<Page> {
        const pages = context.pages();
        if (pages.length == 0) {
          return await context.newPage();
        }
        return pages[0];
      }

      const { laceURL, namiURL } = await walletURLs(context);
      const page = await getDefaultPage();
      console.log("PAGE FETCHED")

      const walletURL = walletType == "lace"?laceURL:namiURL;
      const mnemonic = fs.readFileSync('artifacts/mnemonics/' + walletName, 'utf-8').trim().split(' ');
      let address:null|Bech32 = null;
      console.log("MNEMONIC READ")
      switch (walletType) {
        case "lace":
          address = await lace.configure(page, mnemonic, walletURL);
          break;
        case "nami":
          console.log("NAMI CONFIGURE")
          address = await nami.configure(page, mnemonic, walletURL);
          console.log("NAMI CONFIGURED")
          break;
      }
      if(address == null) {
        throw new Error("Wallet configuration failed - provided address is null!");
      }
      const wallet = { address, name: walletName, mnemonic, url: walletURL, type: walletType };
      const screen = { context, page, wallet };
      cache[walletType][walletName] = screen;
      return screen;
    }
    return screens;
  }
}

setWorldConstructor(ScenarioWorld);
