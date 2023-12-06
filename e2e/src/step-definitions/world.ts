import playwright, {
  BrowserContextOptions,
  Page,
  BrowserContext,
  BrowserType
} from 'playwright';
import { env } from '../env/parseEnv.js'
import * as fs from 'fs';
import * as nami from './wallets/nami.js';
import * as lace from './wallets/lace.js';
import { World, IWorldOptions, setWorldConstructor } from "@cucumber/cucumber";
import { GlobalConfig } from '../env/global.js';
import GlobalStateManager from "../support/globalStateManager.js";
import { Bech32, ContractId } from '../cardano.js';
import { Contract } from '@marlowe.io/language-core-v1';

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
// FIXME: Currently we are fetching addresses from the wallet UI and not by
// precomputing those or by transforming mnemonic to an addresses.
type Addresses = (walletName:string) => Promise<Bech32>;

type ContractInfo = {
  contractId: ContractId | undefined,
  contract: Contract,
}

const getWalletURL = async (context: BrowserContext) => {
  let background = context.serviceWorkers();
  while (background.length !== 1) {
    await context.waitForEvent('serviceworker');
    background = context.serviceWorkers();
  };
  const walletId = background[0].url().split('/')[2];
  return 'chrome-extension://' + walletId;
}

export class ScenarioWorld extends World {
  constructor(options: IWorldOptions) {
    super(options)

    this.globalConfig = options.parameters as GlobalConfig;
  }

  globalConfig: GlobalConfig;

  screen: Screen | null = null;
  screens!: Screens;
  addresses!: Addresses;
  contracts: { [key: string]: ContractInfo } = {};

  globalStateManager = new GlobalStateManager();

  screensCache = {
    lace: {},
    nami: {}
  };

  async init(contextOptions?: BrowserContextOptions): Promise<void> {
    const screens = await this.mkLazyBrowsers();
    this.screens = screens;
  }

  public getContractInfo(contractName: string): ContractInfo {
    const contractInfo = this.contracts[contractName];
    if (contractInfo === undefined) {
      throw new Error(`Contract ${contractName} is not defined`);
    }
    return contractInfo;
  }

  public setContractInfo(contractName: string, contractInfo: ContractInfo): void {
    this.contracts[contractName] = contractInfo;
  }

  public async getWalletAddress(walletName?:string): Promise<Bech32> {
    // Just check if there is any wallet configured in the cache for a given user and grab the address
    if(walletName === undefined) {
      const { wallet } = this.getScreen();
      return wallet.address;
    }
    const fileContent = await fs.promises.readFile('artifacts/wallets/' + walletName + '/testnet.bech32', 'utf-8');
    return Bech32.fromString(fileContent.trim());
  }

  public getScreen(): Screen {
    if(this.screen == null) {
      throw new Error("Screen is not set. Please use \"I use {walletName} {walletType} browser\" step to set it up.");
    }
    return this.screen;
  }

  private mkLazyBrowsers = async (): Promise<Screens> => {
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
      const pathToExtension = walletType == "lace"?pathToLaceExtension:pathToNamiExtension;
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
            `--disable-extensions-except=${pathToExtension}`,
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

      const walletURL = await getWalletURL(context);
      const page = await getDefaultPage();

      const mnemonic = fs.readFileSync('artifacts/wallets/' + walletName + '/mnemonic', 'utf-8').trim().split(' ');
      const expectedAddress:Bech32 = Bech32.fromString(fs.readFileSync('artifacts/wallets/' + walletName + '/testnet.bech32', 'utf-8').trim());
      let address:null|Bech32 = null;
      switch (walletType) {
        case "lace":
          address = await lace.configure(page, mnemonic, walletURL);
          break;
        case "nami":
          address = await nami.configure(page, mnemonic, walletURL);
          break;
      }
      if(address === null || address.toString() != expectedAddress.toString()) {
        throw new Error("Wallet configuration failed - wallet address is not the same as expected: given " + address + " != expected " + expectedAddress);
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
