// The purpose of this file is to be able to enable dynamic paramters to be passed via the actual cucumber arguments 
// that we pass when calling the cucumber executable.

import dotenv from 'dotenv';
import { env, getJsonFromFile } from './env/parseEnv.js';
import {
  GlobalConfig,
  HostsConfig,
  PagesConfig,
  PageElementMappings,
  DateTimeFormat,
} from './env/global.js';
// import { CucumberJSAllureFormatter, AllureRuntime } from "allure-cucumberjs";
import path from "path";
import { fileURLToPath } from 'url';

import * as fs from 'fs';

dotenv.config({path: './env/common.env'});
const hostsConfig: HostsConfig = getJsonFromFile(env('HOSTS_URLS_PATH'))
const pagesConfig: PagesConfig = getJsonFromFile(env('PAGE_URLS_PATH'))
const pageMappingFiles = fs.readdirSync(`${process.cwd()}${env('PAGE_ELEMENT_MAPPINGS_PATH')}`)

const pageElementMappings: PageElementMappings = pageMappingFiles.reduce(
  (pageElementConfigAcc, file) => {
    const key = file.replace('.json', '');
    const elementMappings = getJsonFromFile(`${env('PAGE_ELEMENT_MAPPINGS_PATH')}${file}`);
    return { ...pageElementConfigAcc, [key]: elementMappings}
  },
  {}
);

const simulatorDateFormat: DateTimeFormat = "D MMM YYYY HH:mm [GMT]Z";

const worldParameters: GlobalConfig = {
  hostsConfig,
  pagesConfig,
  pageElementMappings,
  simulatorDateFormat,
};

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

export const common = {
  import: ['./src/step-definitions/**/**/*.js'],
  paths: ['./src/features/**/*.feature'],
  format: [path.resolve(__dirname, "reporter.js"), "progress-bar"],
  worldParameters
}

export default common;

