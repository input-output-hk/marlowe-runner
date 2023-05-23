/* jshint -W097 */

'use strict';

import { main } from "../output/Main/index.js";
import domready from 'domready';
import "../public/style.scss";
import aboutMarkdown from "../public/about.md";

import actusApplicability from "../static/actus-dictionary-applicability.json";
import actusTerms from "../static/actus-dictionary-terms.json";

const config = {
  develMode: process.env.DEVEL_MODE,
  marloweWebServerUrl: process.env.MARLOWE_WEB_SERVER_URL,
  aboutMarkdown: aboutMarkdown,
  actusDictionaries: {
    applicability: actusApplicability.applicability,
    terms: actusTerms.terms
  }
};

domready(function () {
  main(config)();
});

