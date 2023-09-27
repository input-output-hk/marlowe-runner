/* jshint -W097 */

'use strict';

import { main } from "../output/Main/index.js";
import domready from 'domready';
import "../public/style.scss";
import 'reactflow/dist/style.css'

const config = {
  develMode: process.env.DEVEL_MODE,
  marloweWebServerUrl: process.env.MARLOWE_WEB_SERVER_URL,
};

domready(function () {
  main(config)();
});

