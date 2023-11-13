import { CucumberJSAllureFormatter, AllureRuntime } from "allure-cucumberjs";
import path from "path";
import { fileURLToPath } from 'url';

class Reporter extends CucumberJSAllureFormatter {

  constructor(options) {
    const __filename = fileURLToPath(import.meta.url);
    const __dirname = path.dirname(__filename);
    super(
      options,
      new AllureRuntime({
        resultsDir: path.resolve(__dirname, "allure-results"),
      }),
      {},
    );
  }
}

export default Reporter;