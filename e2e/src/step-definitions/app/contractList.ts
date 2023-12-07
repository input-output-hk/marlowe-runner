import { When } from "@cucumber/cucumber";
import { ScenarioWorld } from '../world.js';
import { waitFor, waitForSelectorVisible, waitForTestIdVisible } from "../../support/wait-for-behavior.js";
import { ContractId } from "../../cardano.js";

When(
  /^I can see "([^"]*)" contract id in the first row in the table$/,
  async function(this: ScenarioWorld, contractName: string) {
    const { page } = this.getScreen();
    const contractInfo = this.getContractInfo(contractName);

    if(contractInfo.contractId !== undefined) {
      throw new Error("Contract already exists and has an ID");
    }

    const firstContractRow = await waitForSelectorVisible(page, '#contracts tbody tr:nth-child(1)');
    const contractIdStr = await firstContractRow.getAttribute('data-testId');
    if(contractIdStr === null) {
      throw new Error("Contract row does not have a data-testId attribute");
    }
    const contractId = ContractId.fromString(contractIdStr);
    this.setContractInfo(contractName, { contract: contractInfo.contract, contractId });
  }
)

When(
  /^I should see "([^"]*)" status of the "([^"]*)" contract$/,
  async function(this: ScenarioWorld, expectedStatus: string, contractName: string) {
    const { page } = this.getScreen();
    const contractInfo = this.getContractInfo(contractName);
    const timeout = 240000;

    const actionsCell = await waitForTestIdVisible(page, contractInfo.contractId?.toString() + "-actions", timeout);
    if(actionsCell !== null) {
      await waitFor(async () => {
        const actualStatus = await actionsCell.innerText();
        console.log("actualStatus: " + actualStatus);
        return actualStatus.trim() === expectedStatus
      }, { label: "Contract status is " + expectedStatus, timeout });
    } else {
      throw new Error("Could not find the actions cell for the contract");
    }
  }
)


When(
  /^I start advancing "([^"]*)" contract$/,
  async function(this: ScenarioWorld, contractName: string) {
    const { page } = this.getScreen();
    const contractInfo = this.getContractInfo(contractName);

    const actionsCell = await waitForTestIdVisible(page, contractInfo.contractId?.toString() + "-actions");
    if(actionsCell !== null) {
      const advanceButton = actionsCell.locator('button');
      await advanceButton.click();
    } else {
      throw new Error("Could not find the actions cell for the contract");
    }
  }
)
