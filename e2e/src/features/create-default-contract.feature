@smoke
@regression

Feature: As a user, I would like to create a default contract with my lace wallet

    As a user I would like to log into runner with my lace wallet
    And create a default contract

    Scenario Outline: Creating default escrow contract with Lace wallet
      Given I configure my <wallet_name> wallet
      And I am on the "home" page
      Then I should see a "heading" with "Choose a wallet" text

      When I authorize my <wallet_name> wallet
      Then I should see a "button" with "Create a contract" text

      When I click the "button" with "Create a contract" text
      Then I should see a "tab" with "Source graph" text
      And I should see a "tab" with "Code" text
      And I should see a "button" with "Upload JSON" text
      And I should see a "button" with "Submit contract" text
      And I should see a "button" with "Back to contract list" text

      When I click the "button" with "Submit contract" text And sign the transaction with <wallet_name> wallet
      Then I should see "Successfully created and submitted the contract. Contract transaction awaits to be included in the blockchain." text
      And I should see the first "button" showing "Syncing" text
      And I should see the first "button" showing "Advance" text

    Examples:
      | wallet_name |
      | lace        |
      | nami        |