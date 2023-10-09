@dev
@smoke
@regression

Feature: As a user I should be able to choose a wallet

    As a user I should be able to navigate to the marlowe runner home page and
    see all the starting point options available

    Scenario: As a user I expect to be able to choose the Lace wallet
      Given I am on the "home" page
      Then I should see a "heading" with "Choose a wallet" text

      When I authorize my "lace" wallet
      Then I should see a "button" with "Create a contract" text

      When I click the "button" with "Create a contract" text
      Then I should see a "tab" with "Source graph" text
      And I should see a "tab" with "Code" text
      And I should see a "button" with "Upload JSON" text
      And I should see a "button" with "Submit contract" text
      And I should see a "button" with "Back to contract list" text

      When I click the "button" with "Submit contract" text And sign the transaction
      Then I should see "Successfully created and submitted the contract. Contract transaction awaits to be included in the blockchain." text
