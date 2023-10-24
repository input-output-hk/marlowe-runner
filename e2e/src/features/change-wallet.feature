@smoke
@regression

Feature: As a user, I would like to switch the wallet

    As a user I would like to switch the wallet

    @dev
    Scenario: Install Nami
      Given I configure my nami wallet
      And I am on the "home" page
      Then I should see a "heading" with "Choose a wallet" text

      When I authorize my nami wallet
      Then I should see a "button" with "Create a contract" text
