@dev
@smoke
@regression

Feature: As a user I should be able to choose a wallet

    As a user I should be able to navigate to the marlowe runner home page and
    see all the starting point options available

    Scenario: As a user I expect to be able to choose the Lace wallet
      Given I am on the "home" page
      Then I should see a "heading" with "Choose a wallet" text
      And I should see a "button" with "Cardano" text
