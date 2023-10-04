@dev
@smoke
@regression

Feature: As a user I should be able to create a new contract

    As a user I should be able to navigate to the marlowe runner home page and
    see create contract option available

    Scenario: As a user I expect to be able to create a new Marlowe contract wallet
      Given I am on the "home" page
      And I should see a "button" with "Create a contract" text
