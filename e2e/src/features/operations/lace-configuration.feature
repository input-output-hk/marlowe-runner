@smoke
@regression

@lace
Feature: As a user, I would like to initialize a lace wallet and use it

    As a user I would like to initialize a lace wallet and use it

    @lace-configuration
    Scenario Outline: I use alice lace browser
      Given I use alice lace browser

    @lace-authorization
    Scenario Outline: Connect a lace wallet
      Given I use alice lace browser
      And I am on the "home" page
      Then I should see a "heading" with "Choose a wallet" text

      When I authorize the app
      Then I should see a "button" with "Create a contract" text
