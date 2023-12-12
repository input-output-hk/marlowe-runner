@smoke
@regression

@nami
Feature: As a user, I would like to initialize a nami wallet and use it

    As a user I would like to initialize a nami wallet and use it

    @nami-configuration
    Scenario Outline: I use alice nami browser
      Given I use alice nami browser

    @nami-authorization
    Scenario Outline: Connect a nami wallet
      Given I use alice nami browser
      And I am on the "home" page
      Then I should see a "heading" with "Choose a wallet" text

      When I authorize the app
      Then I should see a "button" with "Create a contract" text
