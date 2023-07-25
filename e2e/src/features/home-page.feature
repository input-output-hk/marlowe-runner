@dev
@smoke
@regression

Feature: As a user I should be able to see the Marlowe Playground home page

    As a user I should be able to navigate to the marlowe playground home page and
    see all the starting point options available

    Scenario: As a user I expect to be able to see the available languages
      Given I am on the "home" page
      Then I should see a "heading" with "Your Marlowe Contracts" text
      And I should see a "button" with "Create Contract" text
      And I should see a "button" with "Use Contract Template" text

