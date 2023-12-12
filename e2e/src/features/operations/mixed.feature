@smoke
@regression

@wallets-mixed
Feature: As a user, I would like to initialize multiple wallets in a scenario and use them

    @two-lace-wallets
    Scenario Outline: I use alice lace browser together with bob lace browser
      Given I use alice lace browser
      And I use bob lace browser

    @two-nami-wallets
    Scenario Outline: I use alice nami browser together with bob nami browser
      Given I use alice nami browser
      And I use bob nami browser

    @nami-lace-wallets
    Scenario Outline: I use alice lace browser together with bob nami browser
      Given I use alice lace browser
      And I use bob nami browser

  #     @lace-authorization
  #     Scenario Outline: Connect a lace wallet
  #       Given I use alice lace browser
  #       And I am on the "home" page
  #       Then I should see a "heading" with "Choose a wallet" text
  # 
  #       When I authorize the wallet
  #       Then I should see a "button" with "Create a contract" text
