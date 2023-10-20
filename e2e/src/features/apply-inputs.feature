@smoke
@regression

Feature: As a user, I would like to apply an input on a current contract

    As a user I would like to apply an input on a current contract
    So that I can proceed through the contract

    @dev
    Scenario: Creating a deposit with a Lace wallet
      Given I configure my lace wallet
      And I am on the "home" page
      Then I should see a "heading" with "Choose a wallet" text

      When I authorize my lace wallet
      Then I should see a "button" with "Create a contract" text

      When I click the "button" with "Create a contract" text
      And I generate the contract "SimpleDeposit" and write it to "/tmp/deposit.json"
      And I enter the contents of "/tmp/deposit.json" into the "contract-input" field
      # # And I enter the contents of "../support/contracts/lace-10-2024/deposit.json" into the "contract-input" field
      # Then I pause the page

      # When I click the "button" with "Submit contract" text And sign the transaction
      # Then I should see a "button" with "Syncing" text
      # And I should see "Successfully created and submitted the contract. Contract transaction awaits to be included in the blockchain." text

    Scenario: Creating a choice with a Lace wallet
      Given I am on the "home" page
      When I authorize my "lace" wallet
      Then I should see a "button" with "Create a contract" text

      When I click the "button" with "Create a contract" text
      And I enter the contents of "../support/contracts/lace-10-2024/choice.json" into the "contract-input" field

      When I click the "button" with "Submit contract" text And sign the transaction
      Then I should see a "button" with "Syncing" text
      And I should see "Successfully created and submitted the contract. Contract transaction awaits to be included in the blockchain." text

    Scenario: Creating a notify contract with a Lace wallet
      Given I am on the "home" page
      When I authorize my "lace" wallet
      Then I should see a "button" with "Create a contract" text

      When I click the "button" with "Create a contract" text
      And I enter the contents of "../support/contracts/lace-10-2024/notify.json" into the "contract-input" field

      When I click the "button" with "Submit contract" text And sign the transaction
      Then I should see a "button" with "Syncing" text
      And I should see "Successfully created and submitted the contract. Contract transaction awaits to be included in the blockchain." text

#    Scenario: Creating a notify contract with a Lace wallet
#      Given I am on the "home" page
#      When I authorize my "lace" wallet
#      Then I should see a "button" with "Create a contract" text
#
#      When I click the "button" with "Create a contract" text
#      And I generate the contract "TimeoutedOut" and put write it to "/tmp/timed-out.json"
#      And I enter the contents of "/tmp/timed-out.json" into the "contract-input" field
#
#      When I click the "button" with "Submit contract" text And sign the transaction
#      Then I should see a "button" with "Syncing" text
#      And I should see "Successfully created and submitted the contract. Contract transaction awaits to be included in the blockchain." text
