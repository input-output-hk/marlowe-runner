@smoke
@regression

Feature: As a user, I would like to apply an input on a current contract

    As a user I would like to apply an input on a current contract
    So that I can proceed through the contract

    @creating-deposit
    Scenario Outline: Creating a deposit with a wallet
      Given I use alice <wallet_name> browser
      And I am on the "home" page
      Then I should see a "heading" with "Choose a wallet" text

      When I authorize the app
      Then I should see a "button" with "Create a contract" text

      When I click the "button" with "Create a contract" text
      And I generate the contract "SimpleDeposit" and write it to "/tmp/deposit.json"
      And I enter the contents of "/tmp/deposit.json" into the "contract-input" field

      When I click the "button" with "Submit contract" text And sign the transaction
      Then I should see the first "button" showing "Syncing" text
      And I should see the first "button" showing "Advance" text
      And I should see "Successfully created and submitted the contract. Contract transaction awaits to be included in the blockchain." text

      When I click the first "button" with "Advance" text
      Then I should see a "button" with "Advance contract" text

      When I click the "checkbox" with "Deposit 0.000001 ₳" text
      And I click the "button" with "Advance contract" text And sign the transaction
      And I should see "Successfully applied the inputs. Input application transaction awaits to be included in the blockchain." text

    Examples:
      | wallet_name |
      | lace        |
      # | nami        |

    @creating-choice
    Scenario Outline: Creating a choice with a wallet
      Given I use alice <wallet_name> browser
      And I am on the "home" page
      Then I should see a "heading" with "Choose a wallet" text

      When I authorize the app
      Then I should see a "button" with "Create a contract" text

      When I click the "button" with "Create a contract" text
      And I generate the contract "SimpleChoice" and write it to "/tmp/choice.json"
      And I enter the contents of "/tmp/choice.json" into the "contract-input" field

      When I click the "button" with "Submit contract" text And sign the transaction
      Then I should see the first "button" showing "Syncing" text
      And I should see the first "button" showing "Advance" text
      And I should see "Successfully created and submitted the contract. Contract transaction awaits to be included in the blockchain." text

      When I click the first "button" with "Advance" text
      Then I should see a "button" with "Advance contract" text

      When I click the "button" with "Advance contract" text And sign the transaction
      Then I should see the first "button" showing "Syncing" text
      And I should see the first "button" showing "Advance" text
      And I should see "Successfully applied the inputs. Input application transaction awaits to be included in the blockchain." text

    Examples:
      | wallet_name |
      | lace        |
      | nami        |


    @creating-timeout
    Scenario Outline: Creating a timed-out contract with a <wallet_name> wallet
      Given I use alice <wallet_name> browser
      Given I am on the "home" page
      Then I should see a "heading" with "Choose a wallet" text

      When I authorize the app
      Then I should see a "button" with "Create a contract" text

      When I click the "button" with "Create a contract" text
      And I generate the contract "TimedOutSimpleChoice" and write it to "/tmp/timed-out-choice.json"
      And I enter the contents of "/tmp/timed-out-choice.json" into the "contract-input" field

      When I click the "button" with "Submit contract" text And sign the transaction
      Then I should see the first "button" showing "Syncing" text
      And I should see the first "button" showing "Advance" text
      And I should see "Successfully created and submitted the contract. Contract transaction awaits to be included in the blockchain." text

      When I click the first "button" with "Advance" text
      Then I should see a "button" with "Advance contract" text

      When I click the "button" with "Advance contract" text And sign the transaction
      Then I should see the first "button" showing "Syncing" text
      And I should see the first "button" showing "Advance" text
      And I should see "Successfully applied the inputs. Input application transaction awaits to be included in the blockchain." text

    Examples:
      | wallet_name |
      | lace        |
      | nami        |

    @creating-notify
    Scenario Outline: Creating a notify contract with a <wallet_name> wallet
      Given I use alice <wallet_name> browser
      Given I am on the "home" page
      Then I should see a "heading" with "Choose a wallet" text

      When I authorize the app
      Then I should see a "button" with "Create a contract" text

      When I click the "button" with "Create a contract" text
      And I generate the contract "SimpleNotify" and write it to "/tmp/notify.json"
      And I enter the contents of "/tmp/notify.json" into the "contract-input" field

      When I click the "button" with "Submit contract" text And sign the transaction
      Then I should see the first "button" showing "Syncing" text
      And I should see the first "button" showing "Advance" text
      And I should see "Successfully created and submitted the contract. Contract transaction awaits to be included in the blockchain." text

      When I click the first "button" with "Advance" text
      Then I should see a "button" with "Advance contract" text

      When I click the "button" with "Advance contract" text And sign the transaction
      Then I should see the first "button" showing "Syncing" text
      And I should see the first "button" showing "Advance" text
      And I should see "Successfully applied the inputs. Input application transaction awaits to be included in the blockchain." text

    Examples:
      | wallet_name |
      | lace        |
      | nami        |

    @escrow-with-three-wallets
    Scenario: Creating an escrow contract with two separate wallets
      # Charlie submits the contract
      Given I use charlie lace browser
      # TODO: And I log in
      And I am on the "home" page

      When I authorize the app
      When I click the "button" with "Create a contract" text
      #Escrow: Alice is a buyer, Bob is a seller, Charlie is a mediator
      And I generate Escrow contract with "alice" as a buyer and "bob" as a seller and "charlie" as a mediator and call it "escrow"

      And I enter the json of the contract "escrow" into the "contract-input" field
      When I click the "button" with "Submit contract" text And sign the transaction

      Then I can see "escrow" contract id in the first row in the table
      Then I should see "Syncing" status of the "escrow" contract
      Then I should see "Awaiting other party" status of the "escrow" contract

      # Alice performs first deposit and reports a problem
      Given I use alice lace browser
      And I am on the "home" page
      When I authorize the app
      Then I should see "Advance" status of the "escrow" contract

      When I start advancing "escrow" contract
      And I click the "button" with "Advance contract" text And sign the transaction
      Then I should see "Advance" status of the "escrow" contract

      When I start advancing "escrow" contract
      And I click the "checkbox" with "Report problem" text
      And I click the "button" with "Advance contract" text And sign the transaction
      Then I should see "Awaiting other party" status of the "escrow" contract

      # Bob disputes the problem
      Given I use bob lace browser
      And I am on the "home" page
      When I authorize the app
      Then I should see "Advance" status of the "escrow" contract

      When I start advancing "escrow" contract
      And I click the "checkbox" with "Dispute problem" text
      And I click the "button" with "Advance contract" text And sign the transaction
      Then I should see "Awaiting other party" status of the "escrow" contract

      # Charlie is a mediator and he resolves the dispute by confirming the problem
      Given I use charlie lace browser
      Then I should see "Advance" status of the "escrow" contract

      When I start advancing "escrow" contract
      And I click the "checkbox" with "Confirm problem" text
      And I click the "button" with "Advance contract" text And sign the transaction
      Then I should see "Complete" status of the "escrow" contract

      Given I use bob lace browser
      Then I should see "Complete" status of the "escrow" contract

      Given I use alice lace browser
      Then I should see "Complete" status of the "escrow" contract


    @submission-failure-with-empty-wallet
    Scenario Outline: Creating a deposit with a wallet
      Given I use alice <wallet_name> browser
      And I am on the "home" page
      When I authorize the app

      When I click the "button" with "Create a contract" text
      And I generate Escrow contract with "empty" as a buyer and "empty" as a seller and "empty" as a mediator and call it "escrow"

      And I enter the json of the contract "escrow" into the "contract-input" field
      When I click the "button" with "Submit contract" text And sign the transaction

      Then I can see "escrow" contract id in the first row in the table
      Then I should see "Syncing" status of the "escrow" contract
      Then I should see "Awaiting other party" status of the "escrow" contract

      Given I use empty <wallet_name> browser
      And I am on the "home" page
      When I authorize the app
      Then I should see "Advance" status of the "escrow" contract

      When I start advancing "escrow" contract
      And I click the "button" with "Advance contract" text And sign the transaction
      Then I should see "Advance" status of the "escrow" contract

    Examples:
      | wallet_name |
      | lace        |
