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

    @contract-creation-failure-with-empty-wallet
    Scenario Outline: Creating a deposit with a wallet
      Given I use empty lace browser
      And I am on the "home" page
      When I authorize the app

      When I click the "button" with "Create a contract" text
      And I generate Escrow contract with "empty" as a buyer and "empty" as a seller and "empty" as a mediator and call it "escrow"

      And I enter the json of the contract "escrow" into the "contract-input" field
      When I click the "button" with "Submit contract" text And sign the transaction
      Then I should see error toast
      # The actual error:
      # An error occured during contract submission: (ServerApiError ApiError { message: "CoinSelectionFailed \"Insufficient lovelace available for coin selection: valueFromList [(AdaAssetId,8766894)] required, but valueFromList [] available.\"", error: CoinSelectionFailed, details: {"contents":"Insufficient lovelace available for coin selection: valueFromList [(AdaAssetId,8766894)] required, but valueFromList [] available.","tag":"CoinSelectionFailed"} })#

    @input-application-failure-with-empty-wallet
    Scenario Outline: Creating a deposit with a wallet
      Given I use alice lace browser
      And I am on the "home" page
      When I authorize the app

      When I click the "button" with "Create a contract" text
      And I generate Escrow contract with "empty" as a buyer and "empty" as a seller and "empty" as a mediator and call it "escrow"

      And I enter the json of the contract "escrow" into the "contract-input" field
      When I click the "button" with "Submit contract" text And sign the transaction

      Then I can see "escrow" contract id in the first row in the table
      Then I should see "Syncing" status of the "escrow" contract
      Then I should see "Awaiting other party" status of the "escrow" contract

      Given I use empty lace browser
      And I am on the "home" page
      When I authorize the app
      Then I should see "Advance" status of the "escrow" contract

      When I start advancing "escrow" contract
      And I click the "button" with "Advance contract" text
      Then I should see error toast
      # The actual error:
      # An error occured during contract submission: (ServerApiError ApiError { error: CoinSelectionFailed, message: "CoinSelectionFailed \"No collateral found in [(TxIn \\\"0c338171c1d7a1a7228b60e8ed8a3d168b2fd74853e0cd4bb6fe85aec69786bd\\\" (TxIx 1),TxOut (AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (ScriptHashObj (ScriptHash \\\"d85fa9bc2bdfd97d5ebdbc5e3fc66f7476213c40c21b73b41257f09d\\\")) StakeRefNull)) (TxOutValue MultiAssetInBabbageEra (valueFromList [(AdaAssetId,2000000)])) (TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra (HashableScriptData \\\"\\\\216y\\\\159\\\\216y\\\\159@\\\\255\\\\216y (...)"" })

    @contract-creation-failure-with-no-pure-ada-utxo
    Scenario Outline: Creating a deposit with a wallet
      Given I use no-pure-ada-utxo lace browser
      And I am on the "home" page
      When I authorize the app

      When I click the "button" with "Create a contract" text
      And I generate Escrow contract with "empty" as a buyer and "empty" as a seller and "empty" as a mediator and call it "escrow"

      And I enter the json of the contract "escrow" into the "contract-input" field
      When I click the "button" with "Submit contract" text And sign the transaction
      Then I should see error toast
      # The actual error:
      # An error occured during contract submission: (ServerApiError ApiError { message: "CoinSelectionFailed \"Insufficient lovelace available for coin selection: valueFromList [(AdaAssetId,9340124)] required, but valueFromList [(AdaAssetId,5172320),(AssetId \\\"0122e5079749e36fc703335621430e65017231726166e319a32ed831\\\" \\\"Other provider\\\",4),(AssetId \\\"32dada46a8f74bc884627f1258f52777b9a95bd6d0a492ccaabd6d99\\\" \\\"Mediator\\\",4),(AssetId \\\"58a4d8a00bf87cdc21af08c0fbbe0c8d239ea428386df0b1494cfd97\\\" \\\"Withdrawer\\\",4)] available.\"", error: CoinSelectionFailed, details: {"contents":"Insufficient lovelace available for coin selection: valueFromList [(AdaAssetId,9340124)] required, but valueFromList [(AdaAssetId,5172320),(AssetId \"0122e5079749e36fc703335621430e65017231726166e319a32ed831\" \"Other provider\",4),(AssetId \"32dada46a8f74bc884627f1258f52777b9a95bd6d0a492ccaabd6d99\" \"Mediator\",4),(AssetId \"58a4d8a00bf87cdc21af08c0fbbe0c8d239ea428386df0b1494cfd97\" \"Withdrawer\",4)] available.","tag":"CoinSelectionFailed"} })#

    @submission-failure-of-input-application-for-closed-contract
    Scenario Outline: User tries to apply the input on a closed contract
      Given I use alice lace browser
      Given I am on the "home" page
      When I authorize the app

      When I click the "button" with "Create a contract" text
      And I generate "DoubleDeposit" contract with "alice" as a first depositor and "bob" as a second depositor and call it "double-deposit"
      And I enter the json of the contract "double-deposit" into the "contract-input" field

      When I click the "button" with "Submit contract" text And sign the transaction
      Then I can see "double-deposit" contract id in the first row in the table

      Then I should see success toast and close it
      Then I should see "Advance" status of the "double-deposit" contract
      # We leave this user on the input application page
      When I start advancing "double-deposit" contract

      # and switch to the second user
      Given I use bob lace browser
      Given I am on the "home" page
      When I authorize the app
      Then I should see "Advance" status of the "double-deposit" contract
      When I start advancing "double-deposit" contract
      And I click the "checkbox" with "Deposit 2 ₳" text
      When I click the "button" with "Advance contract" text And sign the transaction
      Then I should see "Syncing" status of the "double-deposit" contract
      Then I should see "Complete" status of the "double-deposit" contract

      # Now we switch back to the first user
      Given I use alice lace browser
      When I click the "button" with "Advance contract" text
      # FAIULRE
      Then I should see error toast
      # The actual error:
      # An error occured during contract submission: (ServerApiError ApiError { message: "LoadMarloweContextErrorNotFound", error: LoadMarloweContextErrorNotFound, details: {"tag":"LoadMarloweContextErrorNotFound"} })#

    @submission-failure-of-exclusive-input-application
    Scenario Outline: Two users try to apply the inputs which are exclusive
      Given I use alice lace browser
      Given I am on the "home" page
      When I authorize the app

      When I click the "button" with "Create a contract" text
      And I generate "DoubleDepositAndNotify" contract with "alice" as a first depositor and "bob" as a second depositor and call it "double-deposit"
      And I enter the json of the contract "double-deposit" into the "contract-input" field

      When I click the "button" with "Submit contract" text And sign the transaction
      Then I can see "double-deposit" contract id in the first row in the table
      Then I should see success toast and close it

      Then I should see "Advance" status of the "double-deposit" contract
      # We leave this user on the input application page
      When I start advancing "double-deposit" contract

      # and switch to the second user
      Given I use bob lace browser
      Given I am on the "home" page
      When I authorize the app
      Then I should see "Advance" status of the "double-deposit" contract
      When I start advancing "double-deposit" contract
      And I click the "checkbox" with "Deposit 2 ₳" text
      When I click the "button" with "Advance contract" text And sign the transaction
      Then I should see success toast and close it
      Then I should see "Syncing" status of the "double-deposit" contract
      Then I should see "Advance" status of the "double-deposit" contract

      # Now we switch back to the first user
      Given I use alice lace browser
      When I click the "button" with "Advance contract" text
      # FAIULRE
      Then I should see error toast
      # The actual error:
      # An error occured during contract submission: (ServerApiError ApiError { message: "MarloweComputeTransactionFailed \"TEApplyNoMatchError\"", error: MarloweComputeTransactionFailed, details: {"contents":"TEApplyNoMatchError","tag":"MarloweComputeTransactionFailed"} })#

    # This artificial scenario is actually trying to test a behavior and error reporting
    # of submission of an invalid transaction.
    # * We ask the Runtime to create a transaction and keep it
    # * We submit a colliding transaction
    # * We try to submit the kept transaction
    @input-application-failure-of-expired-transaction
    Scenario Outline: Two users try to apply the inputs which are exclusive. The second submission is done using old transaction
      Given I use alice lace browser
      Given I am on the "home" page
      When I authorize the app

      When I click the "button" with "Create a contract" text
      And I generate "DoubleDepositAndNotify" contract with "alice" as a first depositor and "bob" as a second depositor and call it "double-deposit"
      And I enter the json of the contract "double-deposit" into the "contract-input" field

      When I click the "button" with "Submit contract" text And sign the transaction
      Then I can see "double-deposit" contract id in the first row in the table
      Then I should see success toast and close it

      Then I should see "Advance" status of the "double-deposit" contract
      # We leave this user on the input application page
      When I start advancing "double-deposit" contract
      When I click the "button" with "Advance contract" text And grab wallet popup

      # and switch to the second user
      Given I use bob lace browser
      Given I am on the "home" page
      When I authorize the app
      Then I should see "Advance" status of the "double-deposit" contract
      When I start advancing "double-deposit" contract
      And I click the "checkbox" with "Deposit 2 ₳" text
      When I click the "button" with "Advance contract" text And sign the transaction
      Then I should see success toast and close it
      Then I should see "Syncing" status of the "double-deposit" contract
      Then I should see "Advance" status of the "double-deposit" contract

      # Now we switch back to the first user
      Given I switch to "alice" lace browser and finally sign the transaction
      Then I should see error toast
      # The actual error:
      # Status Code: 403
      # {
      #     "details": null,
      #     "errorCode": "SubmissionError",
      #     "message": "SubmitFailed \"TxValidationErrorInMode (ShelleyTxValidationError ShelleyBasedEraBabbage (ApplyTxError [UtxowFailure (AlonzoInBabbageUtxowPredFailure (NonOutputSupplimentaryDatums (fromList [SafeHash \\\"68534c553c71d5435372939ddcb3befe73f51d4036c6651d8ad122be55b45085\\\"]) (fromList [SafeHash \\\"14a0df629b39faa3b76b8c2aaaa59d7cbfe9e5f297a1f785b7e9660c37f1c53f\\\"]))),UtxowFailure (AlonzoInBabbageUtxowPredFailure (ExtraRedeemers [RdmrPtr Spend 1])),UtxowFailure (AlonzoInBabbageUtxowPredFailure (PPViewHashesDontMatch (SJust (SafeHash \\\"1557169995ed8cb99308b23de184cc46e8ea43a41024df2bba978ac76906584f\\\")) (SJust (SafeHash \\\"27641f03212d3ab79d4c420a630e6db8586edb40fb1d8eadfbb42b3303ef6e79\\\")))),UtxowFailure (UtxoFailure (AlonzoInBabbageUtxoPredFailure (ValueNotConservedUTxO (MaryValue 7790191 (MultiAsset (fromList []))) (MaryValue 9790191 (MultiAsset (fromList [])))))),UtxowFailure (UtxoFailure (AlonzoInBabbageUtxoPredFailure (BadInputsUTxO (fromList [TxIn (TxId {unTxId = SafeHash \\\"eea2f67cce5ccb720897f10365d9c8bee81c4c8af3192cdc27bdf598dc173b5f\\\"}) (TxIx 1)]))))])) BabbageEraInCardanoMode\""
      # }
      #
