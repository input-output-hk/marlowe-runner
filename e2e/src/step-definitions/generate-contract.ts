import { When } from '@cucumber/cucumber';
import { ScenarioWorld } from './setup/world.js';
import {
  Contract,
  datetoTimeout,
} from "@marlowe.io/language-core-v1";
import { MarloweJSON } from "@marlowe.io/adapter/codec";

type ContractName = "SimpleDeposit" | "SimpleChoice" | "TimedOutSimpleChoice" | "SimpleNotify" | "Escrow";

const mkEscrow = (buyerAddress: string, sellerAddress: string): Contract => {  
  const twentyMinutesInMilliseconds = 20 * 60 * 1000;
  const inTwentyMinutes = datetoTimeout(new Date(Date.now() + twentyMinutesInMilliseconds));
 
  return {
    timeout: inTwentyMinutes,
    timeout_continuation: "close",
    when: [
      {
        then: {
          when: [
            {
              then: "close",
              case: {
                for_choice: {
                  choice_owner: {
                    address: buyerAddress
                  },
                  choice_name: "Everything is alright"
                },
                choose_between: [
                  {
                    to: 0n,
                    from: 0n
                  }
                ]
              }
            },
            {
              then: {
                token: {
                  token_name: "",
                  currency_symbol: ""
                },
                to: {
                  account: {
                    address: buyerAddress
                  }
                },
                then: {
                  when: [
                    {
                      then: "close",
                      case: {
                        for_choice: {
                          choice_owner: {
                            address: sellerAddress
                          },
                          choice_name: "Confirm problem"
                        },
                        choose_between: [
                          {
                            to: 1n,
                            from: 1n
                          }
                        ]
                      }
                    },
                    {
                      then: {
                        when: [
                          {
                            then: {
                              token: {
                                token_name: "",
                                currency_symbol: ""
                              },
                              to: {
                                party: {
                                  address: sellerAddress
                                }
                              },
                              then: "close",
                              pay: 10000000n,
                              from_account: {
                                address: buyerAddress
                              }
                            },
                            case: {
                              for_choice: {
                                choice_owner: {
                                  address: sellerAddress
                                },
                                choice_name: "Dismiss claim"
                              },
                              choose_between: [
                                {
                                  to: 0n,
                                  from: 0n
                                }
                              ]
                            }
                          },
                          {
                            then: "close",
                            case: {
                              for_choice: {
                                choice_owner: {
                                  address: sellerAddress
                                },
                                choice_name: "Confirm problem"
                              },
                              choose_between: [
                                {
                                  to: 1n,
                                  from: 1n
                                }
                              ]
                            }
                          }
                        ],
                        timeout_continuation: "close",
                        timeout: inTwentyMinutes
                      },
                      case: {
                        for_choice: {
                          choice_owner: {
                            address: sellerAddress
                          },
                          choice_name: "Dispute problem"
                        },
                        choose_between: [
                          {
                            to: 0n,
                            from: 0n
                          }
                        ]
                      }
                    }
                  ],
                  timeout_continuation: "close",
                  timeout: inTwentyMinutes
                },
                pay: 10000000n,
                from_account: {
                  address: sellerAddress
                }
              },
              case: {
                for_choice: {
                  choice_owner: {
                    address: buyerAddress
                  },
                  choice_name: "Report problem"
                },
                choose_between: [
                  {
                    to: 1n,
                    from: 1n
                  }
                ]
              }
            }
          ],
          timeout_continuation: "close",
          timeout: inTwentyMinutes
        },
        case: {
          party: {
            address: buyerAddress
          },
          of_token: {
            token_name: "",
            currency_symbol: ""
          },
          into_account: {
            address: sellerAddress
          },
          deposits: 10000000n
        }
      }
    ],
  }
}

const mkSimpleDeposit = (address: string): Contract => {
  const twentyMinutesInMilliseconds = 20 * 60 * 1000;
  const inTwentyMinutes = datetoTimeout(new Date(Date.now() + twentyMinutesInMilliseconds));
  return {
    timeout: inTwentyMinutes,
    timeout_continuation: "close",
    when: [
      { case: {
          party: {address: address},
          deposits: 1n,
          of_token: { currency_symbol: "", token_name: "" },
          into_account: {address: address}
        },
        then: "close",
      },
    ]
  };
}

const mkSimpleChoice = (address: string): Contract => {
  const twentyMinutesInMilliseconds = 20 * 60 * 1000;
  const inTwentyMinutes = datetoTimeout(new Date(Date.now() + twentyMinutesInMilliseconds));
  return {
    timeout: inTwentyMinutes,
    timeout_continuation: "close",
    when: [
      { case: {
          choose_between:
            [{
              from: 1n,
              to: 2n
            }],
          for_choice: {
            choice_owner: {address: address},
            choice_name: "simpleChoice",
          }
        },
        then: "close",
      },
    ]
  };
}

const mkTimedOutSimpleChoice = (address: string): Contract => {
  const twentyMinutesInMilliseconds = 20 * 60 * 1000;
  const inTwentyMinutes = datetoTimeout(new Date(Date.now() - twentyMinutesInMilliseconds));
  return {
    timeout: inTwentyMinutes,
    timeout_continuation: "close",
    when: [
      { case: {
          choose_between:
            [{
              from: 1n,
              to: 2n
            }],
          for_choice: {
            choice_owner: {address: address},
            choice_name: "simpleChoice",
          }
        },
        then: "close",
      },
    ]
  };
}
const mkSimpleNotify = (address: string): Contract => {
  const twentyMinutesInMilliseconds = 20 * 60 * 1000;
  const inTwentyMinutes = datetoTimeout(new Date(Date.now() - twentyMinutesInMilliseconds));
  return {
    timeout: inTwentyMinutes,
    timeout_continuation: "close",
    when: [
      { case: {
          notify_if: true,
        },
        then: "close",
      },
    ]
  };
}


// // And I generate the contract "SimpleDeposit" and write it to "/tmp/deposit.json"
When(
  /^I generate the contract "([^"]*)" and write it to "([^"]*)"/,
  async function(this: ScenarioWorld, contractName: ContractName, fileName: string) {
    const {
      globalStateManager
    } = this;
    const walletAddress = globalStateManager.popValue("wallet-address");
    switch (contractName) {
      case "SimpleDeposit":
        const contract1 = mkSimpleDeposit(walletAddress);
        globalStateManager.appendValue(fileName, MarloweJSON.stringify(contract1, null, 4))
        break;
      case "SimpleChoice":
        const contract2 = mkSimpleChoice(walletAddress);
        globalStateManager.appendValue(fileName, MarloweJSON.stringify(contract2, null, 4))
        break;
      case "TimedOutSimpleChoice":
        const contract3 = mkTimedOutSimpleChoice(walletAddress);
        globalStateManager.appendValue(fileName, MarloweJSON.stringify(contract3, null, 4))
        break;
      case "SimpleNotify":
        const contract4 = mkSimpleNotify(walletAddress);
        globalStateManager.appendValue(fileName, MarloweJSON.stringify(contract4, null, 4))
        break;
      case "Escrow": 
        const walletAddress2 = globalStateManager.getValue("wallet-address");
        const contract5 = mkEscrow(walletAddress, walletAddress2);
        globalStateManager.appendValue(fileName, MarloweJSON.stringify(contract5, null, 4))
        break;
    }
  }
);