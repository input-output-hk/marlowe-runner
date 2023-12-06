import { When } from '@cucumber/cucumber';
import { ScenarioWorld } from '../world.js';
import {
  Contract,
  datetoTimeout,
} from "@marlowe.io/language-core-v1";
import { MarloweJSON } from "@marlowe.io/adapter/codec";
import { Bech32 } from '../../cardano.js';

const mkAddressBasedEscrow = (buyerAddress: Bech32, sellerAddress: Bech32, mediatorAddress: Bech32): Contract => {
  const twentyMinutesInMilliseconds = 20 * 60 * 1000;
  const inTwentyMinutes = datetoTimeout(new Date(Date.now() + twentyMinutesInMilliseconds));
  const buyerAddressStr = buyerAddress.toString();
  const sellerAddressStr = sellerAddress.toString();
  const mediatorAddressStr = mediatorAddress.toString();

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
                    address: buyerAddressStr
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
                    address: buyerAddressStr
                  }
                },
                then: {
                  when: [
                    {
                      then: "close",
                      case: {
                        for_choice: {
                          choice_owner: {
                            address: sellerAddressStr
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
                                  address: sellerAddressStr
                                }
                              },
                              then: "close",
                              pay: 10000000n,
                              from_account: {
                                address: buyerAddressStr
                              }
                            },
                            case: {
                              for_choice: {
                                choice_owner: {
                                  address: mediatorAddressStr
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
                                  address: mediatorAddressStr
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
                            address: sellerAddressStr
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
                  address: sellerAddressStr
                }
              },
              case: {
                for_choice: {
                  choice_owner: {
                    address: buyerAddressStr
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
            address: buyerAddressStr
          },
          of_token: {
            token_name: "",
            currency_symbol: ""
          },
          into_account: {
            address: sellerAddressStr
          },
          deposits: 10000000n
        }
      }
    ],
  }
}

When(
  /^I generate Escrow contract with "([^"]*)" as a buyer and "([^"]*)" as a seller and "([^"]*)" as a mediator and call it "([^"]*)"$/,
  async function(this: ScenarioWorld, buyer: string, seller: string, mediator: string, contractNickname: string) {
    const buyerAddress = await this.getWalletAddress(buyer);
    const sellerAddress = await this.getWalletAddress(seller);
    const mediatorAddress = await this.getWalletAddress(mediator);

    const contract = mkAddressBasedEscrow(buyerAddress, sellerAddress, mediatorAddress);
    this.setContractInfo(contractNickname, { contract: contract, contractId: undefined });
  }
);

