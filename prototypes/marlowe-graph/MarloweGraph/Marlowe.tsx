export type Address = string
export type TokeName = string
export type CurrencySymbol = string
export type ValueId = string
export type Timeout = number

export type ChoiceId = { choice_name: string, choice_owner: Party }

export type Payee
  = { account: AccountId }
  | { party: Party }

export type Party
  = { address: Address }
  | { role_token: TokeName }

export type AccountId = Party

export type Token = { currency_symbol: CurrencySymbol, token_name: TokeName }

export type Value
  = { amount_of_token: Token, in_account: AccountId }
  | { negate: Value }
  | { add: Value, and: Value }
  | { value: Value, minus: Value }
  | { multiply: Value, times: Value }
  | { divide: Value, by: Value }
  | { value_of_choice: ChoiceId }
  | { use_value: ValueId }
  | { if: Observation, then: Value, else: Value }
  | "time_interval_start"
  | "time_interval_end"
  | number

export type Observation
  = { both: Observation, and: Observation }
  | { either: Observation, or: Observation }
  | { not: Observation }
  | { chose_something_for: ChoiceId }
  | { value: Value, ge_than: Value }
  | { value: Value, gt: Value }
  | { value: Value, lt: Value }
  | { value: Value, le_than: Value }
  | { value: Value, equal_to: Value }
  | true
  | false

export type Bound = { from: number, to: number }

export type Action
  = { party: Party, deposits: Value, of_token: Token, into_account: AccountId }
  | { choose_between: Bound[], for_choice: ChoiceId }
  | { notify_if: Observation }

export type Case
  = { case: Action, then: Contract, thenEdgeEvents?: ContractEdgeEvents, }
  | { case: Action, merkleized_then: string }

export enum PathState {
  Taken = 0,
  Future = 1,
  Pruned = 2,
}

export interface ContractNodeEvents {
  onClick?(state: PathState, e: React.MouseEvent<HTMLDivElement, MouseEvent>): void
  onMouseOver?(state: PathState, e: React.MouseEvent<HTMLDivElement, MouseEvent>): void
}

export interface ContractEdgeEvents {
  onClick?(state: PathState, e: React.MouseEvent<HTMLDivElement, MouseEvent>): void
  onMouseOver?(state: PathState, e: React.MouseEvent<HTMLDivElement, MouseEvent>): void
}

export type Contract
  = "close"
  | {
    nodeEvents?: ContractNodeEvents,
    thenEdgeEvents?: ContractEdgeEvents,
    pay: Value, token: Token, to: Payee, from_account: AccountId, then: Contract,
  }
  | {
    nodeEvents?: ContractNodeEvents,
    thenEdgeEvents?: ContractEdgeEvents,
    elseEdgeEvents?: ContractEdgeEvents,
    if: Observation, then: Contract, else: Contract,
  }
  | {
    nodeEvents?: ContractNodeEvents,
    timeout_continuationEdgeEvents?: ContractEdgeEvents,
    when: Case[], timeout: Timeout, timeout_continuation: Contract,
  }
  | {
    nodeEvents?: ContractNodeEvents,
    thenEdgeEvents?: ContractEdgeEvents,
    let: ValueId, be: Value, then: Contract,
  }
  | {
    nodeEvents?: ContractNodeEvents,
    thenEdgeEvents?: ContractEdgeEvents,
    assert: Observation, then: Contract,
  }

