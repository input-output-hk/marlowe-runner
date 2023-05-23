### ACTUS

ACTUS proposes a global standard for the consistent representation of financial instruments by breaking down the diversity of financial instruments into distinct cash flow patterns.

Financial contracts are mutual agreements between parties about future cash flows. The ACTUS specification defines the semantics of a financial contract per contract type by providing the algorithms and formulas for computing the cash-flows explicitly.

The [taxonomy](https://www.actusfrf.org/taxonomy) of financial contracts is maintained by the ACTUS foundation.

---

#### Amortizing loans

> NOTE: This version of ACTUS Labs prototype implements only this kind of contracts.

An amortizing loan is a loan where according to an amortization schedule payments are executed to pay back the principal with interest.


* ##### Principal at maturity (PAM)
  Principal at maturity only defines periodic interest payments, the full principal is due at maturity.

* ##### Linear Amortizer (LAM)
  Regular principal repayments over time, the interest payments decrease linearly.

* ##### Negative Amortizer (NAM)
  Negative amortization means that the payments per period are smaller than the interest, i.e. the balance of the loan increases over time.

* ##### Annuity (ANN)
  The annuity amortization consists of regular payments of equal amounts over the lifetime of the loan.

---

#### ACTUS contracts
An ACTUS contract is basically a state machine: When an event occurs, scheduled or unscheduled, there might be cashflow and the internal state of the contract is updated.

ACTUS has a list of well defined, possible events, for example:
* IP: Interest payment
* MD: Maturity date

The specification of the full list can be found in the [actus-dictionary for events](https://github.com/actusfrf/actus-dictionary/blob/3076d91c4112221458f2137442c644f35ca7a77c/actus-dictionary-event.json#L41).

In order to get a Marlowe representation of an ACTUS contract, first the projected cash flows are generated.
