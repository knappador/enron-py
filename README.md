![Liars inc.](https://user-images.githubusercontent.com/641139/39740027-c29a5498-52ce-11e8-9c89-acda215b9116.png "Our trademark lapsed because we tried to get rich by lying and went to jail")
[![Documentation Status](https://readthedocs.org/projects/enron/badge/?version=latest)](http://enron.readthedocs.io/en/latest/?badge=latest)
[![Latest Version](https://pypip.in/version/enron/badge.svg)](https://pypi.python.org/pypi/enron/)

# Enron
Types for use in implementing and executing accounting strategies in memory.  This library is aimed a programmers who need to track, find, or lose (hide) money consistent with an accounting design.

## API Sample
Available on [PyPI](https://pypi.org/project/enron/) `pip install enron`
```python
from decimal import Decimal as D
from enron import *

# Define your asset types up-front, strictly once
Asset.define("ENRN")
Asset.define("ENRN")  # throws DefinitionError("Asset already defined")

# Much effort is made to provide succinctness without creating ambiguity
Asset["ENRN"] == Asset.get("ENRN")  # True

# After definition, you can pass asset argument as a str if the asset isn't handy
retirement = AssetAmount("ENRN", 400)

# ...because all asset args are looked up to avoid accidentally making arbitrary types
safe_money = AssetAmount("ERRRR", 100)  # throws AssetTypeError because asset doesn't exist

# Or the type graph can be navigated from closely related types via make* methods
icy_receipt = Asset["ENRN"].make_amount(12)

# Type signature for Decimals accepts str, int, and Decimal, but not float
AssetAmount(asset="ENRN", amount=400.0)  # throws AmountTypeError
Asset["ENRN"].make_amount("400.0")  # str will be coerced to Decimal

# Anonymous or named accounts
life_savings = Account.define(name="life_savings", asset="ENRN", amount=400)
kens_prison_phone_fund = Account.define(asset="ENRN")

# AssetPairs definition is idempotent since different hosts my duplicate edges
Asset.define("USD")
enrn_usd = AssetPair.define(base="ENRN", quote="USD")

# Apples to Oranges entries are supported via Exchange* types
rate = enrn_usd.make_rate(rate=42)  # <ExchangeRate 42USD per ENRN>

# Exchange rates are inhernetly directed, and multiplication and division infer direction
enrn_amount = Asset["ENRN"].make_amount(1)
usd_amount = Asset["USD"].make_amount(42)

enron.FLEXIBLE_QUOTE_MATH = True  # this is Default
enrn_amount * rate  # <AssetAmount 42USD>
enrn_amount / rate  # <AssetAmount 42USD>
usd_amount * rate  # <AssetAmount 1ENRN>
usd_amount / rate  # <AssetAmount 1ENRN>

# Different hosts may identify different assets differently.  Maps exist for convenience.
sec_map = AssetNameMap({Asset["ENRN"]: "JAIL"})
enron_map = AssetNameMap({Asset["ENRN"]: "RICH"})
enron_map.pair("RICH") == sec_map.pair("JAIL")  # True
enron_map.name(Asset["ENRN"])  # "RICH"

# There's also a pair map for weirdly named exchange edges
sec_pair_map = PairNameMap[{enrn_usd: "Q.ENRN.XXZUSD.V"}]
sec_pair_map.name(enrn_usd)  # "Q.ENRN.XXZUSD.V"

# Setting an account's value directly is forbidden
receipts = Account.define(Asset["ENRN"], 10000)
receipts.amount  # Decimal(10000)
receipts.amount = D(42)  # raises AttributeError("Use DoubleEntry...

# Every account mutation is done by moving money from src to dest
slush_fund = Account.define(Asset["ENRN"])
with GeneralLedger.transaction_lock:N
    embezzlement = DoubleEntry(src=receipts, dest=slush_fund, amount=10000)
    GeneralLedger.realize(embezzlement)
```
See the tests for more examples until the docs are up.

## Motivation
If you need total awareness and correctness of where money or assets are going, using naked addition and subtraction is an easy way to allow bugs (or attacks) to creep into the systems that now pay what was your salary instead to a goofy disgruntled coworker who escaped with a red Swingline® to a beach in Mexico.  

To make matters worse, if you interact with external systems, lack of accounting enforcement in business logic means you won't be able to differentiate or prove that the 3rd party has an error, which could lead to needless debugging and paranoia, all while Milton in Mexico keeps enjoying your retirement.

These bugs are easily catchable by static analyzers or RTTC if the type information is available.  Without a strict systems in place, your destiny is auditing code line-by-line to find the erroneous sign flip or the incomplete handling of a case-switch that allowed money to literally be garbage collected as it slipped out of scope.  This is where eyeballs go to die.

Especially in dynamic languages like Python that are very happy to do what you mean (except it wasn't what you meant), this can be harrowing.  This library can hopefully provide that some critical pieces of the system at a minimum don't hide your flawed logic surruptitiously.  

**The bottom line is that adopting accounting primitives can streamline constraint verification / descrepency detection and diagnosis of logic flaws in-situ while unit & integration tests fundamentally cannot.**

## Design Principles
* **Double-entry mutation** - Moving assets between accounts always uses [double-entry](https://en.wikipedia.org/wiki/Double-entry_bookkeeping_system) semantics and implementation.  It is impossible for money to come from or get lost in space unless that space is explicitly designated.
* **Programmer-centric API conventions** - In traditional accounting, did you know that a "Debit" is sometimes positive and may increase or decrease a balance depending on that accounts "normal balance?"  Let's not do that.  Instead of conditional sign sensing and conditional addition / subtraction, we sense everything positively and use the unmistakeable terms "src" and "dest" in the API.  The only consequence is that instead of `assets = liabilities + equity` you would rearrange an accounting equation to `assets + liabilities + equity = 0`
* **Expressive constraint checking** - Rich comparison & math support - Balance equations can be implemented and calculated without iterating over all accounts.  Support for math on `AccountGroup` directly affords rich comparisons and calculation such as `income = revenue + expenses`
* **Decimal-centric** - Types use Decimal internally and stricter coercion rules to prevent unintentional sources of imbalance, such as constructing a `Decimal` from a `float`
* **Deliberately strict** - Fail early if doing anything that *may possibly* result in burying a bug, such as adding apples to an grapes account or moving from an account to itself.
* **Purpose-generic primitives** - Whether you're tracking FOREX P&L or doing a forensic audit on someone else's inventory & books, the same primitives should accomodate your needs.

## Status Version 0.1
Presently I'm finishing up connecting the type graph and switching away from RTTC towards more proactive PEP484 style type enforcement.  Some RTTC will likely need to be added back in due to the use of metaclasses, but via a better tool than just sprinkling assertions around.

Features are being added as needed.  **Some functionality that has been designed but not planned:**

* Control accounts and subledgers with strict time-period enforcement
* Variable realization of exchange entries to reconcile against an external oracle when exchange is involved
* Routes and route groupings to enforce which accounts can validly transfer to which other accounts and to track movement accross designated boundaries
* Ledger-accounts with O(1) update time and retained ledger
* Richer transaction support and sensible tradeoffs for interaction with database transactions and requirements for guaranteed accurate logs
* Transaction strategies such as all-or-none and context management stack for nested transactions
* Hooks for logging integration to automatically create ledgers from all mutations
* Coercion of ledger entries for consumption by other ledger programs such as [Ledger](https://github.com/ledger/ledger)

## Collaboration & Feature Requests
I will review and merge pull requests for feature upgrades.  Feel free to open issues prior to starting work on a feature to both collaborate on design and get guidance on PR acceptance.  If you need commercial support for a feature, please contact me via github.  I will reserve the right to publish any code open source if it is reaching a point of maintenance conflicts.

## Fake Endorsements
*America's Most Innovative Company* - Fortune Magazine, six years in a row!