from decimal import Decimal as D

from enron.enron import Asset, AssetBalance, AssetAmount, Account, \
    AccountEntry, DoubleEntry, GeneralLedger, ExchangeRate, ExchangeEntry, \
    AccountGroup, AssetPair, AutoAccountGroup
from enron.exceptions import AssetTypeError, AmountTypeError, DefinitionError

import nose.tools as nosetools
from nose.tools import assert_true, assert_false, nottest
from nose.tools import raises

import math

import logging
logger = logging.getLogger()


@nottest
def clear_all_definitions():
    Account._accounts.clear()
    AccountGroup._groups.clear()
    Asset._assets.clear()
    AssetPair._pairs.clear()


@nottest
def prep_test_assets():
    clear_all_definitions()
    Asset.define("EN")
    Asset.define("RON")


@nottest
def prep_test_assets_and_pairs():
    prep_test_assets()
    AssetPair.define(base="EN", quote="RON")
    AssetPair.define(base="RON", quote="EN")


@nottest
def get_test_account():
    prep_test_assets()
    return Account.define(name="Piggybank", asset="EN", amount=0)


@nottest
def prep_test_ledger():
    clear_all_definitions()
    Asset.define("USD")

    assets = AccountGroup.define(name="assets")
    liabilities = AccountGroup.define(name="liabilities")
    equity = AccountGroup.define(name="equity")

    shareholder_equity = Account.define(
        name="shareholder_equity", asset="USD")
    wells_fargo = Account.define(name="wells_fargo", asset="USD")
    equipment = Account.define(name="equipment", asset="USD")
    cash = Account.define(name="cash", asset="USD")

    assets.add(cash)
    assets.add(equipment)
    equity.add(shareholder_equity)
    liabilities.add(wells_fargo)


@raises(DefinitionError)
def test_asset_not_defined():
    clear_all_definitions()
    Asset.get("EN")


def test_asset_definition():
    clear_all_definitions()
    Asset.define("EN")
    Asset.get("EN")
    Asset["EN"]


def test_equality():
    clear_all_definitions()
    Asset.define("EN")
    a1 = Asset.get("EN")
    a2 = Asset.get("EN")
    assert_true(a1 == a2)
    assert_true(a1 is a2)


@raises(DefinitionError)
def cannot_doubly_define():
    clear_all_definitions()
    Asset.define("EN")
    Asset.define("EN")


@raises(AssertionError)
def cannot_create_asset_from_asset():
    prep_test_assets()
    a = Asset.get("EN")
    Asset.define(a)


@raises(ValueError)
def cannot_compare_to_strings():
    prep_test_assets()
    a = Asset.define("EN")
    if a != "RON":
        logger.info("I think it's safe to skip this account")


def test_make_amount():
    clear_all_definitions()
    a = Asset.define("EN")
    a.make_amount(D(1))
    a.make_amount(10)
    a.make_amount()


def test_asset_str_and_repr():
    clear_all_definitions()
    a = Asset.define("EN")
    logger.info("Found a {0!s} made by {0!r}".format(a))


def test_asset_pair_definition():
    prep_test_assets()
    AssetPair.define(quote=Asset["EN"],
                     base=Asset["RON"])
    # idempotent as hosts are expected to duplicate edges
    AssetPair.define(quote="EN",
                     base="RON")


def test_asset_pair_equality():
    prep_test_assets()
    foobar = AssetPair.define(base=Asset["EN"],
                              quote=Asset["RON"])
    barfoo = AssetPair.define(quote=Asset["EN"],
                              base=Asset["RON"])
    nosetools.eq_(foobar, AssetPair["ENRON"])
    assert_true(foobar != AssetPair.get("RONEN"))
    assert_false(foobar == AssetPair["RONEN"])
    assert_true(AssetPair.get("RONEN") == barfoo)


def test_asset_pair_str_and_repr_():
    prep_test_assets()
    foobar = AssetPair.define(quote=Asset["EN"],
                              base=Asset["RON"])
    logger.info("Found a {0!s} made thusly: {0!r}".format(foobar))


def test_asset_amount_creation():
    prep_test_assets()
    a = Asset.get("EN")
    AssetAmount(asset=a, amount="0")
    AssetAmount(asset=a, amount=1)
    AssetAmount(asset=a, amount=D(1))
    AssetAmount(asset="EN", amount=2)


@raises(AmountTypeError)
def test_cannot_instantiate_from_float_mindlessly():
    prep_test_assets()
    my_fun_val = math.e
    a = Asset.get("EN")
    Account.define(name="Donut", asset=a, amount=my_fun_val)


def test_comparisons_and_ident():
    prep_test_assets()
    a = Asset.get("EN")
    am0 = AssetAmount(asset=a, amount=0)
    am1 = AssetAmount(asset=a, amount=1)
    am2 = AssetAmount(asset=a, amount=2)

    assert_true(am0 < am1)
    assert_false(am0 > am1)
    assert_false(am0 >= am1)
    assert_true(am1 < am2)
    assert_false(am1 > am2)
    assert_false(am1 >= am2)
    assert_true(am1 > am0)
    assert_false(am1 < am0)
    assert_false(am1 <= am0)
    assert_true(am2 > am1)
    assert_false(am2 < am1)
    assert_false(am2 <= am1)

    am3 = AssetAmount(asset=a, amount=0)
    assert_true(am0 >= am3)
    assert_true(am0 <= am3)
    assert_true(am0 == am3)
    assert_false(am0 != am3)
    assert_false(am0 is am3)


def test_asset_math():
    prep_test_assets()
    foo = Asset.get("EN")
    am0 = AssetAmount(asset=foo, amount=0)
    am1 = AssetAmount(asset=foo, amount=1)
    am2 = AssetAmount(asset=foo, amount=2)

    nosetools.eq_(am0 + am2, foo.make_amount(2))
    nosetools.eq_(am2 - am1, foo.make_amount(1))
    nosetools.eq_(am2 * am2 * am2, foo.make_amount(8))
    nosetools.eq_(am2 ** (am2 + am1), foo.make_amount(8))
    nosetools.eq_(am2 / am2, am1)
    nosetools.eq_(am2 // am2, am1)
    nosetools.eq_(am2 % am2, am0)
    nosetools.eq_(-am2, -(am1 + am1))
    nosetools.eq_(abs(am0 - am2), am2)
    nosetools.eq_(+(am0 - am2), am2)


def test_flexible_asset_math():
    prep_test_assets()
    foo = Asset.get("EN")

    nosetools.eq_(foo.make_amount(D(1)) + D(1), D(2))
    nosetools.eq_(foo.make_amount(D(1)) - D(1), D(0))
    nosetools.eq_(foo.make_amount(D(2)) * D(2), D(4))
    nosetools.eq_(foo.make_amount(D(2)) ** D(3), D(8))
    nosetools.eq_(foo.make_amount(D(3)) / D(3), D(1))
    nosetools.eq_(foo.make_amount(D(5)) // D(2), D(2))
    nosetools.eq_(foo.make_amount(D(5)) % D(3), D(2))


def test_asset_amount_str_and_repr():
    prep_test_assets()
    am = AssetAmount(asset="EN", amount=200)
    logger.info("Found a {0!s} made by {0!r}".format(am))


def test_asset_balance_set():
    prep_test_assets()
    am = AssetAmount(asset=Asset.get("RON"), amount=1)
    ab = AssetBalance()
    ab[am.asset] = am


def test_asset_balance_keys_by_string():
    prep_test_assets()
    am = AssetAmount(asset=Asset.get("RON"), amount=1)
    ab = AssetBalance()
    ab["RON"] = am
    ab["RON"]


@raises(KeyError)
def test_missing_asset_get_key_for_balance():
    prep_test_assets()
    am = AssetAmount(asset=Asset.get("RON"), amount=1)
    ab = AssetBalance()
    ab["RON"] = am
    ab["EN"]


@raises(DefinitionError)
def test_missing_asset_set_key_for_balance():
    prep_test_assets()
    am = AssetAmount(asset=Asset.get("RON"), amount=1)
    ab = AssetBalance()
    ab["YOWZERS"] = am


def test_asset_quote_cretion():
    prep_test_assets_and_pairs()
    ExchangeRate(rate=D(1.0),
                 base="EN",
                 quote="RON")


def test_asset_quote_multiplication():
    prep_test_assets_and_pairs()
    aq = ExchangeRate(rate=D(2),
                      base="EN",
                      quote="RON")
    qam = AssetAmount(asset="EN",
                      amount=1)
    res = AssetAmount(asset="RON",
                      amount=2)
    nosetools.eq_(aq * qam, res)
    nosetools.eq_(qam * aq, res)
    nosetools.eq_(qam / aq, res)

    bam = AssetAmount(asset="RON",
                      amount=2)
    res = AssetAmount(asset="EN",
                      amount=1)
    nosetools.eq_(bam * aq, res)
    nosetools.eq_(aq * bam, res)
    nosetools.eq_(bam / aq, res)


def test_asset_quote_str_and_repr():
    prep_test_assets_and_pairs()
    aq = ExchangeRate(rate=D(1.0),
                      base=Asset.get("EN"),
                      quote=Asset.get("RON"))
    logger.info("Found a {0!s} made by {0!r}".format(aq))


def test_account_creation():
    prep_test_assets()
    Account.define(name="Piggybank", asset="EN", amount=0)


def test_balance_math():
    prep_test_assets()
    b0 = AssetBalance()
    foo = Asset["EN"]
    bar = Asset["RON"]
    b0[foo] = foo.make_amount(20)
    b0[bar] = bar.make_amount(100)
    b1 = AssetBalance()
    b1[foo] = foo.make_amount(20)
    b1[bar] = bar.make_amount(100)

    # __eq__ with AssetBalance
    nosetools.eq_(b1, b0)
    nosetools.eq_(b0, b1)

    b1[bar] = bar.make_amount(120)

    # __add__ and __sub___, forward & reversed with AssetBalance
    nosetools.eq_((b0 + b1)[foo], foo.make_amount(40))
    nosetools.eq_((b1 + b0)[foo], foo.make_amount(40))
    nosetools.eq_((b0 - b1)[bar], bar.make_amount(-20))
    nosetools.eq_((b1 - b0)[bar], bar.make_amount(20))

    # __sub__ and __rsub__, __eq__ forward, reversed, with AssetAmount
    nosetools.eq_(b0 - foo.make_amount(20), bar.make_amount(100))
    nosetools.eq_(bar.make_amount(100), b0 - foo.make_amount(20))
    nosetools.eq_(foo.make_amount(20) - b0, bar.make_amount(-100))
    nosetools.eq_(bar.make_amount(-100), foo.make_amount(20) - b0)

    # __radd__ and __add__ asset, with AssetAmount
    nosetools.eq_(foo.make_amount(-20) + b0, bar.make_amount(100))
    nosetools.eq_(b0 + foo.make_amount(-20), bar.make_amount(100))


def test_account_balance_math():
    prep_test_assets()
    ac1 = Account.define(asset="EN", amount=D(12))
    ac2 = Account.define(asset="RON", amount=D(10))

    ab1 = AssetBalance()
    nosetools.eq_(ab1 + ac1, ac1)
    nosetools.eq_(ab1 + ac2, ac2)

    ab2 = AssetBalance()
    ab2 = ab2 + ac1
    ab2 = ab2 + ac2
    nosetools.eq_(ab2["EN"], ac1)
    nosetools.eq_(ab2["RON"], ac2)


@raises(DefinitionError)
def test_cannot_reuse_account_name():
    prep_test_assets()
    Account.define(name="Piggybank", asset="EN", amount=0)
    Account.define(name="Piggybank", asset="EN", amount=0)


def test_get_by_name():
    prep_test_assets()
    ac0 = Account.define(name="PiggyBank", asset="EN", amount=0)
    ac1 = Account.get("PiggyBank")
    assert_true(ac0 is ac1)


@raises(KeyError)
def test_wrong_name():
    prep_test_assets()
    Account.define(name="Piggybank", asset="EN", amount=0)
    Account.get("PiggyBank_whoops")


@raises(AttributeError)
def test_cannot_assign_without_double_entry():
    ac = get_test_account()
    ac.amount = D(100.)


def test_account_entry_creation():
    ac = get_test_account()
    AccountEntry(account=ac, asset="EN", amount="123.13")
    AccountEntry(account=ac, asset_amount=AssetAmount(
        asset="EN", amount="123.13"))
    AccountEntry(account=ac, asset=Asset["EN"], amount=1)
    AccountEntry(account=ac, asset="EN", amount=D(1.0))
    AccountEntry(account=ac, amount=1)


def test_account_entry_comparisons():
    ac = get_test_account()

    ae0 = AccountEntry(account=ac, amount=0)
    ae1 = AccountEntry(account=ac, amount=1)
    ae2 = AccountEntry(account=ac, amount=2)

    assert_true(ae0 < ae1)
    assert_false(ae0 > ae1)
    assert_false(ae0 >= ae1)
    assert_true(ae1 < ae2)
    assert_false(ae1 > ae2)
    assert_false(ae1 >= ae2)
    assert_true(ae1 > ae0)
    assert_false(ae1 < ae0)
    assert_false(ae1 <= ae0)
    assert_true(ae2 > ae1)
    assert_false(ae2 < ae1)
    assert_false(ae2 <= ae1)

    ae4 = AccountEntry(account=ac, amount=1)
    assert_true(ae1 == ae4)
    assert_true(ae1 <= ae4)
    assert_true(ae1 >= ae4)
    assert_false(ae1 != ae4)
    assert_false(ae1 is ae4)


@raises(AttributeError)
def test_cannot_set_account_entry_attributes():
    ac = get_test_account()
    ae = AccountEntry(account=ac, amount=0)
    try:
        try:
            ae.account = ac
        except AttributeError:
            ae.amount = math.e
    except AttributeError:
        ae.asset = Asset["RON"]


def test_get_attributes_from_asset_amount():
    ac = get_test_account()
    ae = AccountEntry(account=ac, amount=D(100))
    ae.amount
    ae.asset


def test_account_entry_str_and_repr():
    ac = get_test_account()
    ae = AccountEntry(account=ac, amount=D(100))
    logger.info("Found a {0!s} made by {0!r}".format(ae))


def test_double_entry_creation():
    prep_test_assets()
    ac0 = Account.define(name="PiggyBank", asset="EN")
    ac1 = Account.define(name="Sofa", asset="EN")
    DoubleEntry(src=ac0, dest=ac1, amount="100.0")
    DoubleEntry(src=ac0, dest=ac1, amount=100, asset="EN")
    DoubleEntry(src=ac0, dest=ac1, amount=D(100), asset=Asset["EN"])

    deposit = AccountEntry(account=ac0, amount=D(100.))
    withdrawal = AccountEntry(account=ac1, amount=D(100.))
    DoubleEntry(deposit=deposit, withdrawal=withdrawal)


def test_double_entry_str_repr__():
    prep_test_assets()
    ac0 = Account.define(name="PiggyBank", asset="EN")
    ac1 = Account.define(name="Sofa", asset="EN")
    de = DoubleEntry(src=ac0, dest=ac1, amount="100.0")
    logger.info("Found a {0!s} created like so: {0!r}".format(de))


def test_exchange_entry_creation_and_realization():
    prep_test_assets_and_pairs()
    foo = Asset["EN"]
    bar = Asset["RON"]
    pb = Account.define(name="PiggyBank", asset="EN")
    sofa = Account.define(name="Sofa", asset="RON")

    rate = ExchangeRate(rate=D(2.0), base="EN", quote="RON")
    tde = ExchangeEntry(asset_amount=Asset["EN"].make_amount(1),
                        rate=rate,
                        src="PiggyBank",
                        dest="Sofa")
    nosetools.eq_(tde.withdrawal.amount, D(1))
    nosetools.eq_(tde.deposit.amount, D(2))

    with GeneralLedger.transaction_lock:
        GeneralLedger.realize(tde)
    nosetools.eq_(pb, foo.make_amount(-1))
    nosetools.eq_(sofa, bar.make_amount(2))


def test_exchange_entry_str_and_repr():
    prep_test_assets_and_pairs()
    Account.define(name="PiggyBank", asset="EN")
    Account.define(name="Sofa", asset="RON")

    rate = ExchangeRate(rate=D(2), base="EN", quote="RON")
    tde = ExchangeEntry(asset_amount=Asset["EN"].make_amount(1),
                        rate=rate,
                        src="PiggyBank",
                        dest="Sofa")

    logger.info("Found a {0!s} created like so: {0!r}".format(tde))


def test_account_group_creation():
    prep_test_assets()
    AccountGroup.define(name="EmbezzlingGeneral")

    ac0 = Account.define(name="PiggyBank", asset="EN")
    ac1 = Account.define(name="Sofa", asset="RON")

    ag = AccountGroup.define(name="Slush", accounts=[ac0, ac1])
    assert_true(AccountGroup.get("Slush") is ag)


def test_account_group_str_repr():
    prep_test_assets()
    ac0 = Account.define(name="PiggyBank", asset="EN")
    ac1 = Account.define(name="Sofa", asset="RON")

    ag = AccountGroup.define(name="Slush", accounts=[ac0, ac1])
    logger.info("Caught a wild {0!s} made when a {0!r} got got".format(ag))


def test_asset_ledger_context_management():
    with GeneralLedger.transaction_lock:
        pass
    assert_false(GeneralLedger.transaction_lock._is_owned())


def test_asset_balance_comparisons():
    clear_all_definitions()
    k = Asset.define("KET")
    b0 = AssetBalance()
    b1 = AssetBalance()
    b0[k] = AssetAmount(asset=k, amount=D("123"))
    b1[k] = AssetAmount(asset=k, amount=D("123"))
    assert_true(AssetAmount(asset=k, amount=D("123")) == b1)
    assert_true(b0 == AssetAmount(asset=k, amount=D("123")))
    assert_true(b0 == b1)


def test_commit_and_rollback_transaction():
    prep_test_ledger()
    ipo_amount = AssetAmount(amount=D(100), asset="USD")
    ipo_entry = DoubleEntry(src="shareholder_equity",
                            dest="equipment",
                            asset_amount=ipo_amount)
    with GeneralLedger.transaction_lock:
        GeneralLedger.commit(ipo_entry)

    # Network failed.  Remove the committed amount to free it back up.

    with GeneralLedger.transaction_lock:
        GeneralLedger.rollback(ipo_entry)


def test_accounts_balance_to_zero():
    clear_all_definitions()
    Asset.define("USD")
    assets = Account.define(name="Assets", asset="USD")
    liabilities = Account.define(name="Liabilities", asset="USD")
    equity = Account.define(name="Equity", asset="USD")

    ipo_amount = AssetAmount(amount=D(100), asset="USD")
    ipo_entry = DoubleEntry(src=equity, dest=assets, asset_amount=ipo_amount)

    loan_amount = AssetAmount(amount=D(50), asset="USD")
    loan_entry = DoubleEntry(
        src=liabilities, dest=assets, asset_amount=loan_amount)

    with GeneralLedger.transaction_lock:
        GeneralLedger.realize(ipo_entry)
        GeneralLedger.realize(loan_entry)

    assert_true(assets + liabilities + equity == D(0))


def test_accounting_equation_with_groups():
    clear_all_definitions()
    Asset.define("USD")

    assets = AccountGroup.define(name="assets")
    liabilities = AccountGroup.define(name="liabilities")
    equity = AccountGroup.define(name="equity")

    shareholder_equity = Account.define(
        name="shareholder_equity", asset="USD")
    wells_fargo = Account.define(name="wells_fargo", asset="USD")
    equipment = Account.define(name="equipment", asset="USD")
    cash = Account.define(name="cash", asset="USD")

    assets.add(cash)
    assets.add(equipment)
    equity.add(shareholder_equity)
    liabilities.add(wells_fargo)

    ipo_amount = AssetAmount(amount=D(100), asset="USD")
    ipo_entry = DoubleEntry(src=shareholder_equity,
                            dest=equipment,
                            asset_amount=ipo_amount)

    loan_amount = AssetAmount(amount=D(50), asset="USD")
    loan_entry = DoubleEntry(src=wells_fargo,
                             dest=cash,
                             asset_amount=loan_amount)

    with GeneralLedger.transaction_lock:
        GeneralLedger.realize(ipo_entry)
        GeneralLedger.realize(loan_entry)

    assert_true(assets + liabilities + equity ==
                AssetAmount(asset="USD", amount=D(0)))


@raises(ValueError)
def test_cannot_realize_without_lock():
    prep_test_ledger()
    ipo_amount = AssetAmount(amount=D(100), asset="USD")
    ipo_entry = DoubleEntry(src="shareholder_equity",
                            dest="equipment",
                            asset_amount=ipo_amount)
    GeneralLedger.realize(ipo_entry)


@raises(ValueError)
def test_cannot_commit_without_lock():
    prep_test_ledger()
    ipo_amount = AssetAmount(amount=D(100), asset="USD")
    ipo_entry = DoubleEntry(src="shareholder_equity",
                            dest="equipment",
                            asset_amount=ipo_amount)
    GeneralLedger.commit(ipo_entry)


@raises(ValueError)
def test_cannot_unrealize_without_lock():
    prep_test_ledger()
    ipo_amount = AssetAmount(amount=D(100), asset="USD")
    ipo_entry = DoubleEntry(src="shareholder_equity",
                            dest="equipment",
                            asset_amount=ipo_amount)
    with GeneralLedger.transaction_lock:
        GeneralLedger.realize(ipo_entry)

    # Transaction fell through in real life.  Let's revert.
    GeneralLedger.unrealize(ipo_entry)


@raises(ValueError)
def tried_to_roll_back_committed_transaction():
    prep_test_ledger()
    ipo_amount = AssetAmount(amount=D(100), asset="USD")
    ipo_entry = DoubleEntry(src="shareholder_equity",
                            dest="equipment",
                            asset_amount=ipo_amount)
    with GeneralLedger.transaction_lock:
        GeneralLedger.realize(ipo_entry)

    # oops, network failed.  Better roll it back.

    with GeneralLedger.transaction_lock:
        GeneralLedger.rollback(ipo_entry)


def test_anon_account_def():
    prep_test_assets()
    a = Account.define(asset="EN")
    assert(type(a.name) is str)


def test_anon_account_group():
    prep_test_assets()
    a = AccountGroup.define()
    assert(type(a.name) is str)


def test_auto_asset_group():
    prep_test_assets()
    a = AutoAccountGroup.define()
    foo_ac = a.account_for_asset("EN")
    bar_ac = a.account_for_asset("RON")
    a.remove(foo_ac)
    a.remove(bar_ac)
    foo_ac = a.account_for_asset("EN")
    foo_ac2 = a.account_for_asset("EN")
    assert(foo_ac is foo_ac2)
