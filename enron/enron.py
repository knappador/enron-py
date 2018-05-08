import threading
import uuid
from abc import ABCMeta, abstractmethod
from decimal import Decimal as D
from typing import TypeVar, Union, Optional, Sequence, List, AbstractSet, Mapping, \
    Dict, Any

from enron.exceptions import AmountTypeError, AssetTypeError, DefinitionError


FLEXIBLE_QUOTE_MATH = True
FLEXIBLE_ASSET_DECIMAL_MATH = True


_DCoercibles = TypeVar("_DCoercibles", D, str, int)


def _lossless_decimal(maybe_decimal):
    t = type(maybe_decimal)
    if t is D:
        return maybe_decimal
    elif t is float:
        raise AmountTypeError("Decimal accepts floats, but do so on your own, explicitly")
    return D(maybe_decimal)  # throws type errors for non-coercible types


def _auto_asset_arg(arg: Union['Asset', str]) -> 'Asset':
    if type(arg) is not Asset:
        return Asset.get(arg)
    return arg


def _auto_account_arg(arg: Union["Account", str]) -> "Account":
    if type(arg) is not Account:
        return Account.get(arg)
    return arg


def _anonymous_name_if_none(target: Mapping, name: Optional[str]):
    if name is not None:
        return name
    sentinel = 100000
    while name is None:
        name = str(uuid.uuid1())
        if name in target:
            name = None
        sentinel -= 1
        if sentinel < 0:
            raise ValueError("UUID's insufficiently random to "
                             "create new anonymous names")
    return name


def _bad_comparison(one, another):
    return AssetTypeError("Made comparison between {} and {}".format(one.__class__,
                                                                     another.__class__))


class _AddSubscriptableMeta(type):
    '''Hackish.  Adds index support but without infecting all subclasses'''

    def __getitem__(self, key):
        if hasattr(self, "_subscriptable_class"):
            return self._subscriptable_class(key)
        raise TypeError("{} is not subscriptable".format(type(self)))


class _AddSubscriptableABCMeta(ABCMeta):
    '''Hackish.  Adds index support but without infecting all subclasses'''

    def __getitem__(self, key):
        if hasattr(self, "_subscriptable_class"):
            return self._subscriptable_class(key)
        raise TypeError("{} is not subscriptable".format(type(self)))


class _AddSubscriptableABC(metaclass=_AddSubscriptableABCMeta):
    pass


class Asset(metaclass=_AddSubscriptableMeta):
    '''If ever we should take a bread from the turd bucket (aka toilet), the
    universal turd imbalance will teleport us to netherly toil's realm'''
    _assets = {}
    __slots__ = ("symbol")

    @classmethod
    def define(self, symbol: str) -> 'Asset':
        if symbol in self._assets:
            raise DefinitionError("Asset has already defined.")
        self._assets[symbol] = Asset(symbol=symbol,
                                     _incorrectly=False)
        return self._assets[symbol]

    @classmethod
    def get(self, symbol: str) -> 'Asset':
        return self._assets[symbol]

    @classmethod
    def _subscriptable_class(self, key: str) -> 'Asset':
        return self.get(key)

    def __init__(self, *, symbol: str, _incorrectly=True) -> None:
        if _incorrectly:
            raise ValueError(
                "Do not create directly.  Call define (preferably statically in one place)")
        self.symbol = symbol

    def __eq__(self, other: 'Asset') -> bool:
        if issubclass(other.__class__, Asset):
            return self.symbol == other.symbol
        # if the types don't match, comparison is likely a bug
        raise _bad_comparison(self, other)

    def make_amount(self, amount=D(0)) -> 'AssetAmount':
        # RTTC delegated to AssetAmount __init__
        return AssetAmount(asset=self, amount=amount)

    def __repr__(self):
        return "Asset[{!r}]".format(self.symbol)

    def __str__(self):
        return "<Asset {!s}>".format(self.symbol)

    def __hash__(self):
        return hash((self.symbol, Asset))

    @classmethod
    def all_assets(self) -> Dict[str, 'Asset']:
        return self._assets.copy()


class AssetNameMap:
    '''Double map for navigating from remote host str definitions to internal Asset definitions'''

    def __init__(self, definitions: Dict[Asset, str]):
        self._assets_to_names = {}
        self._names_to_assets = {}
        for k in definitions:
            self._assets_to_names[k] = definitions[k]
            self._names_to_assets[definitions[k]] = k

    def asset(self, name: str) -> Asset:
        return self._names_to_assets[name]

    def name(self, asset: Asset) -> str:
        return self._assets_to_names[asset]

    def all_assets(self) -> List[Asset]:
        return list(self._names_to_assets.values())

    def all_names(self) -> List[str]:
        return list(self._assets_to_names.values())


class AssetPair(metaclass=_AddSubscriptableMeta):
    _pairs = {}
    __slots__ = ("base", "quote", "symbol")

    @classmethod
    def define(self, *, base, quote):
        base = _auto_asset_arg(base)
        quote = _auto_asset_arg(quote)
        internal_symbol = base.symbol + quote.symbol
        if internal_symbol in self._pairs:
            return self._pairs[internal_symbol]
        pair = AssetPair(_incorrectly=False,
                         base=base,
                         quote=quote,
                         symbol=internal_symbol)

        self._pairs[internal_symbol] = pair
        return pair

    def __init__(self, *, base, quote, symbol, _incorrectly=True):
        if _incorrectly:
            raise ValueError("Call define rather than instantiating at will")
        self.base = base
        self.quote = quote
        self.symbol = symbol

    @classmethod
    def _subscriptable_class(self, key):
        return self.get(key)

    @classmethod
    def get(self, key):
        if type(key) is not str:
            raise TypeError(
                "Get asset pairs by str symbol.  Recieved {}".format(type(key)))
        return self._pairs[key]

    def make_rate(self, rate: _DCoercibles) -> 'ExchangeRate':
        return ExchangeRate(pair=self, rate=rate)

    def __eq__(self, other: Asset) -> bool:
        if issubclass(other.__class__, AssetPair):
            return self.base == other.base and self.quote == other.quote
        raise _bad_comparison(self, other)  # avoid returning False

    def __repr__(self):
        return "AssetPair[{!r}]".format(self.symbol)

    def __str__(self):
        return "<AssetPair {!s}>".format(self.symbol)

    def __hash__(self):
        return hash((self.quote, self.base, AssetPair))

    @classmethod
    def all_pairs(self):
        return self._pairs.copy()


class PairNameMap:
    '''Double map for navigating from remote host str definitions to internal Asset definitions'''

    def __init__(self, definitions: Mapping[AssetPair, str]) -> None:
        self._pairs_to_names = {}
        self._names_to_pairs = {}

        for k in definitions:
            self._pairs_to_names[k] = definitions[k]
            self._names_to_pairs[definitions[k]] = k

    def pair(self, name: str) -> AssetPair:
        return self._names_to_pairs[name]

    def name(self, pair: Asset) -> str:
        return self._pairs_to_names[pair]

    def all_pairs(self) -> List[AssetPair]:
        return list(self._names_to_pairs.values())

    def all_names(self) -> List[str]:
        return list(self._pairs_to_names.values())


class BalanceMath(_AddSubscriptableABC):

    @abstractmethod
    def _as_asset_balance(self) -> 'AssetBalance':
        ...

    @staticmethod
    def as_asset_balances(left: Any, right: Any) -> ('AssetBalance', 'AssetBalance'):
        if not issubclass(right.__class__, BalanceMath):
            raise _bad_comparison(left, right)
        left = left._as_asset_balance()
        right = right._as_asset_balance()
        return left, right

    @staticmethod
    def _asset_set(left, right):
        keys = set(left.keys())
        keys.update(set(right.keys()))
        return keys

    def __eq__(self, other):
        if (issubclass(self.__class__, AssetMath)
                and issubclass(other.__class__, AssetMath)):
            left, right = AssetMath.as_asset_amounts(self, other)
            if not left.asset == right.asset:
                raise AssetTypeError(
                    "Comparing {!s} to {!s}".format(left.asset, right.asset))
            return left.amount == right.amount
        left, right = BalanceMath.as_asset_balances(self, other)
        for k in BalanceMath._asset_set(left, right):
            if k in left:
                if k in right:
                    if not left[k] == right[k]:
                        return False
                elif not left[k].amount == D(0):
                    return False
            elif k in right:
                if not right[k].amount == D(0):
                    return False
        return True

    def __add__(self, other):
        left, right = BalanceMath.as_asset_balances(self, other)
        ab = AssetBalance()
        for k in BalanceMath._asset_set(left, right):
            if k in left and k in right:
                ab[k] = left[k] + right[k]
            elif k in left:
                ab[k] = left[k]
            else:
                ab[k] = right[k]
        return ab

    def __radd__(self, other):
        return self.__add__(other)

    def __sub__(self, other):
        left, right = BalanceMath.as_asset_balances(self, other)
        ab = AssetBalance()
        for k in BalanceMath._asset_set(left, right):
            if k in left and k in right:
                ab[k] = left[k] - right[k]
            elif k in left:
                ab[k] = left[k]
            else:
                ab[k] = -right[k]
        return ab

    def __rsub__(self, other):
        # reversed the right / left order and use same subtraction logic
        right, left = BalanceMath.as_asset_balances(self, other)
        ab = AssetBalance()
        for k in BalanceMath._asset_set(left, right):
            if k in left and k in right:
                ab[k] = left[k] - right[k]
            elif k in left:
                ab[k] = left[k]
            else:
                ab[k] = -right[k]
        return ab


class AssetMath(BalanceMath):

    @abstractmethod
    def _as_asset_amount(self) -> 'AssetAmount':
        ...

    def _as_asset_balance(self) -> 'AssetBalance':
        asset_amount = self._as_asset_amount()
        balance = AssetBalance()
        balance[asset_amount.asset] = asset_amount
        return balance

    @staticmethod
    def as_asset_amounts(left: Any, right: Any) -> ('AssetAmount', 'AssetAmount'):
        left = left._as_asset_amount()
        if not issubclass(right.__class__, AssetMath):
            if FLEXIBLE_ASSET_DECIMAL_MATH and type(right) is D:
                right = left.asset.make_amount(right)
            else:
                raise _bad_comparison(left, right)
        else:
            right = right._as_asset_amount()
        if left.asset != right.asset:
            raise ValueError("Wrong asset!  Cannot compare apples to oranges")
        return left, right

    '''__eq__ is delegated to BalanceMath in case it needs to up-type to AssetBalance
    for the comparison.  There is no __req__'''

    def __gt__(self, other: Any) -> bool:
        # __lt__ is reflection
        left, right = AssetMath.as_asset_amounts(self, other)
        return left.amount > right.amount

    def __lt__(self, other: Any) -> bool:
        left, right = AssetMath.as_asset_amounts(self, other)
        return left.amount < right.amount

    def __le__(self, other: Any) -> bool:
        left, right = AssetMath.as_asset_amounts(self, other)
        return left.amount <= right.amount

    def __ge__(self, other: Any) -> bool:
        left, right = AssetMath.as_asset_amounts(self, other)
        return left.amount >= right.amount

    def __add__(self, other: Any) -> 'AssetAmount':
        try:
            left, right = AssetMath.as_asset_amounts(self, other)
        except AssetTypeError:
            return NotImplemented
        return AssetAmount(asset=left.asset, amount=left.amount + right.amount)

    def __sub__(self, other: Any) -> 'AssetAmount':
        try:
            left, right = AssetMath.as_asset_amounts(self, other)
        except AssetTypeError:
            return NotImplemented
        return AssetAmount(asset=left.asset, amount=left.amount - right.amount)

    def __mul__(self, other: Any) -> 'AssetAmount':
        try:
            left, right = AssetMath.as_asset_amounts(self, other)
        except AssetTypeError:
            return NotImplemented
        return AssetAmount(asset=left.asset, amount=left.amount * right.amount)

    def __mod__(self, other: Any) -> 'AssetAmount':
        try:
            left, right = AssetMath.as_asset_amounts(self, other)
        except AssetTypeError:
            return NotImplemented
        return AssetAmount(asset=left.asset, amount=left.amount % right.amount)

    def __truediv__(self, other: Any) -> 'AssetAmount':
        try:
            left, right = AssetMath.as_asset_amounts(self, other)
        except AssetTypeError:
            return NotImplemented
        return AssetAmount(asset=left.asset, amount=left.amount / right.amount)

    def __floordiv__(self, other: Any) -> 'AssetAmount':
        try:
            left, right = AssetMath.as_asset_amounts(self, other)
        except AssetTypeError:
            return NotImplemented
        return AssetAmount(asset=left.asset, amount=left.amount // right.amount)

    def __pow__(self, other: Any) -> 'AssetAmount':
        try:
            left, right = AssetMath.as_asset_amounts(self, other)
        except AssetTypeError:
            return NotImplemented
        return AssetAmount(asset=left.asset, amount=left.amount ** right.amount)

    def __neg__(self) -> 'AssetAmount':
        asset_amount = self._as_asset_amount()
        return AssetAmount(asset=asset_amount.asset, amount=(-asset_amount.amount))

    def __abs__(self) -> 'AssetAmount':
        asset_amount = self._as_asset_amount()
        return AssetAmount(asset=asset_amount.asset, amount=abs(asset_amount.amount))

    def __pos__(self) -> 'AssetAmount':
        return abs(self)


class AssetBalance(dict, BalanceMath):
    '''A set of unlike values destined for a single purpose'''

    def __setitem__(self, asset: Asset, asset_amount: Union['AssetAmount', D]) -> None:
        if type(asset_amount) is D:
            asset_amount = asset.make_amount(asset_amount)
        asset = _auto_asset_arg(asset)

        assert(asset == asset_amount.asset)
        super().__setitem__(asset, asset_amount)

    def __getitem__(self, asset: Asset) -> 'AssetAmount':
        asset = _auto_asset_arg(asset)
        return super().__getitem__(asset)

    def _as_asset_balance(self) -> 'AssetBalance':
        return self


class AssetAmount(AssetMath):
    '''So just, what is 5 undefined?  Javascript?'''

    __slots__ = ("asset", "amount")

    def __init__(self, *, asset: Asset, amount: _DCoercibles):
        asset = _auto_asset_arg(asset)
        amount = _lossless_decimal(amount)
        self.asset = asset
        self.amount = amount

    def make_entry(self, account: "Account"):
        return AccountEntry(account=account,
                            asset=self.asset,
                            amount=self.amount)

    def __repr__(self):
        return "AssetAmount(asset={!r}, amount={!r})".format(self.asset,
                                                             self.amount)

    def __str__(self):
        return "<AssetAmount {!s} {!s}>".format(self.amount, self.asset.symbol)

    def _as_asset_amount(self):
        return self


class ExchangeRate:
    '''How many apples are in an orange?'''

    __slots__ = ("rate", "pair")

    def __init__(self, *, asset_pair=None, base=None, quote=None, rate):
        if asset_pair is None:
            base = _auto_asset_arg(base)
            quote = _auto_asset_arg(quote)
            # TODO move this coercion into Pair via nested dict
            asset_pair = AssetPair.get(base.symbol + quote.symbol)
        rate = _lossless_decimal(rate)
        self.pair = asset_pair
        self.rate = rate

    def __getattr__(self, name):
        if name == "base":
            return self.pair.base
        elif name == "quote":
            return self.pair.quote
        return object.__getattribute__(self, name)

    def __truediv__(self, other):
        other = _lossless_decimal(other)
        return ExchangeRate(base=self.base,
                            quote=self.quote,
                            rate=self.rate / other)

    def __rtruediv__(self, other):
        if not issubclass(other.__class__, AssetMath):
            other = _lossless_decimal(other)                        
            return ExchangeRate(base=self.base,
                                quote=self.quote,
                                rate=other / self.rate)
        if other.asset is self.base:
            if FLEXIBLE_QUOTE_MATH:
                return AssetAmount(amount=other.amount * self.rate,
                                   asset=self.quote)
            else:
                raise AssetTypeError("Cannot divide {} by {} per {}."
                                     " set FLEXIBLE_QUOTE_MATH to True to"
                                     " enable automatic interpretation".format(
                                         other.asset.symbol,
                                         self.quote.symbol,
                                         self.base.symbol))
        if other.asset is self.quote:
            return AssetAmount(amount=other.amount / self.rate,
                               asset=self.base)
        raise AssetTypeError("Incompatible assets {} and {} per {}".format(
            other.asset.symbol,
            self.quote.symbol,
            self.base.symbol))

    def __mul__(self, other):
        if not issubclass(other.__class__, AssetMath):
            other = _lossless_decimal(other)
            return ExchangeRate(base=self.base,
                                quote=self.quote,
                                rate=self.rate * other)
        if other.asset is self.quote:
            if FLEXIBLE_QUOTE_MATH:
                return AssetAmount(amount=other.amount / self.rate,
                                   asset=self.base)
            else:
                raise AssetTypeError("Cannot multiply {} by {} per {}."
                                     " set FLEXIBLE_QUOTE_MATH to True to"
                                     " enable automatic interpretation".format(
                                         other.asset.symbol,
                                         self.quote.symbol,
                                         self.base.symbol))
        if other.asset is self.base:
            return AssetAmount(amount=other.amount * self.rate,
                               asset=self.quote)
        raise AssetTypeError("Incompatible assets {} and {} per {}".format(
            other.asset.symbol,
            self.quote.symbol,
            self.base.symbol))

    def __rmul__(self, other):
        return self.__mul__(other)

    def __repr__(self):
        return "ExchangeRate(value={!r}, base={!r}, quote={!r})".format(self.rate,
                                                                        self.base,
                                                                        self.quote)

    def __str__(self):
        return "<ExchangeRate {!s}{!s} per {!s}>".format(
            self.rate, self.quote.symbol, self.base.symbol)


class Account(AssetMath):
    '''A named bucket from which the taking and giving follows divine ritual'''

    _accounts = {}
    __slots__ = ("name", "asset", "amount")

    @classmethod
    def define(self, *, name: Optional[str] = None,
               asset: Optional[Union[Asset, str]] = None,
               amount: Optional[_DCoercibles] = None,
               asset_amount: Optional[AssetAmount] = None) -> 'Account':
        name = _anonymous_name_if_none(target=self._accounts, name=name)
        if name in self._accounts:
            raise DefinitionError("Account with that name already exists")
        self._accounts[name] = Account(name=name,
                                       asset=asset,
                                       amount=amount,
                                       asset_amount=asset_amount,
                                       _incorrectly=False)
        return self._accounts[name]

    @classmethod
    def get(self, name: str) -> "Account":
        if type(name) is str:
            return self._accounts[name]
        raise TypeError("Key must be a str.  Got {}".format(name))

    @staticmethod
    def accounts_for_assets(asset: Optional[Union[str, Asset]] = None,
                            assets: Optional[Sequence[Asset]] = None):
        if assets is not None:
            if asset is not None:
                raise ValueError(
                    "Give an asset or list of assets but please, not both")
            assets = [_auto_asset_arg(a) for a in assets]
        else:
            assets = [_auto_asset_arg(asset)]
        return [a for a in Account._accounts if a.asset == asset]

    @classmethod
    def _subscriptable_class(self, key):
        return self.get(key)

    def __init__(self, *, name, asset=None, amount=None, asset_amount=None, _incorrectly=True):
        if _incorrectly:
            raise ValueError("Do not create accounts directly.  Call define")
        if asset_amount is None:
            assert(asset) is not None
            if amount is None:
                amount = D(0)
            asset = _auto_asset_arg(asset)
            amount = _lossless_decimal(amount)
            asset_amount = AssetAmount(asset=asset, amount=amount)
        assert(type(asset_amount) is AssetAmount)

        object.__setattr__(self, "name", name)
        object.__setattr__(self, "asset", asset_amount.asset)
        object.__setattr__(self, "amount", asset_amount.amount)

    def _withdraw(self, account_entry):
        with GeneralLedger.transaction_lock:
            assert(type(account_entry) is AccountEntry)
            assert(account_entry.asset == self.asset)
            assert(account_entry.account is self)
            object.__setattr__(
                self, "amount", self.amount - account_entry.amount)
        return self.amount

    def _deposit(self, account_entry):
        with GeneralLedger.transaction_lock:
            assert(type(account_entry) is AccountEntry)
            assert(account_entry.asset == self.asset)
            assert(account_entry.account is self)
            object.__setattr__(
                self, "amount", self.amount + account_entry.amount)
        return self.amount

    def available(self):
        with GeneralLedger.transaction_lock:
            withdrawals = [c.withdrawal.amount for c in GeneralLedger.commitments
                           if c.withdrawal.account is self]
            # do not include deposits as these are expected, not guaranteed,
            # and could be rolled back
            return self.amount - sum(withdrawals)

    def __setattr__(self, name, val):
        if name in ("asset", "amount", "name"):
            raise AttributeError("Use a DoubleEntry in production or object.__setattr__ "
                                 "for testing!")
        object.__setattr__(self, name, val)

    def __str__(self):
        return "<Account {!s} {!s} {!s}>".format(self.name, self.amount, self.asset)

    def __repr__(self):
        return "AssetAcount[{!r}]".format(self.name)

    def __hash__(self):
        return hash((Account.__name__, self.name))

    def _as_asset_amount(self):
        return AssetAmount(asset=self.asset, amount=self.amount)


class AccountEntry(AssetMath):
    '''We give to a bucket or take from a bucket according to that bucket's religion'''

    __slots__ = ("account", "_asset_amount")  # asset, amount via __getattr__

    def __init__(self, *, account, asset_amount=None, asset=None, amount=None):
        if asset_amount is None:
            if asset is not None:
                if type(asset) is str:
                    asset = Asset.get(asset)
            else:
                asset = account.asset

            if type(amount) is int:
                amount = D(amount)
            elif type(amount) is str:
                amount = D(amount)
            asset_amount = AssetAmount(asset=asset, amount=amount)
        else:
            assert(asset is None and amount is None)
        assert(type(account) is Account)
        assert(type(asset_amount) is AssetAmount)
        assert(account.asset == asset_amount.asset)
        object.__setattr__(self, "account", account)
        self._asset_amount = asset_amount

    def __getattr__(self, name):
        if name == "asset":
            return self._asset_amount.asset
        if name == "amount":
            return self._asset_amount.amount
        raise AttributeError("{} has no attribute {}".format(self, name))

    def __setattr__(self, name, val):
        if name in ("account", "amount", "asset"):
            raise AttributeError("This is a bad idea, but use object.__setattr__ "
                                 "for testing if you must")
        object.__setattr__(self, name, val)

    def __str__(self):
        return "<AccountEntry {!s} {!s} {!s}>".format(self.account,
                                                      self._asset_amount.amount,
                                                      self._asset_amount.asset)

    def __repr__(self):
        return "AccountEntry(account={!r}, asset={!r}, amount={!r})".format(self.account,
                                                                            self.asset,
                                                                            self.amount)

    def _as_asset_amount(self):
        return self._asset_amount


class DoubleEntry(AssetMath):
    '''The religion is accounting.  Every take has a give.'''

    __slots__ = ("withdrawal", "deposit")

    def __init__(self, *, withdrawal=None, deposit=None,
                 src=None, dest=None, asset_amount=None,
                 asset=None, amount=None):
        if withdrawal is None:
            # TODO branch for deposit case separately
            assert(deposit is None)
            if type(src) is str:
                src = Account.get(name=src)
            if type(dest) is str:
                dest = Account.get(name=dest)
            assert(type(src) is Account)
            assert(type(dest) is Account)
            if asset_amount is None:
                assert(amount is not None)
                if type(amount) is int:
                    amount = D(amount)
                elif type(amount) is str:
                    amount = D(amount)
                assert(type(amount) is D)
                if asset is not None:
                    if type(asset) is str:
                        asset = Asset.get(asset)
                    assert(type(asset) is Asset)
                else:
                    assert(src.asset == dest.asset)
                    asset = src.asset
                asset_amount = AssetAmount(asset=asset, amount=amount)
            else:
                assert(asset is None and amount is None)
            assert(type(asset_amount) is AssetAmount)
            withdrawal = asset_amount.make_entry(account=src)
            deposit = asset_amount.make_entry(account=dest)
        assert(type(withdrawal) is AccountEntry)
        assert(type(deposit) is AccountEntry)
        assert(deposit.account.asset == withdrawal.account.asset)
        assert(deposit.account is not withdrawal.account)
        assert(deposit.amount == withdrawal.amount)
        self.withdrawal = withdrawal
        self.deposit = deposit

    def __str__(self):
        return "<DoubleEntry {}{} from {} to {}>".format(self.withdrawal.amount,
                                                         self.withdrawal.asset,
                                                         self.withdrawal.account,
                                                         self.deposit.account)

    def __repr__(self):
        return ("DoubleEntry(amount={!r}, asset={!r},"
                " src={!r}, dest={!r})").format(self.withdrawal.amount,
                                                self.withdrawal.asset,
                                                self.withdrawal.account,
                                                self.deposit.account)

    def _as_asset_amount(self):
        return self.withdrawal._asset_amount


class ExchangeEntry:
    '''How apples became oranges without leaking lemmiwinks'''

    __slots__ = ("withdrawal", "deposit", "_exchange_rate")

    def __init__(self, *,
                 rate: ExchangeRate,
                 src: Account,
                 dest: Account,
                 asset_amount: AssetAmount) -> None:
        src = _auto_account_arg(src)
        dest = _auto_account_arg(dest)
        assert(rate.base == src.asset)
        assert(rate.quote == dest.asset)
        if src.asset == asset_amount.asset:
            src_amount = asset_amount
            dest_amount = asset_amount * rate
        else:
            dest_amount = asset_amount
            src_amount = asset_amount / rate
        # TODO the type impedence is real
        withdrawal = AccountEntry(account=src, asset_amount=src_amount)
        deposit = AccountEntry(account=dest, asset_amount=dest_amount)
        self.withdrawal = withdrawal
        self.deposit = deposit
        self._exchange_rate = rate

    def __str__(self):
        return "<ExchangeEntry {}{} from {} as {}{} to {}>".format(self.withdrawal.amount,
                                                                   self.withdrawal.asset,
                                                                   self.withdrawal.account,
                                                                   self.deposit.amount,
                                                                   self.deposit.asset,
                                                                   self.deposit.account)

    def __repr__(self):
        return ("ExchangeEntry(src={!r}, dest={!r},"
                " src_amount={!r}, exchange_rate={!r})").format(self.withdrawal.account,
                                                                self.deposit.account,
                                                                self.withdrawal.amount,
                                                                self._exchange_rate)


class AccountGroup(BalanceMath):
    '''Assets + Equity + Liabilities == D(0)'''

    _groups = {}

    @classmethod
    def define(self, *,
               name: Optional[str] = None,
               accounts: Optional[Sequence[Account]] = None,
               _class: Any = None):
        if _class is None:
            _class = self
        name = _anonymous_name_if_none(target=self._groups, name=name)
        if name in self._groups:
            raise DefinitionError("AccountGroup with that name already exists")
        if accounts is not None:
            accounts = set(accounts)
        else:
            accounts = set()
        for a in accounts:
            assert(type(a) is Account)
        ag = _class(name=name, accounts=accounts, _wrongly=False)
        self._groups[name] = ag
        return ag

    @classmethod
    def get(self, name: str) -> 'AccountGroup':
        return self._groups[name]

    @classmethod
    def _subscriptable_class(self, key: str) -> 'AccountGroup':
        return self.get(key)

    def __init__(self, *,
                 name: str,
                 accounts: AbstractSet[Account],
                 _wrongly: bool = True) -> None:
        if _wrongly:
            raise ValueError(
                "Do not create directly.  Call define (preferably early)")
        self.accounts = accounts
        self.name = name

    def add(self, account: Account) -> None:
        self.accounts.add(account)

    def remove(self, account: Account) -> None:
        self.accounts.remove(account)

    def _as_asset_balance(self) -> AssetBalance:
        with GeneralLedger.transaction_lock:
            ab = AssetBalance()
            for a in self.accounts:
                if a.asset in ab:
                    ab[a.asset] = ab[a.asset] + a
                else:
                    # TODO impedence
                    ab[a.asset] = AssetAmount(asset=a.asset, amount=a.amount)
            return ab

    def balance(self) -> AssetBalance:
        return self._as_asset_balance()

    def __str__(self):
        return "<AccountGroup name={!r}>".format(self.name)

    def __repr__(self):
        return "AccountGroup[{!r}]".format(self.name)


class AutoAccountGroup(AccountGroup):

    @classmethod
    def define(self, **kwargs):
        kwargs["_class"] = self
        return super().define(**kwargs)

    def __init__(self, **kwargs):
        if kwargs.get("_wrongly", True):
            raise ValueError("Call define.  Do not instantiate directly")
        accounts = kwargs.get("accounts", None)
        assets = set()
        if accounts is not None:
            for a in accounts:
                if a.asset in assets:
                    raise ValueError("AutoAccountGroup expects one account"
                                     " per asset.  Duplicate asset detected.")
                assets.add(a.asset)
        super().__init__(**kwargs)
        self._asset_map = {}
        for a in self.accounts:
            self._asset_map[a.asset] = a

        balance = kwargs.get("balance", None)
        if balance is not None:
            for am in balance.values():
                self.add(Account.define(asset_amount=am))

    def account_for_asset(self, asset: Asset) -> Account:
        asset = _auto_asset_arg(asset)
        if asset not in self._asset_map:
            auto_account = Account.define(asset=asset)
            self.accounts.add(auto_account)
            self._asset_map[asset] = auto_account
        return self._asset_map[asset]

    def add(self, account: Account) -> None:
        assert(type(account) is Account)
        assert(account.asset not in self._asset_map)
        self.accounts.add(account)
        self._asset_map[account.asset] = account

    def remove(self, account: Account) -> None:
        assert(type(account) is Account)
        self.accounts.remove(account)
        self._asset_map.pop(account.asset)


_DoubleEntryTypes = TypeVar("_DoubleEntryTypes", DoubleEntry, ExchangeEntry)


class GeneralLedger(BalanceMath):
    '''The lock on all transactions, The enforcer of all that may come to pass'''

    commitments = []
    transaction_lock = threading.RLock()

    def __init__(self):
        raise ValueError(
            "What are you doing?  We only need one ledger.  Add accounts!")

    def require_transaction_context(fun):
        def check_transaction_context(*args, **kwargs):
            if not GeneralLedger.transaction_lock._is_owned():
                raise ValueError(
                    "No active transaction.  `use with GeneralLedger.transaction_lock`")
            return fun(*args, **kwargs)
        return check_transaction_context

    @classmethod
    @require_transaction_context
    def commit(self, double_entry: _DoubleEntryTypes):
        assert(type(double_entry) in (DoubleEntry, ExchangeEntry))
        self.commitments.append(double_entry)

    @classmethod
    @require_transaction_context
    def rollback(self, double_entry: _DoubleEntryTypes):
        assert(type(double_entry) in (DoubleEntry, ExchangeEntry))
        self.commitments.remove(double_entry)

    @classmethod
    @require_transaction_context
    def realize(self, double_entry: _DoubleEntryTypes):
        src = double_entry.withdrawal.account
        dest = double_entry.deposit.account
        src._withdraw(double_entry.withdrawal)
        dest._deposit(double_entry.deposit)
        # Poof!  The double entry has been baked into the actual account values
        if double_entry in self.commitments:
            self.commitments.remove(double_entry)

    @classmethod
    @require_transaction_context
    def unrealize(self, double_entry: _DoubleEntryTypes):
        assert(double_entry not in self.commitments)
        src = double_entry.withdrawal.account
        dest = double_entry.deposit.account
        # just reverse the deposit & withdrawal and it's the same as going backwards
        src._deposit(double_entry.withdrawal)
        dest._withdraw(double_entry.deposit)

    @classmethod
    def balance(self,
                assets: Optional[Sequence[Asset]] = None,
                asset: Optional[Asset] = None):
        with GeneralLedger.transaction_lock:
            if assets is None:
                assets = set()
                if asset is not None:
                    assets.add(asset)
                else:
                    for ac in Account._accounts.values():
                        assets.add(ac.asset)
            else:
                assert(asset is None)

            ab = AssetBalance()
            for a in assets:
                balance = D(0)
                for ac in [ac for ac in Account._accounts.values() if ac.asset == a]:
                    balance += ac.amount
                ab[a] = a.make_amount(amount=balance)
            return ab

    def _as_asset_balance(self):
        return self.balance()
