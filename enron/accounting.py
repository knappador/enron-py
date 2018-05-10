from typing import Callable
import threading

from enron.typevars import _DoubleEntryTypes


class Accountant:
    '''The lock on all transactions, The enforcer of all that may come to pass'''

    commitments = []
    transaction_lock = threading.RLock()

    def __init__(self):
        raise ValueError(
            "What are you doing?  We only need one ledger.  Add accounts!")

    def require_transaction_context(fun: Callable) -> Callable:
        def check_transaction_context(*args, **kwargs):
            if not Accountant.transaction_lock._is_owned():
                raise ValueError(
                    "No active transaction.  `use with GeneralLedger.transaction_lock`")
            return fun(*args, **kwargs)
        return check_transaction_context

    @classmethod
    @require_transaction_context
    def commit(cls, double_entry: _DoubleEntryTypes):
        # assert type(double_entry) in (DoubleEntry, ExchangeEntry)
        cls.commitments.append(double_entry)

    @classmethod
    @require_transaction_context
    def rollback(cls, double_entry: _DoubleEntryTypes):
        # assert type(double_entry) in (DoubleEntry, ExchangeEntry)
        cls.commitments.remove(double_entry)

    @classmethod
    @require_transaction_context
    def realize(cls, double_entry: _DoubleEntryTypes):
        src = double_entry.withdrawal.account
        dest = double_entry.deposit.account
        src._withdraw(double_entry.withdrawal)
        dest._deposit(double_entry.deposit)
        # Poof!  The double entry has been baked into the actual account values
        if double_entry in cls.commitments:
            cls.commitments.remove(double_entry)

    @classmethod
    @require_transaction_context
    def unrealize(cls, double_entry: _DoubleEntryTypes):
        assert double_entry not in cls.commitments
        src = double_entry.withdrawal.account
        dest = double_entry.deposit.account
        # just reverse the deposit & withdrawal and it's the same as going backwards
        src._deposit(double_entry.withdrawal)
        dest._withdraw(double_entry.deposit)
