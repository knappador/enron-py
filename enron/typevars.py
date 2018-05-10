from decimal import Decimal as D
from typing import TypeVar


_DoubleEntryTypes = TypeVar("_DoubleEntryTypes", "enron.DoubleEntry", "enron.ExchangeEntry")
_DCoercibles = TypeVar("_DCoercibles", D, str, int)
