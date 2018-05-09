class EnronError(Exception):
    '''Base exception type for easy branching'''
    pass


class AssetTypeError(EnronError, TypeError):
    '''Used for Asset mismatch cases such as adding USD to EUR'''
    pass


class AmountTypeError(EnronError, TypeError):
    '''Passed an argument which couldn't be coerced losslessly to Decimal'''

    def __init__(self, *args, **kwargs):
        super().__init__("Account amounts must be Decimal or losslessly coercible",
                         *args, **kwargs)


class DefinitionError(EnronError, ValueError):
    '''Either you didn't define something or passed an invalid symbol or name.'''
    pass
