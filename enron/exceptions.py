class EnronError(Exception):
    '''Base exception type for easy branching'''
    pass


class AssetTypeError(EnronError, TypeError):
    pass


class AmountTypeError(EnronError, TypeError):
    def __init__(self, *args, **kwargs):
        super().__init__("Account amounts must be Decimal or losslessly coercible")


class DefinitionError(EnronError, ValueError):
    pass
