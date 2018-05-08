class AssetTypeError(TypeError):
    pass


class AmountTypeError(TypeError):
    def __init__(self, *args, **kwargs):
        super().__init__("Account amounts must be Decimal or losslessly coercible")


class DefinitionError(ValueError):
    pass
