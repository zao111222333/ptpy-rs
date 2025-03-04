# App command interface, juse a prue Python shell.
from .ptpyrs import main

__doc__ = ptpyrs.__doc__
if hasattr(ptpyrs, "__all__"):
    __all__ = ptpyrs.__all__