from js2py.pyjs import *

import sys
import docopt_purs

run0 = docopt_purs.var.to_python().PS['Docopt.FFI'].run
def run(text, **kwargs):
    opts = {
        'dontExit': True,
        'argv': sys.argv[1:],
        'env': {}
    }
    opts.update(kwargs)
    try:
        run0(
            text,
            opts
        )()
    except Exception as e:
        if not kwargs.get('dontExit', False):
            print(e)
            exit(1)
        else:
            raise e


args=run(
    """
    Usage: foo bar
    """
)
