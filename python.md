# Python 3

## Shave diacritic marks

From Fluent Python

```
import unicodedata
import string

def shave_marks(txt):
    """Remove all diacritic marks"""
    norm_txt = unicodedata.normalize('NFD', txt)
    shaved = ''.join(c for c in norm_txt
                     if not unicodedata.combining(c))
    return unicodedata.normalize('NFC', shaved)
```

## Prevent running command-line code when sending to interactive interpreter

def myfunction(x):
    pass

if __name__ == "__main__" and sys.flags.interactive == 0:
    print("command-line mode")
    print(myfunction(sys.argv[1]))

## pip

run first:

python -m pip install --upgrade pip

## traceback

Inside an except block:

traceback.print_exc()  # prints the traceback
traceback.format_exc() # returns a string of the traceback

## JSON

json.dumps(your_obj, indent=2)

## Packages and import

According to https://alex.dzyoba.com/blog/python-import/, both __init__.py and __main__.py should be included to run `python -m module.py`

## Emacs REPL and importing from multiple files

Suppose you have main.py and helper.py (imported by main). If you edit helper, send helper, and then re-send main, the old helper contents will come back.

One possible workaround is to check if a variable, IMPORTED, was set or not:

```
if not locals().get('IMPORTED'):
    print("Importing to main")
    from helper import f
    IMPORTED = True
```

## requests retries

import requests
from requests.adapters import HTTPAdapter, Retry

session = requests.Session()
retries = Retry(total=5, backoff_factor=1, status_forcelist=[502, 503, 504])
session.mount("https://", HTTPAdapter(max_retries=retries))

session.get(...)
session.post(...)

## PyCharm

Type Ctrl + Alt + S to open Settings. Search for "Run file in Python Console" and assign Ctrl + F10 to it (and remove existing keybinding).

## better_exceptions

this package shows the values of function arguments when an exception is raised.
