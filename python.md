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