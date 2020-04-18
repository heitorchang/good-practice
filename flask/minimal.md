# Minimal app

```python
# hello.py

from flask import Flask

app = Flask(__name__)


@app.route('/')
def hello():
    return "Minimal Hello World"


if __name__ == '__main__':
    app.run()
```

Run with `python hello.py`