# Hello World

The layout should be:

```
/projects/project-name
|
|-- app/
|   +-- __init__.py
|   +-- db.py
|   +-- templates/
|   |   +-- base.html
|   |
|   +-- static/ 
|
+-- tests/
+-- venv/
```

1) `mkdir helloworld`
2) `cd helloworld`
3) `python -m venv venv`
4) `source venv/scripts/activate`
5) `pip install flask`
6) `mkdir app`
7) `cd app`
8) Create `__init__.py` in the `app` directory

```
# app/__init__.py

import os

from flask import Flask


def create_app():
    app = Flask(__name__, instance_relative_config=True)
    app.config.from_mapping(
        SECRET_KEY='dev',
        DATABASE=os.path.join(app.instance_path, 'db.sqlite'),
    )

    try:
        os.makedirs(app.instance_path)
    except OSError:
        pass

    @app.route('/hello')
    def hello():
        return "Hello World"

    return app
```

9) `export FLASK_APP=app`
10) `export FLASK_ENV=development`
11) `flask run`
