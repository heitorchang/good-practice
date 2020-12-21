# Flasky

Book by Miguel Grinberg: Flask Web Development

## Basic app

```
from flask import Flask
app = Flask(__name__)

@app.route('/')
def index():
    return 'Hello world'


@app.route('/user/<name>')
def user(name):
    return 'Hello, {}'.format(name)


if __name__ == "__main__":
    app.run(debug=True)

    # run with 'python hello.py'
```

## Request object

`from flask import request`

## Context globals

`current_app`: app instance

`g`: temporary storage

`request`

`session`: user session

```
from hello import app
from flask import current_app
current_app.name
# fails
```

```
app_ctx = app.app_context()
app_ctx.push()
current_app.name
app_ctx.pop()
```

## Request object

`form`

`args`: query string

`values`: combination of form and args

`cookies`

`headers`

`files`

`get_data()`

`get_json()`

`blueprint`

`endpoint`

`method`

`scheme`

`is_secure()`

`host`

`path`

`query_string`

`full_path`

`url`

`base_url`

`remote_addr`

`environ`

