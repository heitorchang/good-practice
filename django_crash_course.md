# Django Crash Course

Based on the Feldroys' book

Install Conda, Git, and PostgreSQL

## Set up conda

`conda create -n PROJECTNAME python=3.8`

`conda activate PROJECTNAME`

`conda install -c conda-forge cookiecutter`

`cd /PATH/TO/PROJECTS`

## Use the django-crash-starter cookiecutter template

`cookiecutter gh:feldroy/django-crash-starter`

Re-download if asked

Add `psycopg2` to `requirements/base.txt`

Run `pip install -r requirements/local.txt`

## Create a PostgreSQL database

`createdb -U postgres PROJECTNAME`

## Adjust settings

Create a file `mysecrets.py` in `config/settings`.

In `mysecrets.py`, add

```
LOCAL_DATABASE_URL = "postgres://postgres:PASSWORD@localhost:5432/PROJECTNAME"
```

In `config/settings/base.py`, add

```
from mysecrets import LOCAL_DATABASE_URL

# ...
    "default": env.db(
        "DATABASE_URL", default=LOCAL_DATABASE_URL
    )
```

