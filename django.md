* Create a virtualenv

```
python -m venv venvdjango

source venvdjango/Scripts/activate

pip install Django

django-admin startproject mysite
```

* hide the secret key in a file `secrets.py` and import it in `settings.py`

```
from . import secrets

SECRET_KEY = secrets.SECRET_KEY
```

* In the new project, create a .gitignore file with:

```
# Emacs

*~
\#*\#
.\#*



# Python

# Byte-compiled / optimized / DLL files
__pycache__/
*.py[cod]
*$py.class



# Django

secrets.py
db.sqlite3
```

[https://docs.djangoproject.com/en/2.1/ref/models/fields/](https://docs.djangoproject.com/en/2.1/ref/models/fields/)

Avoid using null on string-based fields such as CharField and TextField. If a string-based field has null=True, that means it has two possible values for “no data”: NULL, and the empty string. In most cases, it’s redundant to have two possible values for “no data;” the Django convention is to use the empty string, not NULL. One exception is when a CharField has both unique=True and blank=True set. In this situation, null=True is required to avoid unique constraint violations when saving multiple objects with blank values.
```

# PostgreSQL

```
DATABASES = {
    'default': {
        'ENGINE': 'django.db.backends.postgresql_psycopg2',
        'NAME': 'hellopost',
        'USER': 'hellopost',
        'PASSWORD': 'secretpass',
        'HOST': '127.0.0.1',
        'PORT': '',  # default port
    }
}
```