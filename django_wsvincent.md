# Django for Professionals

Ch. 3

Using Ubuntu 20

mkdir books
cd books
python3 -m venv venvbooks
source venvbooks/bin/activate
pip install django psycopg2-binary

* Add venvbooks/ to .gitignore

django-admin startproject config .

* Hide the SECRET_KEY in mysecrets.py file and put DEBUG there
* add __pycache__/ to .gitignore

* Create a new role for the Postgres database and allow it to login
* Create a new database for this role

Edit config/settings.py

```
DATABASES = {
    'default': {
        'ENGINE': 'django.db.backends.postgresql',        
        'NAME': 'books',
        'USER': 'books',
        'PASSWORD': 'mybooks',
        'HOST': '127.0.0.1',
        'PORT': 5433
    }
}
```

* Create a custom user

python manage.py startapp accounts

```
# accounts/models.py

from django.contrib.auth.models import AbstractUser
from django.db import models


class CustomUser(AbstractUser):
    pass
```

* In settings.py, add accounts to INSTALLED_APPS and add `AUTH_USER_MODEL = 'accounts.CustomUser'` at the end.


Ch. 6

STATICFILES_DIRS defines the location of static files in local development.

STATIC_ROOT is the location of static files for production (collectstatic will compile all files across the project)

```
STATIC_URL = '/static/'
STATICFILES_DIRS = (str(BASE_DIR.joinpath('static')),)
STATIC_ROOT = str(BASE_DIR.joinpath('staticfiles'))
STATICFILES_FINDERS = [
    "django.contrib.staticfiles.finders.FileSystemFinder",
    "django.contrib.staticfiles.finders.AppDirectoriesFinder"
]
```

progress: Ch. 6 images 