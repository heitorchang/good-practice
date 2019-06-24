* Create a virtualenv

```
python -m venv venvdjango

source venvdjango/Scripts/activate

pip install Django

django-admin startproject mysite
```

To save required packages, run `pip freeze > requirements.txt`

* hide the secret key in a file `secrets.py` and import it in `settings.py`

```
from .secrets import SECRET_KEY
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


# Django

secrets.py
db.sqlite3

# Virtualenv

env/
venv/
```

[https://docs.djangoproject.com/en/2.1/ref/models/fields/](https://docs.djangoproject.com/en/2.1/ref/models/fields/)

Avoid using null on string-based fields such as CharField and TextField. If a string-based field has null=True, that means it has two possible values for “no data”: NULL, and the empty string. In most cases, it’s redundant to have two possible values for “no data;” the Django convention is to use the empty string, not NULL. One exception is when a CharField has both unique=True and blank=True set. In this situation, null=True is required to avoid unique constraint violations when saving multiple objects with blank values.

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

# Initial data for models

[See Django docs](https://docs.djangoproject.com/en/2.2/howto/initial-data/) and click on "Data migration"

```
python manage.py makemigrations --empty yourappname
```

then write a custom migration

```
from django.db import migrations


def create_default_tipos(apps, schema_editor):
    Tipo = apps.get_model('todolist', 'Tipo')
    pessoal = Tipo(nome="Pessoal")
    pessoal.save()

    profissional = Tipo(nome="Profissional")
    profissional.save()
    

class Migration(migrations.Migration):

    dependencies = [
        ('todolist', '0001_initial'),
    ]

    operations = [
        migrations.RunPython(create_default_tipos),
    ]
```

# related_name

For `models.ForeignKey`, use `appname_class-plural` for `related_name`. [(Reference)](http://martinbrochhaus.com/related-names.html)

[Be careful with related_name](https://docs.djangoproject.com/en/dev/topics/db/models/#be-careful-with-related-name-and-related-query-name)

If you are using related_name or related_query_name on a ForeignKey or ManyToManyField, you must always specify a unique reverse name and query name for the field. This would normally cause a problem in abstract base classes, since the fields on this class are included into each of the child classes

```
from django.db import models

class Base(models.Model):
    m2m = models.ManyToManyField(
        OtherModel,
        related_name="%(app_label)s_%(class)s_related",
        related_query_name="%(app_label)s_%(class)ss",
    )

    class Meta:
        abstract = True

class ChildA(Base):
    pass

class ChildB(Base):
    pass
```