# Django Crash Course

Based on the Feldroys' book

Install Conda, Git, and PostgreSQL

## Set up conda

`conda create -n PROJECTNAME python=3.8`

`conda activate PROJECTNAME`

Run `conda activate PROJECTNAME` every time a new shell is started

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

Add `*secrets.py` to `.gitignore`

Create a file `mysecrets.py` in `config/settings`.

In `mysecrets.py`, add

```
LOCAL_DATABASE_URL = "postgres://postgres:PASSWORD@localhost:5432/PROJECTNAME"
```

In `config/settings/base.py`, add

```
from .mysecrets import LOCAL_DATABASE_URL

# ... lines skipped ...

    "default": env.db(
        "DATABASE_URL", default=LOCAL_DATABASE_URL
    )
```

Test the setup

`python manage.py runserver`

## Running automated tests

Add `disable_warnings = already-imported` to `.coveragerc` in the project's root directory

Add `*.html` to the `omit = ` option in `.coveragerc`

(Skip to Chapter 25)

To test:

`coverage run -m pytest`

Run the test suites before generating a report or HTML

`coverage report` tells you how many statements are being tested

`coverage html` generates a report in `htmlcov/index.html`

## Creating a new app

(Chapter 27)

In the project's root directory, run

`python manage.py startapp APPNAME`

then move it into the inner `PROJECTNAME` directory

`mv APPNAME PROJECTNAME/`

Change the `name` in `PROJECTNAME/APPNAME/apps.py` to

`name = 'PROJECTNAME.APPNAME'`

Add it to `LOCAL_APPS` in `config/settings/base.py`

```
LOCAL_APPS = (
    # users
    'PROJECTNAME.APPNAME.apps.APPNAMEConfig',
)
```

## App models

```
from django.conf import settings
from django.db import models

from autoslug import AutoSlugField

from model_utils.models import TimeStampedModel


class OBJECT(TimeStampedModel):
    # from Chapter 49
    creator = models.ForeignKey(
                settings.AUTH_USER_MODEL,
                null=True,
                on_delete=models.SET_NULL
              )
              
    name = models.CharField("Human-readable description", max_length=255)

    # add a slug
    slug = AutoSlugField("OBJECT address",
             unique=True, always_update=False, populate_from="name")


    def __str__(self):
        return self.name             
```

Run `python manage.py makemigrations` and `python manage.py migrate`

## Test the app

Run `coverage html` and open `htmlcov/index.html`. Clicking on the filename will show which statements need to be tested

(Chapter 29)

Delete `PROJECTNAME/APPNAME/tests.py`

Create the directory `PROJECTNAME/APPNAME/tests/`

Touch `__init__.py` in this new directory

Create `test_models.py`

```
import pytest

from ..models import OBJECT

pytestmark = pytest.mark.django_db


def test_OBJECT_FUNCTION():
    object = OBJECT.objects.create(...)
```

## Using the built-in admin site

(Chapter 31)

Create a superuser

`python manage.py createsuperuser`

Register the new class in `PROJECTNAME/APPNAME/admin.py`

```
from django.contrib import admin

from .models import OBJECT, OBJECT2


admin.site.register([OBJECT1, OBJECT2])
```

# Class-Based Views

(Chapter 33)

## List View

(Chapter 34)

```
from django.views.generic import ListView, DetailView

from .models import OBJECT


class OBJECTListView(ListView):
    model = OBJECT
```

## Define OBJECT URL Patterns

```
# PROJECTNAME/APPNAME/urls.py

from django.urls import path
from . import views


app_name = "APPNAME"

urlpatterns = [
    path(
        route='',
        view=views.OBJECTListView.as_view(),
        name='list'
    ),
]
```

Include APPNAME/urls.py in the root URLConf

Add to `config/urls.py`

```
# Your stuff: custom urls includes go here
path(
    'APPNAME/',
    include('PROJECTNAME.APPNAME.urls', namespace='APPNAME'),
),
```

## Add the List template

(Chapter 36)

Create the directory 'PROJECTNAME/templates/APPNAME'

In this directory, create 'OBJECT_list.html'

```
{% extends "base.html" %}

{% block title %}APPNAME List{% endblock title %}

{% block content %}
<h2>OBJECT List</h2>

<ul>
  {% for OBJECT in OBJECT_list %}
    <li>{{ OBJECT.name }}</li>
  {% endfor %}
</ul>
{% endblock content %}
```

## Add the Detail view

(Chapter 37)

Add to APPNAME/views.py:

```
class OBJECTDetailView(DetailView):
    model = OBJECT
```

Add the URL path to APPNAME/urls.py

```
path(
    route='<slug:slug>/',
    view=views.OBJECTDetailView.as_view(),
    name='detail'
),

Add a template (Chapter 38)

```
{% extends "base.html" %}

{% block title %}OBJECTS: {{ OBJECT.name }}{% endblock title %}

{% block content %}

<h2>{{ OBJECT.name }}</h2>

<p>
  {{ OBJECT.description }}
</p>
{% endblock content %}
```

## Accessing a many-to-many field in templates

Suppose your models are:

```
class ProductCategory(models.Model):
    name = TextField()

class Product(models.Model):
    name = TextField()
    category = models.ManyToManyField(ProductCategory)
```

In the ProductCategory template, write

```
{% for product in productcategoy.product_set.all %}
  {{ product.name }}
{% endfor %}
```

In the Product template, write

```
{% for category in product.category.all %}
  {{ category.name }}</a>
{% endfor %}
```

## Writing factories for tests

(Chapter 40)

Factory Boy is a third-party library that generates test data

Create `APPNAME/tests/factories.py`

```
import factory
import factory.fuzzy

from ..models import OBJECT


class OBJECTFactory(factory.django.DjangoModelFactory):
    ATTRIBUTE = factory.fuzzy.FuzzyText()


    class Meta:
        model = OBJECT
```

The factory creates objects with autogenerated data

`obj = OBJECTFactory()`

## Testing with the factory

(Chapter 42)

```
def test___str()__():
    obj = OBJECTFactory()
    assert obj.__str__() == obj.name
    assert str(obj) == obj.name

## Country data

Add the package `django-countries==6.0` to `requirements/base.txt` and run `pip install -r requirements/local.txt`

Add `django_countries` to `THIRD_PARTY_APPS` in `settings/base.py`

Since a new field was added, the factory needs to be updated

`country_of_origin = factory.Faker('country_code')

## Object creation by users

(Chapter 46)

`OBJECTCreateView`

Object form

Crispy forms (Chapter 47)

LoginRequiredMixin (Chapter 48)

Add and display the Object creator (Chapters 49 and 50)

The default `User` generated by cookiecutter has

`from PROJECTNAME.users.tests.factories import UserFactory`

In the `OBJECTFactory` definition, add

`creator = factory.SubFactory(UserFactory)`

## Test all the views

(Chapter 53)

`assertContains`

post data

## Test the URLs

(Chapter 54)

## Add and test Update views

(Chapters 55 and 56)
