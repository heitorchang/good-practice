# Django 2.2.2

* Remember to use a custom user (see wsvincent django for professionals)

* Create a virtualenv

```
python -m venv venvdjango

# OR #

python3.9 -m venv venvdjango

source venvdjango/Scripts/activate

pip install --upgrade pip
pip install wheel
pip install Django

django-admin startproject mysite
```

To save required packages, run `pip freeze > requirements.txt`

* hide the secret key in a file `mysecrets.py` and import it in `settings.py`. There is a Python module named `secrets`, so use the name `mysecrets`

```
from .mysecrets import SECRET_KEY
```

* In the new project, create a .gitignore file with:

```
# Emacs

*~
\#*\#
.\#*


# Python

# Byte-compiled / optimized / DLL files
*.pyc
__pycache__/


# Django

mysecrets.py
db.sqlite3

# touch these filenames so that they may be tab-completed
makemigrations
migrate
createsuperuser
runserver

# Virtualenv

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
        ('todolist', '0001_initial'),  # only if this file was already created
    ]

    operations = [
        migrations.RunPython(create_default_tipos),
    ]
```

# related_name

In Django REST Framework's [Authentication tutorial](https://www.django-rest-framework.org/tutorial/4-authentication-and-permissions/), `related_name` must match exactly the name of the Serializer variable in `serializers.py`.

```
# models.py

class Snippet(models.Model):
    created = models.DateTimeField(auto_now_add=True)
    # ...
    owner = models.ForeignKey('auth.User', related_name='snippets_snippets', on_delete=models.CASCADE)


# serializers.py

class UserSerializer(serializers.ModelSerializer):
    snippets_snippets = serializers.PrimaryKeyRelatedField(many=True, queryset=Snippet.objects.all())

    class Meta:
        model = User
        fields = ('id', 'username', 'snippets_snippets')
```

For `models.ForeignKey`, use `appname_class-plural` for `related_name`. For example, `myapp_pages`. [(Reference)](http://martinbrochhaus.com/related-names.html)

```
class Page(models.Model):
    site = models.ForeignKey(Site, related_name="myapp_pages")
```

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

## App `urls.py`

```
from django.urls import path

from . import views

app_name = "bday"

urlpatterns = [
    path('', views.index, name="index"),
    path('all/', views.ListView.as_view(), name="list"),
    path('<int:pk>/', views.DetailView.as_view(), name="detail"),

    path('add', views.PersonCreate.as_view(), name='add'),
    path('<int:pk>/edit', views.PersonUpdate.as_view(), name='update'),
    path('<int:pk>/delete', views.PersonDelete.as_view(), name='delete'),
]
```


## App `models.py`

```
from django.db import models
from django.urls import reverse


class Person(models.Model):
    name = models.CharField(max_length=80)
    bday = models.DateField()
    wish = models.CharField(max_length=80)

    def get_absolute_url(self):
        return reverse('bday:detail', kwargs={'pk': self.pk})
        
    def __str__(self):
        return str(self.id) + " " + self.name
```


## App `views.py`

```
from django.shortcuts import render
from django.views import generic
from django.urls import reverse_lazy
from django.views.generic.edit import CreateView, DeleteView, UpdateView
from .models import Person
from .forms import PersonForm

class PersonCreate(CreateView):
    """It is strongly recommended that you explicitly set all fields that should be edited in the form using the fields attribute.

    https://docs.djangoproject.com/en/2.1/topics/forms/modelforms/#django.forms.ModelForm
    """
    
    model = Person
    form_class = PersonForm


class PersonUpdate(UpdateView):
    model = Person
    form_class = PersonForm


class PersonDelete(DeleteView):
    model = Person
    success_url = reverse_lazy('bday:list')


def index(request):
    return render(request, 'bday/index.html')


class DetailView(generic.DetailView):
    model = Person
    

class ListView(generic.ListView):
    model = Person
```


## App `forms.py`

```
from django.forms import ModelForm, Textarea, DateInput
from .models import Person

class HTMLDateInput(DateInput):
    input_type = "date"
    

class PersonForm(ModelForm):
    class Meta:
        model = Person
        fields = ('name', 'bday', 'wish')
        widgets = {
            'wish': Textarea(attrs={'cols': 80, 'rows': 20}),
            'bday': HTMLDateInput(),  # date format doesn't do anything in Chrome
        }
```


## `base.html` template

```
{% load static %}
<head>
  <link rel="stylesheet" href="{% static "bday/css/style.css" %}">
</head>
<body>
BDay Base

{% block content %}{% endblock %}
<script src="{% static "ui/js/w2ui.min.js" %}"></script>
</body>
```

## `index.html`

```
{% extends "bday/base.html" %}
{% load static %}

{% block content %}
BDay index

<script src="{% static "bday/js/alerts.js" %}"></script>
{% endblock %}
```

### CSRF, CORS

`pip install django-cors-headers`

Add `corsheaders` app at the beginning of `INSTALLED_APPS`.

Add in `MIDDLEWARE`, before `'django.middleware.common.CommonMiddleware'`

```
'corsheaders.middleware.CorsMiddleware',
```

Add the following line after `MIDDLEWARE`:

```
CORS_ORIGIN_ALLOW_ALL = True
```

CSRF may be exempted for certain views:

```
from django.views.decorators.csrf import csrf_exempt

@csrf_exempt
def mylist(request):
```

## `urls.py` redirect

```
from django.views.generic.base import RedirectView

...

path('', RedirectView.as_view(url="NEWURL/", permanent=False), name="siteindex"),
```

## Creating a login page

Your apps need to be placed at the beginning of `INSTALLED_APPS`, otherwise, the built-in templates will be loaded.

`python manage.py startapp accounts`

in settings, install accounts and add:

```
LOGIN_URL = '/accounts/login/'
LOGIN_REDIRECT_URL = '/'
LOGOUT_REDIRECT_URL = '/'
```

at the end

in `PROJECT/urls.py` add:

```
from django.urls import path, include

...

    path('accounts/', include('django.contrib.auth.urls')),
```

copy the login template from https://docs.djangoproject.com/en/2.2/topics/auth/default/ to `accounts/templates/registration/login.html`

```
<!DOCTYPE html>

<html lang="en-us" >
<head>
<title>Log in | Django site admin</title>
<link rel="stylesheet" type="text/css" href="/static/admin/css/base.css">
<link rel="stylesheet" type="text/css" href="/static/admin/css/login.css">

    <meta name="viewport" content="user-scalable=no, width=device-width, initial-scale=1.0, maximum-scale=1.0">
    <link rel="stylesheet" type="text/css" href="/static/admin/css/responsive.css">
    
</head>

<body>

  <div id="container">

    
    <!-- Header -->
    <div id="header">
        <div id="branding">
            <h1 id="site-name"><a href="/">Login</a></h1>
        </div>
    </div>
	
    <div id="content-main">
        <center>
{% if form.errors %}
<p>Your username and password didn't match. Please try again.</p>
{% endif %}

{% if next %}
    {% if user.is_authenticated %}
    <p>Your account doesn't have access to this page. To proceed,
    please login with an account that has access.</p>
    {% endif %}
{% endif %}

<form method="post" action="{% url 'login' %}">
{% csrf_token %}

<table>
<tr>
    <td>{{ form.username.label_tag }}</td>
    <td>{{ form.username }}</td>
</tr>
<tr>
    <td>{{ form.password.label_tag }}</td>
    <td>{{ form.password }}</td>
</tr>
</table>

<input type="submit" value="Login">
<input type="hidden" name="next" value="{{ next }}">
</form>

{# Assumes you setup the password_reset view in your URLconf #}
<p><a href="{% url 'password_reset' %}">Lost password?</a></p>

</center>
</div>
</div>
</body>
</html>
```

`password_change_form.html` and `password_change_done.html` should also be saved in `/accounts/templates/registration/` (find in Django source /contrib/admin/templates/registration/)

## Timezone

When using a `DateTimeField` in a model's `__str__`, it must be converted to the local timezone manually.

```
from django.utils import timezone

def format_time(t):
    return timezone.localtime(t).strftime("%d/%m/%Y %H:%M")
```

# Test coverage

Using `cookiecutter gh:feldroy/django-crash-startee`

Run `coverage run -m pytest`

Then `coverage report`

# Simple tests

Test response status codes

```
from django.test import SimpleTestCase

class SimpleTests(SimpleTestCase):
    def test_home_page_status_code(self):
        response = self.client.get('/')
        self.assertEqual(response.status_code, 200)
```

# Permissions to view restricted pages

According to [Paulo](https://stackoverflow.com/questions/33086777/set-user-permissions-for-specific-views-in-django), groups may be used instead of permissions (which are linked to models).

Place group membership tests in a dedicated file (such as `permissions/permtests.py`) and then use a decorator to apply the test:

```
def must_be_supervisor(user):
    return user.groups.filter(name='Supervisors').exists()

@user_passes_test(must_be_supervisor)
def quarter_report(request):
    pass
```

# Template tags to conditionally show links on homepage

Create `APP_DIR/templatetags/user_tags.py`:

```
from django import template


register = template.Library()

@register.filter(name="tem_grupo")
def tem_grupo(user, group_name):
    return user.groups.filter(name=group_name).exists()
```

In the template:

```
{% load user_tags %}

{% if request.user|tem_grupo:"mapas" %}
<a href="/mapas/">Mapas</a>
{% endif %}

{% if request.user.is_superuser %}
<a href="/admin/">Admin</a>
{% endif %}
```

# Testing at class level

(Django for Professionals Ch. 5)
`setUpTestData()` allows the creation of initial data at the class level.