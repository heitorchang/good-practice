# Django 2.2.2

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

For `models.ForeignKey`, use `appname_class-plural` for `related_name`. For example, `myapp_pages`. [(Reference)](http://martinbrochhaus.com/related-names.html)

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

There is a `corsheaders` app that should be added at the beginning of `INSTALLED_APPS`. Also, add the following line:

```
CORS_ORIGIN_ALLOW_ALL = True
```

CSRF may be exempted for certain views:

```
from django.views.decorators.csrf import csrf_exempt

@csrf_exempt
def mylist(request):
```