# Django REST Framework back-end server

The following steps create a REST API server for todo items. Authentication is handled with JWT.

## virtualenv

`python -m venv venvdrf`

`source venvdrf/Scripts/activate`

## Python packages

Most likely you will need to get past CORS restrictions while in development.

`pip install` the following:

```
django
djangorestframework
django-cors-headers
django-rest-registration
djangorestframework-simplejwt
```

## Project structure

`mkdir PROJECTNAME`

`cd PROJECTNAME`

`git init`

Create the `.gitignore` file:

## .gitignore

```
# emacs
\#*
.\#*

# python
__pycache__

# django
secrets.py
db.sqlite3

# if using react 
node_modules
.pnp
.pnp.js

# testing
coverage

# production
build

# misc
.DS_Store
.env.local
.env.development.local
.env.test.local
.env.production.local

npm-debug.log*
yarn-debug.log*
yarn-error.log*
```

# Generate the server (back-end)

`django-admin.exe startproject server`

## Django settings

Add to `settings.py`:

```python
# server/server/settings.py

# Save SECRET_KEY to secrets.py (hidden through .gitignore)
from .secrets import SECRET_KEY 

...

INSTALLED_APPS = [
    'corsheaders',  # at the top
    'django.contrib...
    ...
    'rest_framework',
    'rest_registration',
]

# Add CORS middleware before CommonMiddleware
MIDDLEWARE = [
    ...
    'corsheaders.middleware.CorsMiddleware',  # before CommonMiddleware
    'django.middleware.common.CommonMiddleware',
    ...
]

CORS_ORIGIN_ALLOW_ALL = True

...

WSGI_APPLICATION = 'server.wsgi.application'

REST_FRAMEWORK = {
    'DEFAULT_AUTHENTICATION_CLASSES': (
        'rest_framework_simplejwt.authentication.JWTAuthentication',        
    )
}

REST_REGISTRATION = {
    'REGISTER_VERIFICATION_ENABLED': False,
    'REGISTER_EMAIL_VERIFICATION_ENABLED': False,
    'RESET_PASSWORD_VERIFICATION_ENABLED': False,
}
```


## Django App

Run `python manage.py startapp APPNAME`

Add the app to `INSTALLED_APPS` in `settings.py`

### Models

```python
# APPNAME/models.py

from django.db import models

class TodoItem(models.Model):
    owner = models.ForeignKey('auth.User', related_name='appname_todoItems', on_delete=models.CASCADE)
    description = models.CharField(max_length=100)

    class Meta:
        ordering = ('description',)
```

Then run `python manage.py makemigrations` and `python manage.py migrate`

### Serializers

```python
# APPNAME/serializers.py

from rest_framework import serializers
from django.contrib.auth.models import User

from .models import TodoItem


class TodoItemSerializer(serializers.ModelSerializer):
    # NOT NEEDED owner = serializers.PrimaryKeyRelatedField(read_only=True, default=serializers.CurrentUserDefault())
    
    class Meta:
        model = TodoItem
        fields = ('id', 'owner', 'description')
        extra_kwargs = {'owner': {'required': False}}  # Allows POSTing from JS
```

### Permissions

```python
# APPNAME/permissions.py

from rest_framework import permissions


class IsOwner(permissions.BasePermission):
    """
    Allow only the owner to read/update/delete
    """

    def has_object_permission(self, request, view, obj):
        return obj.owner == request.user
```

### Views

```python
# APPNAME/views.py

from rest_framework import generics
from constantes.permissions import EDono
from .models import Conta, Orcamento
from .serializers import ContaSerializer, OrcamentoSerializer


class ListaDeContas(generics.ListCreateAPIView):
    serializer_class = ContaSerializer
    permission_classes = (EDono,)

    def get_queryset(self):
        return Conta.objects.filter(usuario=self.request.user)

    def perform_create(self, serializer):
        serializer.save(usuario=self.request.user)


class DetalheDeConta(generics.RetrieveUpdateDestroyAPIView):
    serializer_class = ContaSerializer
    permission_classes = (EDono,)

    def get_queryset(self):
        return Conta.objects.filter(usuario=self.request.user)
```

```python
# Old version

from rest_framework import generics, permissions
from django.contrib.auth.models import User
from .models import TodoItem
from .serializers import TodoItemSerializer
from .permissions import IsOwner


class TodoItemList(generics.ListCreateAPIView):
    serializer_class = TodoItemSerializer
    permission_classes = (permissions.IsAuthenticated,)

    def get_queryset(self):
        return TodoItem.objects.filter(owner=self.request.user)
    
    def perform_create(self, serializer):
        serializer.save(owner=self.request.user)


class TodoItemDetail(generics.RetrieveUpdateDestroyAPIView):
    queryset = TodoItem.objects.all()
    serializer_class = TodoItemSerializer
    permission_classes = (IsOwner,)
```

### Admin (optional)

```python
# APPNAME/admin.py

from django.contrib import admin
from . import models

admin.site.register(models.TodoItem)
```

Run `python manage.py createsuperuser` as an alternative to going through `rest_registration`.

### REST Registration

To create a new user, `POST` (either form data or JSON) the following information to `/accounts/register/`

```
{
  "username": "myname",
  "password": "SupersafePassword123",
  "password_confirm": "SupersafePassword123"
}
```

### App `urls.py`

```python
# APPNAME/urls.py

from django.urls import path
from . import views

app_name = 'todos'

urlpatterns = [
    path('todoitems/', views.TodoItemList.as_view(), name="todoItemList"),
    path('todoitems/<int:pk>/', views.TodoItemDetail.as_view(), name="todoItemDetail"),
]
```

### Project `urls.py`

Edit `server/server/urls.py` and add

```python
# server/server/urls.py

from django.contrib import admin
from django.urls import path, include

from rest_framework_simplejwt.views import (
    TokenObtainPairView,
    TokenRefreshView,
)

urlpatterns = [
    path('admin/', admin.site.urls),
    path('token/', TokenObtainPairView.as_view(), name='token_obtain_pair'),
    path('token/refresh/', TokenRefreshView.as_view(), name='token_refresh'),
    path('accounts/', include('rest_registration.api.urls')),
    path('todos/', include('todos.urls')),
]

```

## Tests

```python
# APPNAME/tests.py

from django.urls import reverse
from rest_framework import status
from rest_framework.test import APITestCase, APIClient
from django.contrib.auth.models import User
from .models import TodoItem


class TodoItemTests(APITestCase):
    def test_create_todoitem(self):
        """Create a new TodoItem"""

        my_username = "tester"
        my_password = "SuperSecret123@"

        print(my_username, my_password)
        
        # Create new user
        url = "/accounts/register/"
        response = self.client.post(url, {"username": my_username, "password": my_password, "password_confirm": my_password}, format='json')
        self.assertEqual(response.status_code, status.HTTP_201_CREATED)

        # Get access token
        url = reverse('token_obtain_pair')
        response = self.client.post(url, {"username": my_username, "password": my_password}, format='json')
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        token = response.data['access']

        client = APIClient()
        client.credentials(HTTP_AUTHORIZATION='Bearer ' + token)
        
        url = reverse('todos:todoItemList')
        data = {'description': 'new todo item'}
        response = client.post(url, data, format='json')
        
        self.assertEqual(response.status_code, status.HTTP_201_CREATED)
        self.assertEqual(TodoItem.objects.count(), 1)
        self.assertEqual(TodoItem.objects.filter(description='new todo item').count(), 1)
```

Run `python manage.py test`

## Starting the server

Run `python manage.py runserver`

Now choose a front-end client (such as [React](https://github.com/heitorchang/good-practice/blob/master/react.md))

## Testing

Use `Postman`, `curl`, `httpie`, etc.

## REST Framework Simple JWT Authentication

`POST` data (form-data) to `/token/` to get the access and refresh tokens

```
"username": "lois"
"password": "rest1233"
```

## REST Registration

`POST` form-data to `/accounts/register/`

```
username
password
password_confirm
```

## Creating an account type

In the `POST` header, send

```
Authorization: Bearer ...Access Token contents...
```

In the `POST` body, send a JSON:

```
{"name": "new acct type",
"equityType": false}
```

## Reading data

`GET` http://localhost:8000/alexie/accounttypes/ with header

```
Authorization: Bearer ...Access Token contents...
```
