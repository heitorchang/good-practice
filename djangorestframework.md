# Django REST Framework notes

## virtualenv

`python -m venv venvrestframeworknotes`

`source venvrestframeworknotes/Scripts/activate`

## Python packages

Most likely you will need to get through CORS and CSRF restrictions

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
# react 
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

# emacs
\#*
.\#*

# python
__pycache__

# django
secrets.py
db.sqlite3
```

## Generate the server (back-end) and the client (front-end)

`django-admin.exe startproject server`

`npx create-react-app client`

## Django settings

Add to `settings.py`:

```
# Save SECRET_KEY to secrets.py (hidden through .gitignore)
from .secrets import SECRET_KEY 

...

INSTALLED_APPS = [
    'corsheaders',  # at the top
    'django.contrib...
    ...
    'rest_framework',
    'rest_registration',
    'MYAPP',   
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

WSGI_APPLICATION = 'PROJECTNAME.wsgi.application'

REST_FRAMEWORK = {
    'DEFAULT_AUTHENTICATION_CLASSES': (
        # 'rest_framework.authentication.TokenAuthentication',
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

`python manage.py startapp APPNAME`

### Models

```
class TodoItem(models.Model):
    owner = models.ForeignKey('auth.User', related_name='appname_todoItems', on_delete=models.CASCADE)
    description = models.CharField(max_length=100)

    class Meta:
        ordering = ('description',)
```

`python manage.py makemigrations`
`python manage.py migrate`

### Serializers

```
from rest_framework import serializers
from django.contrib.auth.models import User

from .models import TodoItem


class TodoItemSerializer(serializers.ModelSerializer):
    owner = serializers.ReadOnlyField(source='owner.username')
    
    class Meta:
        model = TodoITem
        fields = ('id', 'owner', 'description')
```

### Permissions

```
from rest_framework import permissions


class IsOwner(permissions.BasePermission):
    """
    Allow only the owner to read/update/delete
    """

    def has_object_permission(self, request, view, obj):
        return obj.owner == request.user
```

### Views

```
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

### App `urls.py`

```
from django.urls import path
from . import views

urlpatterns = [
    path('todoitems/', views.TodoItemList.as_view()),
    path('todoitems/<int:pk>/', views.TodoItemDetail.as_view()),
]
```

### Project `urls.py`

Edit `server/server/urls.py` and add

```
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
]

```

## Running the server

`python manage.py runserver`
