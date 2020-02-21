# TDD with Django

19 feb. 2020

https://www.obeythetestinggoat.com/

Page numbers refer to the Brazilian 2nd edition

## Download geckodriver.exe

Go to https://github.com/mozilla/geckodriver/releases and download the latest version to a directory in your system path

## Virtualenv

In your `code` (or other top-level) directory, run

```
mkdir virtualenvs
cd virtualenvs
```

`python -m venv superlists`

`source superlists/Scripts/activate`

## Install packages in the virtualenv

`pip install "django>3" selenium`

## Create an empty project directory

In the `code` directory,

`mkdir superlists`

`cd superlists`

## Create a failing test

Create the file `functional_tests.py` in the `superlists` directory

```
from selenium import webdriver

browser = webdriver.Firefox()
browser.get('http://127.0.0.1:8000')

assert 'Django' in browser.title
```

## Run the test

`python functional_tests.py`

## Create a new Django project

In the `superlists` directory, run

`django-admin.exe startproject superlists .`

Because of the dot at the end, the project will be created in the current directory.

## Save the installed packages

`pip freeze > requirements.txt`

## Create a `.gitignore`

In the `superlists` directory, create the file `.gitignore`

```
# Emacs

*~
\#*
.\#*


# Testing

geckodriver*


# Python

__pycache__/


# Django

db.sqlite3
mysecrets.py


# Tab completion Shortcuts

runserver
makemigrations
migrate
```

## Start the development server and test again

In one terminal, run in the top-level `superlists` directory:

`python manage.py runserver`

and in another (remember to activate the virtualenv),

`python functional_tests.py`

## Create a Git repo and push

```
git init
git add .
git commit -m "empty project"
git remote add origin https://github.com/heitorchang/superlists.git
git push -u origin master
```
