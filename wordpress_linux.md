# WordPress on Linux

## Local setup

### MySQL

To access the local MySQL server, unzip phpMyAdmin and run

`php -S 127.0.0.1:9999`

See the file `mysql-ubuntu.txt` for instructions on how to set up the root user.

My credentials are stored in `misc/usuarios-local.txt`

#### Creating a new database

It is best to create a new user and an associated database to isolate projects.

Click on "User accounts" and then, "Add user account"

Check the box "Create database with the same name and grant all privileges"

### WordPress installation

Copy `wordpress-VERSION.tar.gz` to your projects (root) directory.

`tar xf wordpress-VERSION.tar.gz`

`mv wordpress/ NEW_PROJECT_NAME/`

`cd NEW_PROJECT_NAME/`

`php -S 127.0.0.1:8000` (running a development server is the easiest way to move forward)