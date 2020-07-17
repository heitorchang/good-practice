# psql

Connecting (default port is 5432, I am using 5433 to leave the default port available for SQL proxies)

`psql -h 127.0.0.1 -p 5433 -U postgres -d database`

Help

`\?` and `\h`

Exiting psql

`\q`

In Windows, `~/.psqlrc` is

`c:/Users/USERNAME/AppData/Roaming/postgresql/psqlrc.conf`

## psqlrc

```
\set PROMPT1 '%~%x%# '
\x auto
\set ON_ERROR_STOP on
\set ON_ERROR_ROLLBACK interactive

\pset null <null>
```

## in psql

**Always use semicolons at the end of SQL commands.**

Table names might have to be enclosed in double quotes.

`create database dbname;`

Connect to a database

`\c dbname`

Current directory

`\! pwd`

Change directory

`\cd dirname`

Import file

`\i filename`

List tables, views, sequences

`\d`

`\d "Tablename"`