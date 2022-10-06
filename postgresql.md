# psql

Connecting (default port is 5432, I am using 5433 to leave the default port available for SQL proxies)

`psql -h 127.0.0.1 -p 5433 -U postgres -d database`

Help

`\?` and `\h`

Exiting psql

`\q`

In Windows, `~/.psqlrc` is

`c:/Users/USERNAME/AppData/Roaming/postgresql/psqlrc.conf`


## psql REPL

Save password instead of typing it interactively

`export PGPASSWORD=secret`

Create a new database and user

`create database artofpsql;`
`create user dimitri with encrypted password 'mysecret';`
`grant all privileges on database artofpsql to dimitri;`

Run a file

`psql -f filename.sql`


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

select table_name from information_schema.tables where table_schema = 'public' order by table_name;

`\d`

`\d "Tablename"`

See column names

select column_name from information_schema.columns
where table_schema = 'public'
and table_name = 'mytable'
order by column_name;


## Users

peer authentication refers to the operating system's current user

To alter a login's password:

enter the `psql` command-line tool

`alter user tina with encrypted password 'mobliz';`


## CRUD

insert into users (name, age) values
('tina', 23),
('joe', 33);

select * from users where name like '%tina%';

*Hint*: before executing an UPDATE or DELETE, write a SELECT statement first to ensure the returned rows are exactly the rows to be changed.

update users set age = 25 where name = 'tina';

delete from users where name = 'joe';


## Greatest-n-per-group

Example: There may be multiple model simulations during a single day (multiple rows). The "updated" column is the time when the simulation was run. We wish to find the newest simulation run for a given model and 'created' date:

            select * from (
	    select row_number() over (partition by hsfe.model_id order by model_id desc, created desc, updated desc) as part_row,
	    hsfe.model_id, m.display_name as modelo, m.vies, m.parent_folder, created as ultima,
            jsonb_array_length(hsfe.variables->'stations'->0->'data'->'ena') as comprimento
            from hydric_stations_fcst_ena hsfe
	    join model m using(model_id)
            where created >= %s::date - interval '8 days'
            and created <= %s::date
            and m.custom_forecast is true
            group by hsfe.model_id, m.display_name, m.vies, m.parent_folder, created, updated, m.sort_order, comprimento
            order by m.sort_order, ultima desc) as cte
	    where part_row = 1;

## Adding column with default value

alter table MY_TABLE add column NEW_COLUMN TYPE default MY_DEFAULT;


## Delete using a CTE

-- Check rows with a SELECT first
with cte_to_delete as (
  select game_id from mygame
  join mygame_genre mgg using(game_genre_id)
  where mgg.name = 'RPG'
  and price < 70
)
select * from mygame
where game_id in (select ctd.game_id from cte_to_delete ctd)
;

Then replace the SELECT line with a DELETE from
Note: ctd is added to avoid a "correlated subquery" reference that can result in DELETE affecting extra rows


## Show row count for all tables

select table_schema, table_name, (xpath('/row/cnt/text()', xml_count))[1]::text::int as row_count
from (select table_name, table_schema, query_to_xml(format(
  'select count(*) as cnt from %I.%I', table_schema, table_name), false, true, '') as xml_count
  from information_schema.tables
  where table_schema = 'public') t
order by row_count desc, table_name;


## enums

\dT
select unnest(enum_range(null::MY_ENUM))::text as e order by e;

## backup database

pg_dump -h localhost -U admin mydb > backups/mydb_2022-01-01.sql

## Save a query result to CSV

\copy (select * from data ...) to '/tmp/data_001.csv' with csv delimiter ';' header