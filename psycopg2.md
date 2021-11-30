# Common Postgres tasks

## DSN

```
HOST = "127.0.0.1"
PORT = "5432"
DATABASE = "mydatabase"
USER = "heitor"
PASSWORD = "supersecret"

DSN = f"host={HOST} port={PORT} dbname={DATABASE} user={USER} password={PASSWORD}"
```

## Connecting

To access columns by index (integer):

```
with psycopg2.connect(DSN) as conn:
    with conn.cursor() as cur:
        # do something
```

To access columns as dictionary keys:

```
with psycopg2.connect(DSN) as conn:
    with conn.cursor(cursor_factory=RealDictCursor) as cur:
```

## Fetch one

```
    count_stations_sql = "SELECT count(*) FROM stations"

    cur.execute(count_stations_sql)
    result = cur.fetchone()
    count = result[0]
```


## Fetch all

```
    stations_sql = "SELECT id FROM stations WHERE id >= %s AND id <= %s ORDER BY id"

    cur.execute(stations_sql, (start_id, end_id))
    result = cur.fetchall()

    return [row[0] for row in result]
```

```
    cur.execute(f"""
    SELECT id, name, latitude, longitude, city, state
    FROM stations
    """)
    return cur.fetchall()
```

## Insert one

```
cur.execute("""
INSERT INTO station (name, city, state, lat, lon)
VALUES (%s, %s, %s, %s, %s)
ON CONFLICT (name, city, state) DO NOTHING
""", (
    s['name'],
    s['city'],
    s['state'],
    s['lat'],
    s['lon'],
))
```

## Insert many

```
    from psycopg2.extras import execute_batch

    sql = """
        INSERT INTO rainfall (station, observation_time, mm)
            VALUES (%s, %s, %s)
            ON CONFLICT (station, observation_time) DO
                UPDATE SET mm = %s, updated = %s
    """

    rows = [(r.station, r.observation_time, r.mm, r.mm, insert_datetime_now) for r in py_obj_list]

    # inside with block
    
    execute_batch(
        cur,
        sql,
        rows,
        page_size=PAGE_SIZE
    )
```

## Using IN operator

instead of passing a list, pass a tuple to replace the %s in an "where x IN %s" clause.