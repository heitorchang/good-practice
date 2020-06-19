# Common Postgres tasks

## DSN

```
HOST = "127.0.0.1"
PORT = "5432"
DATABASE = "cemaden"
USER = "heitor"
PASSWORD = "supersecret"

DSN = f"host={HOST} port={PORT} dbname={DATABASE} user={USER} password={PASSWORD}"
```

## Connecting

Columns are accessed by index (integer):

```
with psycopg2.connect(DSN) as conn:
    with conn.cursor() as cur:
        # do something
```

Columns are accessed as dictionary keys:

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
INSERT INTO station (name, city, state, lat, lon, provider_acronym, basin, ref, provider_id)
VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s)
ON CONFLICT (provider_id, ref) DO NOTHING
""", (
    s['name'],
    s['city'],
    s['state'],
    s['lat'],
    s['lon'],
    s['provider_acronym'],
    s['basin'],
    s['ref'],
    s['provider_id']
))
```

## Insert many

```
    from psycopg2.extras import execute_batch

    sql = """
        INSERT INTO rainfall (station, when_measured, mm)
            VALUES (%s, %s, %s)
            ON CONFLICT (station, when_measured) DO
                UPDATE SET mm = %s, updated = %s
    """

    rows = [(r.station, r.when_measured, r.mm, r.mm, insert_datetime_now) for r in py_obj_list]

    # inside with block
    
    execute_batch(
        cur,
        sql,
        rows,
        page_size=PAGE_SIZE
    )
```
