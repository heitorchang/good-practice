# FastAPI with uvicorn on Google Cloud Platform

`Dockerfile`

```
FROM python:3.10-slim

ENV PYTHONUNBUFFERED True

ENV APP_HOME /app
WORKDIR $APP_HOME
COPY . ./

RUN pip install --no-cache-dir -r requirements.txt

CMD exec uvicorn main:app --host "0.0.0.0" --port $PORT
```

`.gitignore`

```
*.py[cod]
__pycache__/
dist/
```

`requirements.txt`

```
fastapi==0.73.0
uvicorn==0.17.5
httpx==0.22.0
requests==2.25.1
google-cloud-storage==2.3.0
... and others ...
```

`main.py`

```
from fastapi import FastAPI, Header, HTTPException
from fastapi.middleware.cors import CORSMiddleware

import requests
import httpx

import os


VERSION = "0.1"


app = FastAPI()


origins = [
    "https://tempook.com",
    "https://tempook.com.br",
    "https://www.tempook.com",
    "https://www.tempook.com.br",
    "https://staging.tempook.com",
    "https://staging.tempook.com.br",
    "http://127.0.0.1:27001",
    "http://127.0.0.1:8000",
    "http://127.0.0.1:8001",
    "http://127.0.0.1:8080",
    "http://127.0.0.1:8081",
]

app.add_middleware(
    CORSMiddleware,
    allow_origins=origins,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


def insert_tok_log(desc):
    logger_url = "https://tok-logger-p64zrskf5q-rj.a.run.app/log/"
    requests.post(logger_url, json={
        "token": os.environ.get("TOK_LOGGER_TOKEN", "no token"),
        "app": f"tokapi_demo v.{VERSION}",
        "description": desc
    })


@app.get("/")
async def root():
    return {"message": "TOK API app"}


@app.get("/wod/{model_name}/{lat},{lon}/{wod_vars}")
async def wod(model_name, lat, lon, wod_vars, tok_api_token: str = Header(None)):
    """Retrieve user_id for given TOK-API-Token. Full example downloads forecast from Belgingur."""
    model_name_whitelist = {
        'TOK10': 'brazil-9pt6-3pt2-noda/all',
    }

    bel_model_name = model_name_whitelist.get(model_name)
    if bel_model_name is None:
        raise HTTPException(status_code=400, detail="No model name provided.")

    if 'TOK' in model_name:
        bel_endpoint = "https://wod.belgingur.is/api/v2/data/point/schedule/{bel_model_name}/latlon/{parsed_lat_lon}/vars/{wod_vars}.json"
    else:
        raise HTTPException(status_code=400, detail="Invalid model name.")

    # check if tok_api_token is valid
    if not tok_api_token:
        raise HTTPException(status_code=400, detail="Missing TOK-API-Token.")

    api_token_validation_url = "https://www.tempook.com/api/cliente_id/"
    response = requests.post(api_token_validation_url, json={"api_token": tok_api_token})
    response_json = response.json()
    if 'user_id' in response_json:
        insert_tok_log(f"User {response_json['user_id']}: {model_name} {lat},{lon}")
        return {"user_id": response_json['user_id']}
    else:
        raise HTTPException(status_code=400, detail="Could not validate API token.")

    # bel_username = os.environ.get("TOKAPI_PREV_PONTUAL_BEL_USERNAME")
    # bel_password = os.environ.get("TOKAPI_PREV_PONTUAL_BEL_PASSWORD")
    # bel_get_url = bel_endpoint.format(bel_model_name=bel_model_name, parsed_lat_lon=parsed_lat_lon, wod_vars=wod_vars)
    #
    # async with httpx.AsyncClient() as client:
    #     bel_response = await client.get(bel_get_url, auth=(bel_username, bel_password), timeout=190)
    #     return bel_response.json()
```


## Local testing

`uvicorn main:app --reload`


## Deployment

Use the "continuous deployment" option.

Currently we are connecting GCP to our Bitbucket repos.


## Manual build

To avoid accidentally deploying a buggy release, edit the trigger to run only on a manual invocation.

1. Select the "Triggers" tab, then find the "build trigger" link at the bottom.

2. Click on the "Triggers" link on the left sidebar.

3. Find the right trigger and click on Run.

4. Click on "Show" in the snackbar that appears on the bottom to monitor the build.


## Reverting to an older build

Under the "Revisions" tab, click on Manage Traffic.