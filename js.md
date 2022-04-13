# JavaScript (ES6)

```
axios.then(axios.spread((aResponse, bResponse) => {
  ...
  this.a = aResponse.data;
}))
```

will work, but will not if `function(a, b) { ... }` is used.


# Fetch API

```
  fetch('https://example.com/', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    },
    body: JSON.stringify(data)
  })
  .then((response) => response.json())
  .then((data) => console.log(data))
  .catch((error) => console.error(error));
```

the function applied to data will only run when the fetch is completed.

To block execution, use async/await

```
const resp = await fetch('http://127.0.0.1:9000/dummy', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json'
  },
  body: JSON.stringify({ apiToken: "my-token" })
});
const data = await resp.json();
```

Example with separate function

```
async function getData(requestArg) {
  const resp = await fetch(`https://api.mysite.com/${requestArg}`, {
    method: 'GET',
    headers: {
      Authorization: 'Bearer ' + jwtToken,
    },
  });
  return await resp.json();
}

async function atualizarGrafico() {
  const json = await getData(123);
  console.log(json);
}
```

AbortController stops pending fetch requests

// API calls for directory listing
let listingController = null;

async function getListing(path) {
  if (listingController) {
    listingController.abort();
  }

  try {
    listingController = new AbortController();
    const response = await fetch(`https://storage.tempook.com/tokstorage/lista_ordenada/?t=${apiToken}&r=Comercializadora&s=${safeParams}`,
      { signal: listingController.signal });
    const json = await response.json();
    return json;
  }
  catch (e) {
    console.log("getListing: Aborted.");
    return { links: [] };
  }
}

async function processListing(path) {
  const data = await getListing(path);
  if (data.links.length === 0) {
    $("#files").html("Carregando...");
    return;
  }
  // rest of code for valid links
}

processListing('mypath');