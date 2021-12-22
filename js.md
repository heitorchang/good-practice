# JavaScript (ES6)

```
axios.then(axios.spread((aResponse, bResponse) => {
  ...
  this.a = aResponse.data;
}))
```

will work, but will not if `function(a, b) { ... }` is used.


# Fetch API

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

the function applied to data will only run when the fetch is completed.

To block execution, use async/await