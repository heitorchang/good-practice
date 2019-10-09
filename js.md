# JavaScript (ES6)

```
axios.then(axios.spread((aResponse, bResponse) => {
  ...
  this.a = aResponse.data;
}))
```

will work, but will not if `function(a, b) { ... }` is used.