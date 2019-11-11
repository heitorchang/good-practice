# Button styles

By default, `active` (button click) is not included. The correct order in `tailwind.config.js` is:

```
...
variants: {
  backgroundColor: ['responsive', 'hover', 'focus', 'active']
},
...
```

because otherwise `hover` would defeat `active` when the button is clicked.

To combine:

`md:hover:bg-green-500`
