# Using with Gatsby

gatsby new PROJECT_NAME
cd PROJECT_NAME

npm install -D gatsby-plugin-postcss tailwindcss@latest postcss@latest autoprefixer@latest

npx tailwindcss init -p

Edit purge in tailwind.config.js:

purge: ['./src/**/*.{js,jsx,ts,tsx}'],


Edit gatsby-config.js

Add `gatsby-plugin-postcss` to plugins: [...]


Create src/styles/global.css and add:

@tailwind base;
@tailwind components;
@tailwind utilities;


Create ./gatsby-browser.js:

import './src/styles/global.css';


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
