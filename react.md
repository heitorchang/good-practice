```
npx create-react-app APPNAME
cd APPNAME
npm start
```

`npm run build` for production

`npm test` to start the test runner

`npm run eject` copies build dependencies, config and scripts to the `app` directory. This action cannot be undone.

To run existing projects

```
npm install
npm start
```

## [Directory structure](https://facebook.github.io/create-react-app/docs/folder-structure)

`public/index.html` (template) and `src/index.js` must both exist with these exact filenames

## [Installing a Dependency](https://facebook.github.io/create-react-app/docs/installing-a-dependency)

For example, `npm install --save react-router-dom`

## Importing

```
// works if A has a default export. SOMENAME can be anything
import SOMENAME from './A'
```

```
// A must have the named export EXACTNAME
import { EXACTNAME } from './A'
```

A module can only have one default export, but any number of named exports.

Curly braces are used to import non-default exports.