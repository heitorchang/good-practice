# React (front-end)

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

## Props

In the Tic-tac-toe tutorial, a square on the board has an associated value (id)

```
class Board extends React.Component {
  renderSquare(i) {
    return <Square value={i} />;
  }
}
```

The `value` prop is a parameter (attribute)

```
class Square extends React.Component {
  render() {
    return (
      <button>
        {this.props.value}
      </button>
    );
  }
}
```

Passing props is how information flows from parents to children.

## State

React components can have state by setting `this.state` in their constructors.

## Constructor

You need to always call `super` when defining a class. In React, all component `constructor(props) { }` should start with `super(props)`

## Data

To collect data from multiple children or to have two child components communicate with each other, you need to declare the shared state in their parent component instead.

The parent component can pass the state back down to the children. If the parent has full control over its children, they are called **controlled components**.

## Immutability is preferred

`const squares = this.state.squares.slice();`

`var player = {name: 'John', score: 1}`
`var newPlayer = {...player, score: 2}`

## Function components

Function components are components that only contain a `render` method and don't have their own state.

`function Square(props) { }`

## Material UI

`npx create-react-app my-app --template rmuif`
`npm install @material-ui/core`
`npm install @material-ui/icons`

### Accessing the underlying DOM element

Use a `ref`, then access its `current` property.

```
const buttonRef = React.createRef();
<Button ref={buttonRef} />
const element = ref.current;
```

## React Hooks

```
import React, { useState, useEffect, useRef } from 'react';
```

* Define a state variable `count` and a function `setCount` to alter this state.

`const [count, setCount] = useState(0)`

* Run a block of code on component's mount, update and unmount

```
useEffect(() => {
  document.title = `You clicked ${count} times`;
})
```

To skip an update, pass a state variable in an array as the second argument to `useEffect()`, such as

```
useEffect(() => {
  document.title = `You clicked ${count} times`;
}, [count]);
```

When `count` is unmodified, this effect is skipped.

If an optional `unmount` block should be run, it should be a function returned by the `useEffect` argument:

```
useEffect(() => {
  ChatAPI.subscribeToFriendStatus(props.friend.id, handleStatusChange);

  return () => {
    ChatAPI.unsubscribeFromFriendStatus(props.friend.id, handleStatusChange);
  };
}, [props.friend.id]); // only re-subscribe if props.friend.id changes
```

* A custom hook allows you to reuse some stateful logic between components.

```
import React, { useState, useEffect } from 'react';

function useFriendStatus(friendID) {
  const [isOnline, setIsOnline] = useState(null);

  function handleStatusChange(status) {
    setIsOnline(status.isOnline);
  }

  useEffect(() => {
    ChatAPI.subscribeToFriendStatus(friendID, handleStatusChange);
    return () => {
      ChatAPI.unsubscribeFromFriendStatus(friendID, handleStatusChange);
    };
  });

  return isOnline;
}
```