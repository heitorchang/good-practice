# Experimenting with Vue 3 alpha (vue-next)

*2 Apr 2020*

First, install Node.js

## Install vue-next dependencies

Create a project directory and run

```
git clone https://github.com/vuejs/vue-next-webpack-preview
cd PROJECT_DIR
npm i
```

Remove the `src/` directory and make a new empty `src/` directory

## src/ files

**App.vue**

```
<template>
    <button @click="inc">Inc</button>
    <span>Button clicked {{ count }} times.</span>
</template>

<script>
    import { ref } from "vue";
    export default {
      setup () {
        const count = ref(0);
        const inc = () => {
          count.value += 1;
        };
        return {
          count,
          inc
        }
      }
    }; 
</script>
```

**main.js**

```
import { createApp } from "vue";
import App from './App.vue';

createApp(App).mount("#app");
```

## Running the dev server

`npm run dev`

## Deploying

`npm run build`

Create a simple `index.html` file in `dist/`

```
<div id="app"></div>
<script src="main.js"></script>
```
