# Notes

`v-bind` links a data variable or expression to a HTML tag's attribute.

`<img v-bind:src="image">`

```
data: {
  image: './img/01.jpg''
}
```

`v-bind` is optional. `:src`, or `:href` are also valid.

# Conditional rendering

`v-if` adds and removes elements from the DOM, while `v-show` just toggles visibility (`display: none;`), so it's faster

# Prevent flash of unstyled content

v-cloak hides raw template

```
<style>
[v-cloak] { display: none; }
</style>

<div id="app" v-cloak>
```

# Vue CLI

In Powershell, run

`vue create myapp`

# Vuetify

To install, go to the app directory and run:

`vue add vuetify`

# Unit testing

Following the Vue Mastery course, we run

`vue create unit-testing-vue`

select Babel, Router, Vuex, Linter and Unit testing

use history for router
use Prettier
Lint on save
Jest
Save config in dedicated config files
Do not save preset

## Create a new component

Create the `AppHeader.vue` file in `src/components`

```
<template>
  <div>
    <button v-show="loggedIn">Logout</button>  // the test's output
  </div>
</template>

<script>
export default {
  data() {
    return {
      loggedIn: false  // the test's input
    }
  }
}
```

## Create the test .spec.js file

In the `tests/unit` directory, create `AppHeader.spec.js`

We want to test whether the button shows up or not, based on the state of `loggedIn`

In Jest, `describe` creates a test suite

`expect` creates the assertion

Run `npm run test:unit` to start the tests

```
import AppHeader from '@/components/AppHeader'
import { mount } from '@vue/test-utils'

describe('AppHeader', () => {
  test('if user is not logged in, do not show logout button', () => {
    const wrapper = mount(AppHeader)
    expect(wrapper.find('button').isVisible()).toBe(false)
  })

  test('if user is logged in, show logout button', async () => {
    const wrapper = mount(AppHeader)
    wrapper.setData({ loggedIn: true })

    await wrapper.vm.$nextTick()
    expect(wrapper.find('button').isVisible()).toBe(false)
  })
})
```

# Labeling and finding elements

Instead of using `id`s, we can set the `data-testid` attribute in the template:

`<input data-testid="name-input" type="text" v-model="name" />`

and in the spec test:

`const input = wrapper.find('[data-testid="name-input"]')`

# Vue 2 refs

In order to call instance methods from the console, refs may be used.

Main HTML:

```
        <div id="myrefsApp">
            <myrefs-dashboard ref="mydashboard"></myrefs-dashboard>
        </div>

        <script>
         const myrefsApp = new Vue({
           el: "#myrefsApp",
         });
        </script>
```

dashboard.js

```
Vue.component('myrefs-dashboard', {
  delimiters: ['{(', ')}'],
  template: `
      <div>
          Dashboard
          <br>
          <myrefs-chart ref="mychart"></myrefs-chart>
      </div>
  `,
  data() {
    return {
      name: "Dashboard",
    }
  },
  methods: {
    sayHi() {
      console.log("Dashboard says hi");
    }
  }
});
```

chart.js

```
Vue.component('myrefs-chart', {
  delimiters: ['{(', ')}'],
  template: `
      <div>
          Chart
      </div>          
  `,
  data() {
    return {
      name: "chart",
    }
  },
  methods: {
    sayHi() {
      console.log("Chart says hi");
    }
  },
});
```

in the console:

```
myrefsApp.$refs.mydashboard.$refs.mychart.sayHi()

myrefsApp.$refs.mydashboard.$refs.mychart.name
```