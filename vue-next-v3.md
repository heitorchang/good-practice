# Experimenting with Vue 3 alpha (vue-next)

*2 Apr 2020*

First, install Node.js

## Install vue-next dependencies

Create a project directory and run

```
git clone https://github.com/vuejs/vue-next-webpack-preview PROJECT_DIR
rm -f -r PROJECT_DIR/src
mkdir PROJECT_DIR/src

cd PROJECT_DIR
npm i
```

## src/ files

**App.vue**

```
<template>
    <button @click="inc" id="btn_inc">Inc</button>
    <span id="span_times">Button clicked {{ count }} times.</span>
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

## Running the dev server on port 8080

`npm run dev`

## Functional test with Python and Selenium

With the dev server running, run this Python script

**functional_tests.py**

```
import unittest
from selenium import webdriver


class ClickTest(unittest.TestCase):
    def setUp(self):
        self.browser = webdriver.Chrome()

    def tearDown(self):
        self.browser.quit()

    def test_click_three_times(self):
        self.browser.get('http://127.0.0.1:8080/')

        btn = self.browser.find_element_by_id("btn_inc")
        for i in range(3):
            btn.click()
            
        self.assertEqual('Button clicked 3 times.', self.browser.find_element_by_id("span_times").text)

        
if __name__ == '__main__':
    unittest.main(warnings="ignore")
```

## Deploying

`npm run build`

Create a simple `index.html` file in `dist/`

```
<div id="app"></div>
<script src="main.js"></script>
```

Deploy the `dist/` directory.
