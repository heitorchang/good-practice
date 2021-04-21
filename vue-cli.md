# Vue CLI

Installing

`sudo npm install -g @vue/cli`

To start a new project

`vue create myapp`

Add Vuetify

`cd myapp`
`vue add vuetify`

Run dev server

`npm run serve`

Build

`npm run build`

Moving dist/ files to a web server (such as Django)

1. Copy the .css and .js hash values to the <link> and <script> tags in the web server's HTML file.
2. Copy the compiled .css and .js files to the static directories inside the web server's project.