# Vuetify

* Containers are 'fixed' by default. Setting them to 'fluid' will make them occupy all available space.

## When a `{{ item }}` does not update

In the case of obsprev/tokenel/ChartPerda, inside the main dialog, a `{{ tempState.totalPerda }}` was not updating.

The solution was to create a `localTotalPerda` data key and in the computation function, set

`this.localTotalPerda = this.tempState.totalPerda;`