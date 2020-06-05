(taken from Django Crash Course)

## Set up conda

`conda create -n PROJECTNAME python=3.8`

`conda activate PROJECTNAME`

Run `conda activate PROJECTNAME` every time a new shell is started

`conda install -c conda-forge cookiecutter`

`cd /PATH/TO/PROJECTS`


## Freeze the environment

To share your environment, run

`conda env export > environment.yml`