## Anaconda Setup

Open Anaconda Prompt

### To create an environment

`conda create --name machinelearning`

Run `conda activate machinelearning`

`conda install -c anaconda keras`

### To use Keras in Jupyter:

`pip install ipykernel`

`python -m ipykernel install --user --name=machinelearning`

`jupyter notebook`

In the notebook, click on Kernel, then Change Kernel