# VirHunter
![VirHunter logo](media/logo.png?raw=true "VirHunter logo")

**VirHunter** is a tool that uses deep learning to identify viruses in plant virome sequencing datasets. In particular, VirHunter classifies previously assembled contigs into virus, host and bacteria classes.

## Useful Info
* Recently we uploaded VirHunter to [Galaxy Europe](https://usegalaxy.eu/root?tool_id=toolshed.g2.bx.psu.edu/repos/iuc/virhunter/virhunter/1.0.0+galaxy1). This means that now you can run VirHunter on your data without installation!
* If you want to get rid of phage or fungi contamination please check out [Decontaminator](https://github.com/cbib/decontaminator). 
It goes well along with VirHunter!
* Finally, [here](https://www.frontiersin.org/articles/10.3389/fbinf.2022.867111/full) is the original publication on VirHunter. Please cite it, when you use VirHunter.
If you are looking for a more practical explanation of VirHunter check out this [presentation](media/virhunter_description.pdf).

## System Requirements
VirHunter installation requires a Unix environment with [python 3.8](http://www.python.org/). 
It was tested on Linux and macOS operating systems. 
For now, VirHunter is still not fully compatible with M1 chip MacBook.

In order to run VirHunter you need to have git and conda already installed. 
If you are installing conda for the first time, we suggest you to use 
a lightweight [miniconda](https://docs.conda.io/en/latest/miniconda.html).
Otherwise, you can use pip for the dependencies' installation.
         
## Installation 

To install VirHunter, you need to download it from github and then to install the dependencies.

First, clone the repository from [github](https://github.com/cbib/virhunter)

```shell
git clone https://github.com/cbib/virhunter.git
```

Go to the VirHunter root folder

```shell
cd virhunter/
```


### Installing dependencies with Conda

First, you have to create the environment from the `envs/environment.yml` file. 
The installation may take around 500 Mb of drive space. 

```shell
conda env create -f envs/environment.yml
```

Second, activate the environment:

```shell
conda activate virhunter
```

### Installing dependencies with pip

If you don't have Conda installed in your system, you can install python dependencies via pip program:

```shell
pip install -r envs/requirements.txt
```

Then if you have macOS you will need to install `wget` library to run some scripts (Conda installation already has it). You can do this with `brew` package manager.

```shell
brew install wget
```

### Testing your installation of VirHunter

You can test that VirHunter was successfully installed on the toy dataset we provide. 
IMPORTANT: the toy dataset is intended only to test that VirHunter has been well installed and all the scripts can be executed. 
These modules should not be used for prediction on your owd datasets!

First, you have to download the toy dataset
```shell
bash scripts/download_test_installation.sh
```
Then run the bash script that calls the testing, training and prediction python scripts of VirHunter.
Attention, the training process may take some time (up to an hour).
```shell
bash scripts/test_installation.sh
```

## Using VirHunter for prediction

To run VirHunter you can use the already pre-trained models or train VirHunter yourself (described in the next section).
Pre-trained model weights are already available for the multiple host plants. 
You can download them using the `download_weights.sh` script.

```shell
bash scripts/download_weights.sh 
```

Before launching the prediction you will need to fill the `configs/predict_config.yaml` file. 
If for example, you want to use the weights of the pretrained model for peach, 
you should change the field `weights` in the `configs/predict_config.yaml` to `weights/peach`.

VirHunter supports prediction for multiple test files at once. 
For that you need to change a bit the field `test_ds` in the
`configs/predict_config.yaml`. 

```yaml
predict:
    test_ds:
      - /path/to/test_ds_1
      - /path/to/test_ds_2
      - /path/to/test_ds_3  
```

Once the config file is ready, you can start the prediction:

```shell
python virhunter/predict.py configs/predict_config.yaml
```

After prediction VirHunter produces two `csv` files and one optional `fasta` file:

1. The first file ends with `_predicted_fragments.csv`
It is an intermediate result containing predictions of the three CNN networks (probabilities of belonging to each of the virus/plant/bacteria class) and of the RF classifier for each fragment of every contig.

2. The second file ends with `_predicted.csv`. 
This file contains final predictions for contigs calculated from the previous file. 
   - `id` - fasta header of a contig.
   - `length` - length of the contig.
   - `# viral fragments`, `# plant fragments` and `# bacterial fragments` - the number of fragments of the contig that received corresponding class prediction by the RF classifier.
   - `decision` - class given by the VirHunter to the contig.
   - `# viral / # total` - number of viral fragments divided by the total number of fragments of the contig.

3. The optional fasta file ends with `_viral.fasta`. It contains contigs that were predicted as viral by VirHunter.
To generate it you need to set flag `return_viral` to `True` in the config file.

`configs/predict_config.yaml` has a field `limit` that is used to discard contigs that are shorter than `limit` from prediction. 
We tested limit of 750 in the paper and suggest using it as a default one. You can change the limit, but we do not guarantee VirHunter performance then.


## Available models
We have trained 8 models of VirHunter:  _carrot_, _grapevine_, _lettuce_, _peach_, _rice_, _sugar beet_, _tomato_ and _generalistic_.
The last model was prepared with a mixture of plants from _dicots_ and _monocots_ clades. 

We recommend to use individual plant models, when the host plant of the virome belongs to the same family. 
In other cases you can use _generalistic_ model. 


## Training your own model

You can train your own model, for example for a specific host species. Before training, you need to collect sequence 
data for training for three reference datasets: _viruses_, _bacteria_ and _host_. 
Examples are provided by running `scripts/download_test_installation.sh` that will download `viruses.fasta`, 
`host.fasta` and `bacteria.fasta` files (real reference datasets should correspond 
e.g. to the whole genome of the host, all bacteria and all viruses from the NCBI).

Training requires execution of the following steps:
- prepare the training dataset for the neural network and Random Forest modules from fasta files with `prepare_ds.py`.
- train the neural network and Random Forest modules with `train.py`

The training will be done twice - for fragment sizes of 500 and 1000.

The successful training of VirHunter produces weights for the three neural networks from the first module and weights for the 
trained Random Forest classifier for fragment sizes of 500 and 1000. They can be subsequently used for prediction.

To execute the steps of the training you must first create a copy of the `template_config.yaml`. 
Then fill in the necessary parts of the config file. No need to fill in all tasks! 
Once config file is filled you can launch the scripts consecutively providing them with the config file like this:
```shell
python virhunter/prepare_ds.py configs/config.yaml
```
And then
```shell
python virhunter/train.py configs/config.yaml
```
Important to note, the suggested number of epochs for the training of neural networks is 10.

### Complex dataset preparation
If you want to prepare dataset that would have host oversampling for chloroplast and CDS (like it was done in the paper), 
you can use `prepare_ds_complex.py` script. Compared to `prepare_ds.py` it will require paths to CDS and chlroplast containing fasta files.

### Training VirHunter on GPU

If you plan to train VirHunter on GPU, please use `environment_gpu.yml` or `requirements_gpu.txt` for dependencies installation.
Those recipes were tested only on the Linux cluster with multiple GPUs.
If you plan to train VirHunter on cluster with multiple GPUs, you will need to uncomment line with
`CUDA_VISIBLE_DEVICES` variable and replace `""` with `"N"` in header of `train_nn.py`, where N is the number of GPU you want to use.

```python
import os
os.environ["CUDA_VISIBLE_DEVICES"] = "N"
```

## VirHunter for galaxy
`virhunter_galaxy` folder contains modified scripts for the galaxy version of virhunter.
