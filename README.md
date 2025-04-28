# DeepDiveR
DeepDiveR is an R package to prepare input for running DeepDive in Python.

**Note:** The DeepDiveR package is under active developement: this branch is currently being prepared for CRAN submission. To see the stable version, which was used to run the analyses in the application note, view the [main branch](https://github.com/DeepDive-project/DeepDiveR).

You can install DeepDiveR directly in an R console by using the devtools library as below. Make sure you have R version 4.4.1 or above installed on your computer:

```
library(remotes)
remotes::install_github("DeepDive-project/DeepDiveR")
library(DeepDiveR)
```

An alternative branch can be installed by specifying the branch to install:

```
library(remotes)
remotes::install_github("DeepDive-project/DeepDiveR", ref = "application_note")
library(DeepDiveR)
```

The functions in DeepDiveR assume input data to be organised by columns with a "Taxon", discrete "Area", "MinAge", "MaxAge" and "Locality" identifier for each fossil occurrence. 
Please carry out any extra cleaning steps, such as removal of duplicate occurrences, prior to use of the data preparation function.

You can read more about following functions using ?function_name_here.

1. Data preparation: ```prep_dd_input```
Generates a .csv file containing information about the time bins, counts of localities per time bin per region and counts of occurrences per taxa through time in each region. If using age   replicates these are also saved in the same file. 

2. Create a configuration file: ```create_config```
Generates a .ini file of settings for analyses that will be executed in step 3. Setting modules to false will remove stages of analyses from the pipeline, by default the full pipeline will be run. For arguments see ```?create_config```.

Settings not included in the arguments for create_config can be updated using:
```
edit_config(config, module = "module_where_parameter_is_stored", parameter = "parameter_you_want_to_set", value = "updated_parameter_value")
```
For arguments see ```?edit_config```.

The configuration file is saved using:
```
config$write(paste(path_dat, "config.ini", sep="/"))
```

3. Execute files and launch analyses
Once the configuration and input files are created, the full DeepDive analysis, inclusive of simulation, model training and empirical predictions, can be carried out through a single command line entered in a Terminal (MacOS and Linux) or Command prompt (Windows) window using the Python script run_dd_config.py:

```
python run_dd_config.py your_path/config_file.ini
```

You can additionally specify a working directory where all output files are saved and the number of CPUs used for the parallelized simulations, which will overwrite the corresponding settings in the configuration file, using the flags 
```
-wd your\_working\_directory
```
and e.g. -cpu 64. 

This script will create a "simulations" folder containing the training and test sets, and a "trained_models" folder containing the trained models and plots of the training history. This folder will additionally include plots comparing the empirical and simulated fossil features (e.g. number of occurrences through time and per area, number of localities, fraction of singletons, and sampled diversity), CSV files with the predicted diversity trajectories for the test set and for the empirical dataset, and a plot of the estimated diversity trajectory.

