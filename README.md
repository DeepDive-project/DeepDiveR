# DeepDiveR
`DeepDiveR` is an R package to prepare input files for running DeepDive in Python. 

DeepDive, a deep learning pipeline for estimating biodiversity patterns in deep time using the fossil record, is described in the following publication:
Cooper, R. B., Flannery-Sutherland, J. T., and Silvestro, D. 2024. DeepDive: estimating global biodiversity patterns through time using deep learning. *Nature Communications*, 15.1, 4199. doi: [10.1038/s41467-024-48434-7](https://doi.org/10.1038/s41467-024-48434-7).

## Installation

You can install DeepDiveR directly in an R console by using the `devtools` library as below. Make sure you have R version 4.4.1 or above installed on your computer:

```
library(devtools)
devtools::install_github("DeepDive-project/DeepDiveR")
library(DeepDiveR)
```

## Data preparation

The functions in DeepDiveR assume input data to be contained within a `data.frame`. Columns should describe the identified `Taxon`, discrete `Area`, `MinAge`, `MaxAge` and `Locality` identifier, for each fossil occurrence.
Any necessary data cleaning steps, such as removal of duplicate occurrences, should be conducted prior to preparation of the DeepDive input files.

## Recommended workflow

More information on each function can be found using ?function_name_here.

1. Data preparation: `prep_dd_input`
Generates a `.csv` file containing information about relevant time bins, counts of localities per time bin per region, and counts of occurrences per taxa through time in each region. If using age replicates, these are also saved in the same file. 

2. Create config file: `create_config`
Generates a `.ini` initialisation file, describing settings to be executed during the DeepDive analysis. Setting a module to `FALSE` will remove that stage of the analysis from the pipeline; default values are all `TRUE`, meaning the full pipeline will be run.

Settings not included in the arguments for `create_config` can be updated using:
```
set_value(attribute_name = "parameter_you_want_to_set", value="value_here", module="module_where_parameter_is_stored", config)
```

To make specific areas appear through time, you can provide age ranges as below:
```
# each row represents a discrete sampling region
area_ages <- rbind(c(max(bins), max(bins)), c(50, 40)))
```
                   
Areas can also be made to disappear using `label = "end"` in the following:
```
areas_matrix(area_ages, n_areas = length(unique(dat$Area)), config)
```
which adds ages to the config.

The config is saved using:
```
config$write(paste(path_dat, "config.ini", sep="/"))
```


4. Read the config file in Python and launch analyses.
