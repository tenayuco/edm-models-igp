# Code for LV map and CCM analysis

[![License:
GPL-2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://choosealicense.com/licenses/gpl-2.0/)


This project aims.....

## Content

This project is structured as follow:

```
.
├─ README.md                                  # Presentation of the project
├─ DESCRIPTION                                # Project metadata
├─ LICENSE.md                                 # License of the project
|
├─ data/                                      # Contains raw data
|  ├─ bifurcation/                              # stores the data of bifurcation
|  |
|  |
|  └─ bicontrol/                             # stores the data of biocontorl
|     ├─ 
|
├─ figures/
|
├─ outputs/    
|
├─ R/                                         # Contains R functions (only)
|  ├─                    # functions used everywhere
|  └─                    # functions create a list with the initial conditions of each model 
|  └─                 # functions to create the database of biocontrol.
|  └─                   # function to plot the biocontrol data 
|  └─                 # function plots the bifurcation data
|  └─                   # functions to create database of bifurcations
|  └─                 # functions with the models
|  └─                   #old functions that could be useful 
|  └─                  # old functions that could be useful 


├─ analyses/                                  # Contains R scripts
|  └─                      # Script to create initial conditions
|  └─                    # Script to create the data frame of biocontrol
|  └─                  # Script to analyze biocontrol
|  └─                # Script to create thedata frame of bifurcations
|  └─                  # Script to plot bifrucations
|  └─                  # Script to do manual bifurcations with the debif
└─ make.R                                     # Script to setup & run the project
```


> [!NOTE]  
> The folder **data/** **output/** are not present in this repository (listed in the `.gitignore`) 
> but we provide the code to recreate it



#

## Usage

Open this project in Positron and either run the makeR. or each of the analyses manually

```r
source("make.R")
```

- All packages will be automatically installed and loaded
- Raw data will be saved in the `data/` directory



## License

This project is released under the 
[GPL-2](https://choosealicense.com/licenses/gpl-2.0/) license.



## Citation



## References
