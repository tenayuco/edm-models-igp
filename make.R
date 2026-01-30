# Look for every package used in the project
# Add them to DESCRIPTION under Imports
rdeps::add_deps()

# Install/update packages listed in DESCRIPTION
devtools::install_deps()

# Load packages under Depends and in R 
devtools::load_all()


### heres is the data
DATA_IGP <- readr::read_csv("data/dataIGP_2025.csv")

##here we use 2 formats of data
DATA_LONG <-  long_formatter(DATA_IGP)
DATA_MEAN <-  mean_formatter(DATA_LONG)


### plot and save data
plotter_data_all(DATA_LONG)

plotter_data_mean(DATA_MEAN)
