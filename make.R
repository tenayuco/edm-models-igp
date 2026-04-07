library(ggplot2)

# Look for every package used in the project
# Add them to DESCRIPTION under Imports
rdeps::add_deps()

# Install/update packages listed in DESCRIPTION
devtools::install_deps(upgrade = "never")


# Load packages under Depends and in R 
devtools::load_all()


########run the procedures you need######3333

source("./analyses/1.analyze_data.R")
