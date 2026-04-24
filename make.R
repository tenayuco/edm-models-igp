
rm(list = ls()) # clear memory
graphics.off() # clear graphics



library(ggplot2)
library(patchwork)

# Look for every package used in the project
# Add them to DESCRIPTION under Imports
rdeps::add_deps()

# Install/update packages listed in DESCRIPTION
devtools::install_deps(upgrade = "never")


# Load packages under Depends and in R 
devtools::load_all()


########run the procedures you need######3333


source("./analyses/0.set_LBLB_model.R")


##here it runs the whole analysis and comparison with iy functions.. but you have to specify the noise you want to use.. 
#works

#------------------STOCHASTIC SIMULATIONS

set.seed(3)

random_seed <- rnorm(1)

num_rep <- 5
diff_len <- FALSE
#reso = 0.1  # Resolution

for (res in c(1, 0.1, 0.01)){
for (l in c(100, 50)){
  for (noi in c(0.1)){

reso <-  res
noise_chosen <- noi
len_chosen <- l

source("./analyses/1.1.stochastic_simulations.R")
}
}
}

  ##########