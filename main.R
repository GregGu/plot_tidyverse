# Set working directory for the project
workdir <- getwd()
setwd(workdir)

# Source functions for this project
Rfiles <- list.files(file.path(paste0(getwd(),"/R/")), ".R") #gets names of r files
sapply(paste0(paste0(getwd(),"/R/"), Rfiles), source) #gets functions from file
fun <- lsf.str()
rm(list=setdiff(ls(), paste(fun)))
workdir <- getwd() # can probabily use unquote to put this in the setdiff line above

data_dir_meta <- "/data/mcmc.meta.rda"
data_dir_stan<- "/data/stan-model.rds"
data_dir_stand <- "/data/stan-data.rds"
load(paste0(workdir, data_dir_meta)) # replace with your data directory
stanm <- readRDS(paste0(workdir, data_dir_stan)) # change to where you have stan data stored
stan_data <- readRDS(paste0(workdir, data_dir_stand))
source("stan_postproc_plot.R")
source("stan_makeplots.R")