# Set working directory
workdir <- getwd() #swap getwd with setwd() if not using Rprojs


# Source functions for this project
Rfiles <- list.files(file.path(paste0(getwd(),"/R/")), ".R") #gets names of r files
sapply(paste0(paste0(getwd(),"/R/"), Rfiles), source) #gets functions from file

if (!("pager" %in% rownames(installed.packages())))
devtools::install_github("sakrejda/pager")
library(pager)

data_dir_meta <- "data/mcmc.meta.rda"
data_dir_jags <- "data/res.country.rda"
data_dir_stan<- "data/stan-model.rds"
data_dir_stand <- "data/stan-data.rds"
load(file.path(workdir, data_dir_meta))
stanm <- readRDS(file.path(workdir, data_dir_stan)) 
stan_data <- readRDS(file.path(workdir, data_dir_stand))
source("stan_postproc_plot.R")
source("stan_makeplots.R")