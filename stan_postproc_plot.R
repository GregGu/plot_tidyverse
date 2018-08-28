# Directories
mydir <- getwd()
#run.name <- "test_stan3k"
# Load and import
# chains <- c("chain1.csv", "chain2.csv", "chain3.csv")
# stanm <- read.csv(paste0(mydir, "/output/", run.name, chains))
# proportions <- rstan::extract(stanm, 'proportions', permute = TRUE, inc_warmup = TRUE)
stanm <- readRDS(paste0(mydir, "/output", "/stan-model.rds")) # change to where you have stan data stored
stan_out <- proportions <- rstan::extract(stanm, "proportions", permute=TRUE)
stan_out <- stan_out[[1]]



# Definitions for props in our stan output
# proportions[1] = (ones - R) .* P;
# proportions[2] = R .* P;
# proportions[3] = (ones - P) .* Z;
# proportions[4] = (ones - P) .* (ones - Z); 
proportions <- list()
proportions[['Total']] <- (stan_out[,1,]+stan_out[,2,])
proportions[['Modern']] <- (stan_out[,2,])
proportions[['Traditional']] <- proportions[['Total']]-proportions[['Modern']]
proportions[['Unmet']] <- (stan_out[,3,])

# Preducted Index, used to index our stan output
stan_data <- readRDS(paste0(mydir, "/output", "/stan-data.rds"))
predicted_index<-stan_data$grid_country_year
library('dplyr') 
predicted_index <- predicted_index %>% mutate(year_idx = year_idx+stan_data$min_year-1 )
for (i in 1:length(proportions)) {
  data = proportions[[i]] %>% 
    label(labels = list(group_1 = dplyr::mutate(predicted_index, group_1 = as.character(grid_idx)))) %>%
    summarize(f = std_estimates)
  proportions[[i]]<- data
}
predictions <- (proportions) #just renaming for now


