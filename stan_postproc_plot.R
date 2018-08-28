
# Load and import
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


