# Load Jags data
load(file.path(workdir, data_dir_jags))

# Transform data from wide to long, create numeric country column, floor year, create index
proportions_jags <- list()
prop_names <- c("Total","Modern","Traditional","Unmet")
res_country_obj <- res.country$CIprop.Lg.Lcat.qt

for (proportion in prop_names){
  dls <- list()
for (country in names(res_country_obj)) {
  dls[[country]] <- res_country_obj[[country]][[proportion]] %>%
          aperm() %>% # aperm transposes the data
          as.data.frame() %>% 
          dplyr::add_rownames("year_idx") %>% # creates a column based on rownames
          dplyr::mutate(year_idx = floor(as.numeric(year_idx))) # floor the year
}
proportions_jags[[proportion]] <- do.call(rbind, dls) %>% 
  as.data.frame %>%
  dplyr::add_rownames("country_idx") %>% # again create column based on rownames
  dplyr::mutate(country = gsub("[^[:alpha:], ]","", country_idx)) %>% # retain only characters in names
  dplyr::mutate(country_idx = as.numeric(as.factor(country))) %>% # create numeric country index based on given jags order
  dplyr::mutate(group_1 = 1:length(country_idx)) %>% # create basic index
  as.data.frame()
}





