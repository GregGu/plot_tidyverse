load(file.path(workdir, data_dir_jags))

# Transform data to long format
proportions_jags <- list()
prop_names <- c("Total","Modern","Traditional","Unmet")
res_country_obj <- res.country$CIprop.Lg.Lcat.qt

for (proportion in prop_names){
  dls <- list()
for (country in names(res_country_obj)) {
  dls[[country]] <- res_country_obj[[country]][[proportion]] %>%
          aperm() %>% #this transposes the data
          as.data.frame() %>% 
          dplyr::add_rownames("year_idx") %>%
          dplyr::mutate(year_idx = floor(as.numeric(year_idx)))
}
proportions_jags[[proportion]] <- do.call(rbind, dls) %>% 
  as.data.frame %>%
  dplyr::add_rownames("country_idx") %>%
  dplyr::mutate(country = gsub("[^[:alpha:], ]","", country_idx)) %>%
  dplyr::mutate(country_idx = gsub("[^[:digit:], ]","", country_idx))
}





