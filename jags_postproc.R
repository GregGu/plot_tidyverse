load(file.path(workdir, data_dir_jags))
res.country$CIprop.Lg.Lcat.qt$Afghanistan$Total
head(proportions$Total)
#we need to get data frames for each
#group_1        index
#country_idx    111111111111111
#year_idx       1950, 1951
#grid_idx       index
# $ 0.025      : num  0.00247 0.00267 0.00288 0.00311 0.00335 ...
# $ 0.1        : num  0.00332 0.00357 0.00384 0.00413 0.00444 ...
# $ 0.5        : num  0.00617 0.00656 0.00697 0.0074 0.00787 ...
# $ 0.9        : num  0.0119 0.0125 0.0131 0.0137 0.0144 ...
# $ 0.975      : num  0.0156 0.0163 0.017 0.0177 0.0185 ...


# Transform data to long format
dls <- list()
props <- res.country$CIprop.Lg.Lcat.qt
for (country in names(props)) {
  dls[[country]] <- props[[country]][["Total"]] %>%
          aperm() %>% #this transposes the data
          as.data.frame() %>% 
          dplyr::add_rownames("year_idx") %>%
          dplyr::mutate(year_idx = floor(as.numeric(year_idx)))
  dls[[country]]$country_idx <- i
          
}

do.call(rbind, dls) %>% 
  as.data.frame %>%
  dplyr::add_rownames("country_idx") %>%
  dplyr::mutate(country_idx = )


