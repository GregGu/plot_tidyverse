# Jackknife Estimate processing before ploting
# Neccessary for JAGS and STAN
library(dplyr)

#estimates
ls <- list(
  mcmc.meta$data.raw$data$props.tot.j,
  mcmc.meta$data.raw$data$props.modern.j,
  mcmc.meta$data.raw$data$props.trad.j,
  mcmc.meta$data.raw$data$props.unmet.j)
names(ls) <- names(predictions)
# se <- list() # std errors arent imputed in data yet
# se[[2]] <- mcmc.meta$data.raw$data$include.modern.ses
# se[[3]] <- mcmc.meta$data.raw$data$include.trad.ses
# se[[4]] <- mcmc.meta$data.raw$data$include.unmet.ses
year <- mcmc.meta$data.raw$data$years.j
name <- mcmc.meta$data.raw$data$name.j
iso <- mcmc.meta$winbugs.data$iso.j
c <- mcmc.meta$winbugs.data$getc.j
#country_name <- mcmc.meta$data.raw$data$name.j
#################################################

# Make Plots
for (i in 1:length(names(predictions))){
  estimates <- data.frame(year_idx=floor(year), unit_idx=c, estimate=ls[[i]]) #, lb=-2*se[i] ,ub=2*se[i])
  names(predictions[[i]])[c(5,7,9)] <- c("lb", "estimate", "ub")
  pred <- predictions[[i]]
  
  library(ggplot2)
  pll = plot_list(names=unique(as.character(pred$country_idx))) %>% 
    plot_ts(data = pred, time=year_idx, value=estimate, group=country_idx, labs(x = "years", y = paste(names(predictions)[i],"contraceptive %"))) %>% 
    plot_ts_bounds(pred, time=year_idx, lb=lb, ub=ub, group=country_idx) %>%
    plot_pt(data = estimates, year_idx, estimate, unit_idx, theme_minimal())
    #%>% #estimate will be the name of the column with estimates
  # plot_pt_bounds(estimates, year_idx, lb, ub, unit_idx,
  #                theme_minimal())
  
  if (!dir.exists('fig'))
    dir.create('fig')
  target <- "fig"
  target_stub <- file.path(target, paste0("time-series-plots-", names(predictions)[i]))
  #saveRDS(pll, file = paste0(target_stub, ".rds"))
  pdf(paste0(target_stub, ".pdf"), height = 4, width = 4)
  for (pl in pll) print(pl); dev.off()
}

