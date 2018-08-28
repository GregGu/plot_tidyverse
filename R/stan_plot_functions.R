library(ggplot2); library(dplyr)
# useless comment to check if git is up to date

#' Create a list of named empty plots.  
#'
#' We often want to produce multi-page plots and ggplot offers
#' no straightfoward way to do this.  This is a minimal structure
#' to make that easy.  
#'
#' @param names, the names of each component plots in a vector.  These
#'               will always be used with 'as.character'.
#' @return a list of empty plots.
plot_list <- function(names) {
  .pl <- list()
  for (name in names) {
    .pl[[as.character(name)]] <- ggplot()
  }
  return(.pl)
}

#' Add a time-series plot to a plot list.
#'
#' For each plot in .pl, add a plot IFF the grouping factor
#' in 'data' has a matching level (when used with 'as.character').
#'
#' @param .pl plot list, generate with `plot_list` and pipe in.
#' @param data data for time-series plots
#' @param time bare name of the time-axis column
#' @param value bare name of the value column.
#' @param group bare name of the variable to use to split plots on.
#' @return plot list
#' @export
plot_ts <- function(.pl, data, time, value, group, ...) {
  group = rlang::enquo(group) 
  time = rlang::enquo(time) 
  value = rlang::enquo(value) 

  
  data <- split(data, dplyr::select(data, !!group) %>% unlist)
  for (name in names(.pl)) {
    i <- as.character(name)
    if (is.null(data[[i]]))
      next
    .pl[[i]] <- .pl[[i]] + geom_line(data = data[[i]], 
                                     aes_string(x=rlang::quo_text(time), y=rlang::quo_text(value)))
      
      
  }
  for (name in names(.pl)) {
    i <- as.character(name)
    for (obj in list(...)) .pl[[i]] <- .pl[[i]] + obj
  }
  return(.pl)
}

#' Add bounds of a time-series to a plot list
#'
#' For each plot in .pl, add a plot IFF the grouping factor
#' in 'data' has a matching level (when used with 'as.character').
#'
#' @param .pl plot list, generate with `plot_list` and pipe in.
#' @param data data for time-series plots
#' @param time bare name of the time-axis column
#' @param lb bare name of the lower bound column.
#' @param ub bare name of the upper bound column.
#' @param group bare name of the variable to use to split plots on.
#' @return plot list
#' @export
plot_ts_bounds <- function(.pl, data, time, lb, ub, group, ...) {
  group = rlang::enquo(group) 
  time = rlang::enquo(time) 
  lb = rlang::enquo(lb) 
  ub = rlang::enquo(ub) 
  
  data <- split(data, dplyr::select(data, !!group) %>% unlist)
  for (name in names(.pl)) {
    i <- as.character(name)
    if (is.null(data[[i]]))
      next
    .pl[[i]] <- .pl[[i]] + geom_ribbon(data = data[[i]], 
                                       aes_string(x=rlang::quo_text(time), 
                                                  ymin=rlang::quo_text(lb), ymax=rlang::quo_text(ub)), 
                                       alpha = 0.3)
  }
  for (name in names(.pl)) {
    i <- as.character(name)
    for (obj in list(...)) .pl[[i]] <- .pl[[i]] + obj
  }
  return(.pl)
}

#' Add point estimates to a plot list.
#'
#' For each plot in .pl, add a plot IFF the grouping factor
#' in 'data' has a matching level (when used with 'as.character').
#'
#' @param .pl plot list, generate with `plot_list` and pipe in.
#' @param data data for point estimate plots
#' @param time bare name of the time-axis column
#' @param value bare name of the value column.
#' @param group bare name of the variable to use to split plots on.
#' @return plot list
#' @export
plot_pt <- function(.pl, data, time, value, group, ...) {
  group = rlang::enquo(group) 
  time = rlang::enquo(time) 
  value = rlang::enquo(value) 
  
  data <- split(data, dplyr::select(data, !!group) %>% unlist)
  for (name in names(.pl)) {
    i <- as.character(name)
    if (is.null(data[[i]]))
      next
    .pl[[i]] <- .pl[[i]] + geom_point(data = data[[i]], 
                                      aes_string(x=rlang::quo_text(time), y=rlang::quo_text(value)))
  }
  for (name in names(.pl)) {
    i <- as.character(name)
    for (obj in list(...)) .pl[[i]] <- .pl[[i]] + obj
  }
  return(.pl)
}


#' Add point estimate bounds to a plot list
#'
#' For each plot in .pl, add a plot IFF the grouping factor
#' in 'data' has a matching level (when used with 'as.character').
#'
#' @param .pl plot list, generate with `plot_list` and pipe in.
#' @param data data for time-series plots
#' @param time bare name of the time-axis column
#' @param lb bare name of the lower bound column.
#' @param ub bare name of the upper bound column.
#' @param group bare name of the variable to use to split plots on.
#' @return plot list
#' @export
plot_pt_bounds <- function(.pl, data, time, lb, ub, group, ...) {
  group = rlang::enquo(group) 
  time = rlang::enquo(time) 
  lb = rlang::enquo(lb) 
  ub = rlang::enquo(ub) 
  
  data <- split(data, dplyr::select(data, !!group) %>% unlist)
  for (name in names(.pl)) {
    i <- as.character(name)
    if (is.null(data[[i]]))
      next
    .pl[[i]] <- .pl[[i]] + geom_errorbar(data = data[[i]], 
                                         aes_string(x=rlang::quo_text(time), 
                                                    ymin=rlang::quo_text(lb), ymax=rlang::quo_text(ub))
    )
  }
  for (name in names(.pl)) {
    i <- as.character(name)
    for (obj in list(...)) .pl[[i]] <- .pl[[i]] + obj
  }
  return(.pl)
}


#' Add point estimates for units to a plot list.
#'
#' For each plot in .pl, add a plot IFF the grouping factor
#' in 'data' has a matching level (when used with 'as.character').
#'
#' @param .pl plot list, generate with `plot_list` and pipe in.
#' @param data data for point estimate plots
#' @param unit bare name of the unit-axis column
#' @param value bare name of the value column.
#' @param group bare name of the variable to use to split plots on.
#' @return plot list
#' @export
plot_unit <- function(.pl, data, unit, value, group, ...) {
  group = rlang::enquo(group) 
  unit = rlang::enquo(unit) 
  value = rlang::enquo(value) 
  
  data <- split(data, dplyr::select(data, !!group) %>% unlist)
  for (name in names(.pl)) {
    i <- as.character(name)
    if (is.null(data[[i]]))
      next
    .pl[[i]] <- .pl[[i]] + geom_point(data = data[[i]], 
                                      aes_string(x=rlang::quo_text(value), y=rlang::quo_text(unit)))
  }
  for (name in names(.pl)) {
    i <- as.character(name)
    for (obj in list(...)) .pl[[i]] <- .pl[[i]] + obj
  }
  return(.pl)
}

#' Add bounds estimates for units to a plot list.
#'
#' For each plot in .pl, add a plot IFF the grouping factor
#' in 'data' has a matching level (when used with 'as.character').
#'
#' @param .pl plot list, generate with `plot_list` and pipe in.
#' @param data data for point estimate plots
#' @param unit bare name of the unit-axis column
#' @param value bare name of the value column.
#' @param group bare name of the variable to use to split plots on.
#' @return plot list
#' @export
plot_unit_bounds <- function(.pl, data, unit, lb, ub, group, ...) {
  group = rlang::enquo(group) 
  unit = rlang::enquo(unit) 
  lb = rlang::enquo(lb) 
  ub = rlang::enquo(ub) 
  
  data <- split(data, dplyr::select(data, !!group) %>% unlist)
  for (name in names(.pl)) {
    i <- as.character(name)
    if (is.null(data[[i]]))
      next
    .pl[[i]] <- .pl[[i]] + geom_errorbarh(data = data[[i]], 
                                          aes_string(x=rlang::quo_text(lb), y=rlang::quo_text(unit),
                                                     xmin = rlang::quo_text(lb), xmax = rlang::quo_text(ub)))
  }
  for (name in names(.pl)) {
    i <- as.character(name)
    for (obj in list(...)) .pl[[i]] <- .pl[[i]] + obj
  }
  return(.pl)
}



#' Map point estimates for units in a plot list.
#'
#' For each plot in .pl, add a plot IFF the grouping factor
#' in 'data' has a matching level (when used with 'as.character').
#'
#' @param .pl plot list, generate with `plot_list` and pipe in.
#' @param data data for point estimate plots
#' @param map list of map df objects to merge data to
#' @param unit bare name of the unit column (in map and data)
#' @param value bare name of the value column.
#' @param group bare name of the variable to use to split plots on.
#' @return plot list
#' @export
map_unit <- function(.pl, data, map, unit, value, group, ...) {
  group = rlang::enquo(group) 
  unit = rlang::enquo(unit) 
  value = rlang::enquo(value) 
  
  data <- split(data, dplyr::select(data, !!group) %>% unlist)
  for (name in names(.pl)) {
    i <- as.character(name)
    if (is.null(data[[i]]))
      next
    local_data <- dplyr::left_join(map[[i]], data[[i]], by = rlang::quo_text(unit))
    .pl[[i]] <- .pl[[i]] + geom_polygon(data = local_data,
                                        aes_string(x='longitude', y= 'latitude', fill = rlang::quo_text(value)), 
                                        colour = 'black') + coord_fixed()
  }
  for (name in names(.pl)) {
    i <- as.character(name)
    for (obj in list(...)) .pl[[i]] <- .pl[[i]] + obj
  }
  return(.pl)
}