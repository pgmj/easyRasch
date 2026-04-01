#' Person fit
#'
#' Outputs a histogram of person fit ZSTD and a plot with person fit ZSTD and
#' person location/score. Defaults to output a histogram and a hex heatmap.
#'
#' Optional grouped output with colorized points.
#'
#' You can also get a vector with row numbers for persons with infit ZSTD
#' over/under +/- 1.96 by using `output = "rowid"`. Or the full dataframe
#' with all respondents infit ZSTD and estimated theta values with
#' `output = "dataframe`.
#'
#' If you desire another cutoff than +/- 1.96, it can be set with `infit_lim`.
#'
#' Note: theta estimation is done using ML, which is not optimal but should
#' be sufficient for this analysis.
#'
#' @param dfin Dataframe with item data only
#' @param model Rasch model to use, "PCM" or "RM"
#' @param pointsize Size of datapoints for grouped view
#' @param alpha Transparency of points (0-1 where 1 = not transparent)
#' @param bins Number of bins for hexplot
#' @param group Optional grouping variable
#' @param output Can also be "rowid" for a dataframe with rownumbers
#' @param infit_lim Lower/upper limit for person infit ZSTD
#' @export
RIpfit <- function(dfin, model = "PCM", pointsize = 2.5, alpha = 0.5, bins = 30,
                   group, output = c("hist","heatmap"), infit_lim = c(-1.96,1.96)) {
  if (model == "PCM") {
    df.erm <- PCM(dfin)
  } else {
    df.erm <- RM(dfin)
  }
  person.locations.estimate <- person.parameter(df.erm)
  person.fit <- eRm::personfit(person.locations.estimate)
  thetas2 <- as.data.frame(person.locations.estimate$theta.table)

  nPfit <- length(person.fit$p.infitZ)
  nCeilingPfit <- length(which(person.fit$p.infitZ > infit_lim[2]))
  nFloorPfit <- length(which(person.fit$p.infitZ < infit_lim[1]))
  nPgoodfit <- (nPfit - (nCeilingPfit + nFloorPfit))

  # find highest/lowest fit and location to set limits for plots automatically
  xlim_max <- ceiling(max(person.fit$p.infitZ, na.rm = TRUE))
  xlim_min <- floor(min(person.fit$p.infitZ, na.rm = TRUE))
  ylim_max <- ceiling(max(thetas2$`Person Parameter`, na.rm = TRUE))
  ylim_min <- floor(min(thetas2$`Person Parameter`, na.rm = TRUE))

  if ("hist" %in% output) {
    hist(person.fit$p.infitZ, col = "#009ca6", xlim = c(xlim_min, xlim_max), xlab = "Person infit ZSTD", main = "Histogram of Person infit ZSTD")
  }
  # check whether there are excluded observations, and if found, adjust thetas2 df
  if (length(person.fit$excl_obs_num) > 0L) {
    thetas2[person.fit$excl_obs_num, ] <- NA
    thetas2 <- na.omit(thetas2)
  }

  df.pfit <- data.frame(
    p_locs = thetas2$`Person Parameter`,
    p_infit = person.fit$p.infitZ
  ) %>%
    rownames_to_column("rownumber") %>%
    mutate(rownumber = gsub(pattern = "P","", rownumber)) %>%
    mutate(rownumber = as.integer(rownumber)) %>%
    dplyr::rename(`Person locations` = p_locs,
                  `Person infit ZSTD` = p_infit)

  if ("rowid" %in% output) {
    rowid <- df.pfit %>%
      dplyr::filter(`Person infit ZSTD` > infit_lim[2] | `Person infit ZSTD` < infit_lim[1]) %>%
      pull(rownumber)
    return(rowid)
  }

  if ("dataframe" %in% output) {
    return(janitor::clean_names(df.pfit))
  }

  if (missing(group) && "heatmap" %in% output) {
    # figure
    df.pfit %>%
      ggplot(aes(x = `Person infit ZSTD`, y = `Person locations`, label = "")) +
      geom_vline(xintercept = infit_lim[1], color = "#e83c63", linetype = 2, linewidth = 0.7) +
      geom_vline(xintercept = infit_lim[2], color = "#e83c63", linetype = 2, linewidth = 0.7) +
      geom_hex(bins = bins, linewidth = 0.1, color = "darkgrey") +
      scale_fill_viridis_c('Count', option = "inferno", begin = 0.1) +
      scale_y_continuous(breaks = seq(ylim_min, ylim_max, by = 1)) +
      scale_x_continuous(breaks = seq(xlim_min, xlim_max, by = 1)) +
      labs(caption = paste0(
        round(nFloorPfit / nPfit * 100, 1), "% of participants have person infit ZSTD below ",infit_lim[1],", and ",
        round(nCeilingPfit / nPfit * 100, 1), "% are above ",infit_lim[1],". \nThus, ", round(nPgoodfit / nPfit * 100, 1),
        "% of participants without floor/ceiling effects are within infit ZSTD limits.\nNote: ",length(person.fit$excl_obs_num)," (",round(length(person.fit$excl_obs_num)/nrow(dfin)*100,1),"%) observations were excluded due to max/min score."
      )) +
      theme(plot.caption = element_text(hjust = 0, face = "italic")) +
      theme(
        panel.background = element_rect(
          fill = "#ebf5f0",
          colour = "#ebf5f0",
          linewidth = 0.5, linetype = "solid"
        ),
        panel.grid.major = element_line(
          linewidth = 0.5, linetype = "solid",
          colour = "white"
        ),
        panel.grid.minor = element_line(
          linewidth = 0.25, linetype = "solid",
          colour = "white"
        )
      )
  }
  else if (!missing(group) && "heatmap" %in% output) {
    group[person.fit$excl_obs_num] <- NA # remove max/min scoring individuals from grouping variable
    df.pfit$grp <- na.omit(as.factor(group))
    df.pfit %>%
      ggplot(aes(x = `Person infit ZSTD`, y = `Person locations`, label = "", color = grp)) +
      geom_vline(xintercept = infit_lim[1], color = "#e83c63", linetype = 2, linewidth = 0.7) +
      geom_vline(xintercept = infit_lim[2], color = "#e83c63", linetype = 2, linewidth = 0.7) +
      geom_hex(bins = bins, linewidth = 0.5) +
      scale_color_brewer('Group', type = "qual", palette= "Dark2") +
      scale_fill_viridis_c('Count', option = "inferno", begin = 0.2) +
      scale_y_continuous(breaks = seq(ylim_min, ylim_max, by = 1)) +
      scale_x_continuous(breaks = seq(xlim_min, xlim_max, by = 1)) +
      labs(caption = paste0(
        round(nFloorPfit / nPfit * 100, 1), "% of participants have person infit ZSTD below ",infit_lim[1],", and ",
        round(nCeilingPfit / nPfit * 100, 1), "% are above ",infit_lim[1],". \nThus, ", round(nPgoodfit / nPfit * 100, 1),
        "% of participants without floor/ceiling effects are within infit ZSTD limits.\nNote: ",length(person.fit$excl_obs_num)," (",round(length(person.fit$excl_obs_num)/nrow(dfin)*100,1),"%) observations were excluded due to max/min score."
      ))  +
      facet_wrap(~grp) +
      guides(color = "none") +
      theme_minimal(base_family = "Arial") +
      theme(strip.text = element_text(size = 12))
  }
}

#' Person fit with U3 for polytomous data
#'
#' This function will estimate person parameters with WLE and item parameters
#' with CML and use these as input to the `PerFit::U3poly()` function. Since
#' the results from `PerFit::U3poly()` are inconsistent, the same analysis is
#' iterated 100 times, and the median proportion of flagged respondents
#' is returned.
#'
#' @param data Dataframe with item responses
#' @export
#'
RIu3poly <- function(data) {

  # check data
  if(min(as.matrix(data), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")
  } else if(nrow(na.omit(data)) == 0) {
    stop("No complete cases in data.")
  }

  # CML estimation of item parameters
  params <- RIitemparams(data, output = "dataframe") %>%
    select(!Location) %>%
    as.matrix()
  # WL estimation of person parameters
  thetas <- RIestThetas(data)$WLE

  u3_prop <- function(x) {
    cf <- PerFit::U3poly(
      matrix = x,
      Ncat = max(as.matrix(x)) + 1, # make sure to input number of response categories, not thresholds
      IRT.PModel = "PCM",
      IP = params,
      Ability = thetas
    ) %>%
      PerFit::cutoff()

    cf$Prop.flagged
  }

  u3_results <- map_vec(1:100, ~ u3_prop(data))
  return(median(u3_results))

}
