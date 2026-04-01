#' Raw sum score to logit score transformation table & figure
#'
#' By default displays a table with raw sum scores and their corresponding logit score
#' and logit standard error. Depends on functions from package `iarm`.
#'
#' Automatically chooses PCM or RM depending on data structure.
#'
#' Optional figure or dataframe output.
#'
#' NOTE: the figure uses `coord_flip()`, take this into account if you wish to add theming.
#'
#' @param data Dataframe with item data only
#' @param output Options: "table" (default), "figure", or "dataframe"
#' @param point_size Point size for figure
#' @param error_width Width of error bar ends for figure
#' @param error_multiplier Range of error bars to multiply with SEM
#' @param ... Options for `kbl_rise()` for table creation
#' @export
RIscoreSE <- function(data, output = "table", point_size = 3,
                      error_width = 0.5, error_multiplier = 1.96, ...) {
  if(min(as.matrix(data), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")

  } else if(max(as.matrix(data), na.rm = T) == 1 && min(as.matrix(data), na.rm = T) == 0) {
    erm_out <- eRm::RM(data)
  } else if(max(as.matrix(data), na.rm = T) > 1 && min(as.matrix(data), na.rm = T) == 0) {
    erm_out <- eRm::PCM(data)
  }

  scoreList <- RI_iarm_person_estimates(erm_out, properties = TRUE)
  scoreTable <- scoreList[[2]] %>%
    as.data.frame() %>%
    dplyr::select(`Raw Score`, WLE, SEM) %>%
    dplyr::rename(`Logit score` = WLE,
                  `Ordinal sum score` = `Raw Score`,
                  `Logit std.error` = SEM)
  rownames(scoreTable) <- NULL

  if (output == "table") {
    scoreTable %>%
      round(3) %>%
      kbl_rise(...)
  } else if (output == "dataframe") {
    return(scoreTable)
  } else if (output == "figure") {
    ggplot(scoreTable, aes(`Ordinal sum score`, `Logit score`)) +
      geom_errorbar(aes(ymin = `Logit score` - (error_multiplier * `Logit std.error`),
                        ymax = `Logit score` + (error_multiplier * `Logit std.error`)),
                    width = error_width, color = "darkgrey"
      ) +
      geom_point(size = point_size, shape = 18) +
      scale_x_continuous() +
      scale_y_continuous() +
      coord_flip() +
      labs(x = "Ordinal sum score",
           y = "Logit interval score") +
      theme_bw()
  }
}


#' Person location estimation (`catR` version)
#'
#' Outputs a vector of person locations, one for each row in the dataframe.
#'
#' Uses `thetaEst()` function from `catR` package to estimate person locations
#' (thetas) for a dataframe with item data as columns and persons as rows.
#' Defaults to use WL estimation (lower bias than ML, see Warm, 1989).
#'
#' A version for multi-core processing is available as `RIestThetasCATr()`.
#'
#' @param data Dataframe with response data only (no demographics etc), items as columns
#' @param itemParams Optional item (threshold) location matrix
#' @param method Estimation method (defaults to "WL")
#' @param theta_range Range of theta (person location) values
#' @export
RIestThetasOLD <- function(data, itemParams, method = "WL",
                           theta_range = c(-10,10)) {

  if (min(as.matrix(data), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")

  } else if (max(as.matrix(data), na.rm = T) == 1) {
    model <- "RM"
  } else if (max(as.matrix(data), na.rm = T) > 1) {
    model <- "PCM"
  }

  # define function to call from purrr::map_dbl later.
  estTheta <- function(personResponse, itemParameters = itemParams, rmod = model,
                       est = method, rtheta = theta_range) {
    thetaEst(itemParameters, as.numeric(as.vector(personResponse)), model = rmod,
             method = est, range = rtheta)
  }
  # if no itemParams are given, calculate them based on input dataframe
  if (missing(itemParams) & model == "PCM") {
    erm_out <- PCM(data)
    itemParams <- thresholds(erm_out)[[3]][[1]][, -1] - mean(thresholds(erm_out)[[3]][[1]][, -1], na.rm=T)

    # Transpose dataframe to make persons to columns, then output a vector with thetas
    data %>%
      t() %>%
      as.data.frame() %>%
      map_dbl(., estTheta)

  } else if (missing(itemParams) & model == "RM") {
    df.erm <- RM(data)
    itemParams <- as.matrix(coef(df.erm, "beta")*-1)

    # Transpose dataframe to make persons to columns, then output a vector with thetas
    data %>%
      t() %>%
      as.data.frame() %>%
      map_dbl(., ~ estTheta(.x, rmod = NULL))

  } else if (!missing(itemParams) & model == "PCM") {

    # Transpose dataframe to make persons to columns, then output a vector with thetas
    data %>%
      t() %>%
      as.data.frame() %>%
      map_dbl(., estTheta)

  } else if (!missing(itemParams) & model == "RM") {
    df.erm <- RM(data)
    itemParams <- as.matrix(coef(df.erm, "beta")*-1)

    # Transpose dataframe to make persons to columns, then output a vector with thetas
    data %>%
      t() %>%
      as.data.frame() %>%
      map_dbl(., ~ estTheta(.x, rmod = NULL))

  }
}

#' Person location estimation
#'
#' Outputs a dataframe of person locations (theta) and measurement error (SEM)
#' for each person
#'
#' IMPORTANT: only use with complete response data. If you have missing item responses
#' `RIestThetasCATr()` or `RIestThetasOLD()` is recommended instead.
#'
#' Uses `iarm::person_estimates()` to estimate person locations
#' (thetas) for a dataframe with item data as columns and persons as rows.
#'
#' Defaults to use WLE estimation (lower bias than MLE, see Warm, 1989;
#' Kreiner, 2025) and PCM.
#'
#' Note: If you want to use a pre-specified set of item parameters, please use
#' `RIestThetasCATr()` or `RIestThetasOLD()`.
#'
#' @param data Dataframe with response data only (no demographics etc), items as columns
#' @param method Estimation method (defaults to "WLE")
#' @export
RIestThetas <- function(data, method = "WLE") {

  if(min(as.matrix(data), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")
  } else if (any(is.na(as.matrix(data)))) {
    stop("You have missing responses in your data. Use `RIestThetasCATr()` instead")

  } else if (max(as.matrix(data), na.rm = T) == 1) {
    model <- "RM"
  } else if (max(as.matrix(data), na.rm = T) > 1) {
    model <- "PCM"
  }

  if (model == "PCM") {
    erm_out <- PCM(data)
  } else if (model == "RM") {
    erm_out <- RM(data)
  }

  thetas <- RI_iarm_person_estimates(erm_out, allperson = TRUE) %>%
    as.data.frame()

  semList <- RI_iarm_person_estimates(erm_out, properties = TRUE)

  if (method == "WLE") {
    theta_df <- data.frame(WLE = thetas$WLE)
    sem_df <- semList[[2]] %>%
      as.data.frame() %>%
      dplyr::select(WLE,SEM)

    theta_df <- left_join(theta_df,sem_df, by = "WLE")

    return(theta_df)

  } else if (method == "MLE") {
    theta_df <- data.frame(MLE = thetas$MLE)
    sem_df <- semList[[1]] %>%
      as.data.frame() %>%
      dplyr::select(MLE,SEM)

    theta_df <- left_join(theta_df,sem_df, by = "MLE")

    return(theta_df)
  }
}

#' Person location estimation with parallel processing
#'
#' Yields about 2-3x speed increase when using 4-8 CPU cores.
#' Requires `library(furrr)`
#'
#' Outputs a vector of person locations, one for each row in the dataframe.
#'
#' Uses thetaEst function from catR package to estimate person locations
#' (thetas) for a dataframe with item data as columns and persons as rows.
#' Defaults to use WL estimation (lower bias than ML, see Warm, 1989).
#'
#' @param data Dataframe with response data only (no demographics etc), items as columns
#' @param itemParams Optional item (threshold) location matrix
#' @param method Estimation method (defaults to `"WL"`)
#' @param cpu Number of CPUs/cores to utilize (default is 4)
#' @param theta_range Range of theta (person location) values
#' @export
RIestThetasCATr <- function(data, itemParams, method = "WL", cpu = 4,
                            theta_range = c(-10,10)) {

  if (min(as.matrix(data), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")

  } else if (max(as.matrix(data), na.rm = T) == 1) {
    #model <- "RM"
    stop("Dichotomous models not yet supported.")
  } else if (max(as.matrix(data), na.rm = T) > 1) {
    model <- "PCM"
  }

  library(furrr) # should probably not be loaded inside the package?
  plan(multisession, workers = cpu)
  # define function to call from purrr::map_dbl later.
  estTheta <- function(personResponse, itemParameters = itemParams, rmod = model,
                       est = method, rtheta = theta_range) {
    thetaEst(itemParameters, as.numeric(as.vector(personResponse)), model = rmod,
             method = est, range = rtheta)
  }
  # if no itemParams are given, calculate them based on input dataframe
  if (missing(itemParams) & model == "PCM") {
    erm_out <- PCM(data)
    itemParams <- thresholds(erm_out)[[3]][[1]][, -1] - mean(thresholds(erm_out)[[3]][[1]][, -1], na.rm=T)

    # Transpose dataframe to make persons to columns, then output a vector with thetas
    estimated_thetas <-
      data %>%
      t() %>%
      as.data.frame() %>%
      future_map_dbl(., estTheta)

  } else if (missing(itemParams) & model == "RM") {
    df.erm <- RM(data)
    itemParams <- as.matrix(coef(df.erm, "beta")*-1)

    # Transpose dataframe to make persons to columns, then output a vector with thetas
    estimated_thetas <-
      data %>%
      t() %>%
      as.data.frame() %>%
      future_map_dbl(., ~ estTheta(.x, rmod = NULL))

  } else if (!missing(itemParams) & model == "PCM") {

    # Transpose dataframe to make persons to columns, then output a vector with thetas
    estimated_thetas <-
      data %>%
      t() %>%
      as.data.frame() %>%
      future_map_dbl(., estTheta)

  } else if (!missing(itemParams) & model == "RM") {
    # Transpose dataframe to make persons to columns, then output a vector with thetas
    estimated_thetas <-
      data %>%
      t() %>%
      as.data.frame() %>%
      future_map_dbl(., ~ estTheta(.x, rmod = NULL))
  }
  plan(sequential)
  return(estimated_thetas)

}
