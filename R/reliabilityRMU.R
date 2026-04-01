### The code below is borrowed from https://github.com/giac01/gbtoolbox/blob/main/R/reliability.R
### and slightly modified to be used with `RIreliabilityRMU()`.
### version used: https://github.com/giac01/gbtoolbox/commit/0e5c5d1547abc61c44f7c72c74c36d829241a33a

#' Estimate reliability (Relative Measurement Uncertainty) from Bayesian measurement models
#'
#' This function measures reliability using posterior draws from a fitted Bayesian model.
#'
#' To use this function, you will need to provide a matrix (input_draws) that contains the posterior draws for the parameter you wish to calculate reliability. The function assumes that rows of input_draws represent subjects and columns represent posterior draws.
#'
#' For an example of how to apply this function to calculate mean score reliability using brms, see \href{https://www.bignardi.co.uk/8_bayes_reliability/tutorial_rmu_sum_score_reliability.html}{this tutorial}.
#'
#' For an example of how to apply this function to go/go-no task data using brms, see \href{https://www.bignardi.co.uk/8_bayes_reliability/tutorial_calculating_rmu_gonogo.html}{this tutorial}.
#'
#' @param input_draws A matrix or data frame of posterior draws. Rows represent subjects and columns represent draws.
#' @param verbose Logical. Print detailed information about the input data. Default is TRUE.
#' @param level Numeric. Credibility level for the highest density continuous interval. Default is 0.95.
#'
#' @return A list containing:
#' \itemize{
#'   \item hdci: A data frame with a point-estimate (posterior mean) and highest density continuous interval for reliability, calculated using the ggdist::mean_hdci function
#'   \item reliability_posterior_draws: A numeric vector of posterior draws for reliability, of length K/2 (K = number of columns/draws in your input_draws matrix)
#' }
#'
#' @references
#' Bignardi, G., Kievit, R., & Bürkner, P. C. (2025). A general method for estimating reliability using Bayesian Measurement Uncertainty. PsyArXiv. \href{https://osf.io/preprints/psyarxiv/h54k8}{doi:10.31234/osf.io/h54k8}
#'
#' @examples
#' \dontrun{
#' # See https://www.bignardi.co.uk/8_bayes_reliability/tutorial_rmu_sum_score_reliability.html for more details on this example
#'
#' # Simulate data
#'
#' set.seed(1)
#' N                   = 5000 # number of subjects (mice)
#' J                   = 3    # number of measurements per subject
#' true_score_variance = 1
#' error_variance      = 10
#'
#' df = expand.grid(j = 1:J, mouse = 1:N)
#'
#' true_scores       = rnorm(N, mean = 10, sd = sqrt(true_score_variance))
#' measurement_error = rnorm(N*J, mean = 0, sd = sqrt(error_variance))
#'
#' df$measurement = true_scores[df$mouse] + measurement_error
#'
#' df_average_lengths = df %>%
#'   group_by(mouse) %>%
#'   summarise(average_measurement = mean(measurement))
#'
#' # Reliability should equal this:
#'
#' true_score_variance/(true_score_variance+error_variance/J)
#'
#' # Approximately the same as:
#'
#' cor(df_average_lengths$average_measurement, true_scores)^2
#'
#' # Fit model and calculate RMU
#'
#' brms_model = brm(
#'   measurement ~ 1 + (1 | mouse),
#'   data    = df
#' )
#'
#' # Extract posterior draws from brms model
#'
#' posterior_draws = brms_model %>%
#'   as_draws_df() %>%
#'   select(starts_with("r_mouse")) %>%
#'   t()
#'
#' # Calculate RMU
#'
#' reliability(posterior_draws)$hdci
#' }
#'
#'
#' @export
RMUreliability <- function(
    input_draws,
    verbose = FALSE,
    level   = .95
) {
  if (!is.numeric(level) || level <= 0 || level >= 1) stop("level must be a numeric value between 0 and 1")

  input_draws <- as.matrix(input_draws)

  # Check for columns with zero SD
  sds <- apply(input_draws, 2, stats::sd, na.rm = TRUE)
  zero_sd_cols <- which(sds == 0)
  if (length(zero_sd_cols) > 0) {
    warning(sprintf("Found %d column(s) with zero standard deviation (columns: %s)",
                    length(zero_sd_cols),
                    paste(zero_sd_cols, collapse = ", ")))
  }

  # Check for NAs in input
  na_count <- base::sum(base::is.na(input_draws))
  if (na_count > 0) {
    warning(sprintf("Found %d NA value(s) in input_draws", na_count))
  }

  if (verbose) {
    base::print(paste0("Number of subjects: ", nrow(input_draws)))
    base::print(paste0("Number of posterior draws: ", ncol(input_draws)))
  }

  col_select <- base::sample(1:ncol(input_draws), replace = FALSE)
  input_draws_1 <- input_draws[, col_select[1:floor(length(col_select) / 2)]]
  input_draws_2 <- input_draws[, col_select[(floor(length(col_select) / 2) + 1):length(col_select)]]

  # Calculate correlations and handle NAs
  reliability_posterior_draws <- sapply(1:ncol(input_draws_1), function(i) {
    x <- input_draws_1[, i]
    y <- input_draws_2[, i]

    # Return 0 if either column has zero variance
    if (stats::var(x, na.rm = TRUE) == 0 || stats::var(y, na.rm = TRUE) == 0) {
      return(0)
    }

    # Calculate correlation and handle NA
    cor_val <- stats::cor(x, y, method = "pearson")
    return(cor_val)
  })

  hdci <- ggdist::mean_hdci(reliability_posterior_draws, .width = level)

  colnames(hdci)[1] = "rmu_estimate"
  colnames(hdci)[2] = "hdci_lowerbound"
  colnames(hdci)[3] = "hdci_upperbound"

  return(hdci)
}


#' Reliability metrics
#'
#' Several metrics are reported, RMU, PSI, and 'empirical'. It is recommended to also
#' use the function `RIrelRep()` to evaluate conditional reliability. RMU seems
#' like the main metric to report.
#'
#' RMU, Relative Measurement Uncertainty:
#' This function uses the `mirt` library to estimate the Rasch model using
#' Marginal Maximum Likelihood and then generates plausible values
#' (PVs; Mislevy, 1991). The function uses borrowed code, see `?RMUreliability`.
#'
#' The PVs are then used with the RMU method described by Bignardi et al. (2025)
#' to estimate a mean and confidence interval. The mean is similar to
#' the expected a posteriori (EAP) reliability point estimate (Adams, 2005).
#' The confidence interval uses the 95% highest continuous density interval (HDCI)
#' based on the distribution of correlations.
#'
#' Default setting is to generate 1000 PVs. More are recommended for stable
#' estimates/CIs. How many more has not been systematically evaluated, but
#' 4000 might be a good starting point. For smaller samples, more PVs is not
#' very demanding computationally, but be wary of the time it takes to create
#' thousands of PVs for each respondent in large samples.
#'
#' PSI, Person Separation Index:
#' Estimated using functions in the `eRm` package, see `?eRm::SepRel`. Note that
#' this excludes min/max scoring individuals, which may result in unexpected
#' results, especially compared to other methods.
#'
#' Empirical:
#' Estimated using `mirt::empirical_rxx()`, see <https://stats.stackexchange.com/questions/427631/difference-between-empirical-and-marginal-reliability-of-an-irt-model>
#'
#' @examples
#' \dontrun{
#' # comparison of a fully Bayesian Rasch model and PVs
#' df <- eRm::raschdat1[,1:20] %>%
#'   rownames_to_column("id") %>%
#'   pivot_longer(!id, names_to = "item")
#'
#' library(brms)
#' brms_model <- brm(
#'   value ~ 1 + (1 | item) + (1 | id),
#'   data    = df,
#'   chains  = 4,
#'   cores   = 4,
#'   family = "bernoulli"
#' )
#'
#' posterior_draws <- brms_model %>%
#'   as_draws_df() %>%
#'   dplyr::select(starts_with("r_id")) %>%
#'   t()
#'
#' RMUreliability(posterior_draws)
#' RIreliability(eRm::raschdat1[,1:20], draws = 4000)
#' }
#'
#' @references
#' \itemize{
#'   \item Bignardi, G., Kievit, R., & Bürkner, P. C. (2025). A general method for estimating reliability using Bayesian Measurement Uncertainty. PsyArXiv. \href{https://osf.io/preprints/psyarxiv/h54k8}{doi:10.31234/osf.io/h54k8}
#'   \item Mislevy, R. J. (1991). Randomization-Based Inference about Latent Variables from Complex Samples. Psychometrika, 56(2), 177–196. \href{https://doi.org/10.1007/BF02294457}{doi:10.1007/BF02294457}
#'   \item Adams, R. J. (2005). Reliability as a measurement design effect. Studies in Educational Evaluation, 31(2), 162–172. \href{https://doi.org/10.1016/j.stueduc.2005.05.008}{doi:10.1016/j.stueduc.2005.05.008}
#' }
#'
#' @param data Dataframe/tibble with only item response data coded as integers
#' @param conf_int Desired confidence interval (HDCI)
#' @param draws Number of plausible values to generate
#' @param estim Estimation method for theta (latent scores)
#' @param boot Optional non-parametric bootstrap for empirical reliability
#' @param cpu Number of cpu cores to use for bootstrap method
#' @param pv Choice of R package. Optional "TAM", requires that you have TAM installed
#' @param iter Number of times the RMU estimation is done on the draws
#' @param verbose Set to `FALSE` to avoid the messages
#' @param theta_range The range of possible theta values
#' @export
RIreliability <- function(data, conf_int = .95, draws = 1000,
                          estim = "WLE", boot = FALSE, cpu = 4, pv = "mirt",
                          iter = 50, verbose = TRUE, theta_range = c(-10,10)) {

  if (verbose == TRUE) {
    message("Note that PSI is calculated with max/min scoring individuals excluded.")
    message(paste0("RMU reliability estimates based on ",draws," posterior draws (plausible values) from ",nrow(data)," respondents.",
                   "\nSee Bignardi, Kievit, & Bürkner (2025). 'A general method for estimating reliability using Bayesian Measurement Uncertainty' for details. ",
                   "The procedure has been modified in `easyRasch` to use plausible values based on an MML estimated Rasch model."))
  }

  if(min(as.matrix(data), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")
  } else if(max(as.matrix(data), na.rm = T) == 1) {
    model <- "RM"
  } else if(max(as.matrix(data), na.rm = T) > 1) {
    model <- "PCM"
  }

  if (model == "PCM") {
    mirt_out <- mirt(
      data,
      model = 1,
      itemtype = "Rasch",
      verbose = FALSE,
      accelerate = "squarem"
    )
    erm_out <- eRm::PCM(data)
  } else if (model == "RM") {
    mirt_out <- mirt(
      data,
      model = 1,
      itemtype = "1PL",
      verbose = FALSE,
      accelerate = "squarem"
    )
    erm_out <- eRm::RM(data)
  }

  # wle <- RI_iarm_person_estimates(erm_out, properties = TRUE, sthetarange = theta_range)[[2]] %>%
  #   as.data.frame()
  # rownames(wle) <- NULL

  empirical_rel <- mirt::fscores(mirt_out,
                                 method = estim,
                                 theta_lim = theta_range,
                                 full.scores.SE = TRUE,
                                 verbose = FALSE) %>%
    mirt::empirical_rxx()

  if (boot == TRUE) {
    registerDoParallel(cores = cpu)
    # bootstrap CI for empirical
    fit <- data.frame()
    fit <- foreach(i = 1:draws, .combine = rbind) %dopar% {

      dat <- data[sample(1:nrow(data), nrow(data), replace = TRUE), ]

      if (model == "PCM") {
        mirt_out2 <- mirt(
          dat,
          model = 1,
          itemtype = "Rasch",
          verbose = FALSE,
          accelerate = "squarem"
        )
      } else if (model == "RM") {
        mirt_out2 <- mirt(
          dat,
          model = 1,
          itemtype = "1PL",
          verbose = FALSE,
          accelerate = "squarem"
        )
      }

      mirt::fscores(mirt_out2,
                    method = estim,
                    theta_lim = theta_range,
                    full.scores.SE = TRUE,
                    verbose = FALSE) %>%
        mirt::empirical_rxx()

    }

    emp_boot <- mean_hdci(fit) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))

  }

  if (pv == "mirt") {
    plvals <- mirt::fscores(mirt_out, method = estim,
                            theta_lim = theta_range,
                            plausible.draws = draws,
                            plausible.type = "MH",
                            verbose = FALSE)

    rmu_draws <- do.call(cbind.data.frame, plvals)
    rmu_iter <- map_dfr(1:iter, ~ RMUreliability(rmu_draws, level = conf_int)[1:3])

    rmu <- rmu_iter %>%
      summarise(rmu_estimate = mean(rmu_estimate),
                hdci_lowerbound = mean(hdci_lowerbound),
                hdci_upperbound = mean(hdci_upperbound)
      ) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))

  } else if (pv == "TAM") {
    estim = "EAP"
    if (model == "PCM") {
      tam_out <- TAM::tam.mml(resp = data, irtmodel = "PCM", verbose = FALSE)
    } else if (model == "RM") {
      tam_out <- TAM::tam.mml(resp = data, irtmodel = "1PL", verbose = FALSE)
    }

    plvals <- TAM::tam.pv(tam_out, nplausible = draws, verbose = FALSE)[["pv"]][,-1]

    rmu_iter <- map_dfr(1:iter, ~ RMUreliability(plvals, level = conf_int)[1:3])

    rmu <- rmu_iter %>%
      summarise(rmu_estimate = mean(rmu_estimate),
                hdci_lowerbound = mean(hdci_lowerbound),
                hdci_upperbound = mean(hdci_upperbound)
      ) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))
  }

  if (boot == TRUE) {

    return(list(#WLE = wle,
      PSI = eRm::person.parameter(erm_out) %>% eRm::SepRel(),
      Empirical = paste0(estim,"_empirical = ",round(empirical_rel,3)),
      Empirical_bootstrap = paste0(estim,"_empirical = ",emp_boot$y," (95% HDCI [",emp_boot$ymin,", ",emp_boot$ymax,"]) (",draws," bootstrap resamples)"),
      RMU = paste0(estim,"-RMU = ",rmu$rmu_estimate," (95% HDCI [",rmu$hdci_lowerbound,", ",rmu$hdci_upperbound,"]) (",draws," draws) using package ",pv," and ",iter," RMU iterations.")
    )
    )
  } else {
    return(list(#WLE = wle,
      PSI = eRm::person.parameter(erm_out) %>% eRm::SepRel(),
      Empirical = paste0(estim,"_empirical = ",round(empirical_rel,3)),
      RMU = paste0(estim,"-RMU = ",rmu$rmu_estimate," (95% HDCI [",rmu$hdci_lowerbound,", ",rmu$hdci_upperbound,"]) (",draws," draws) using package ",pv," and ",iter," RMU iterations.")
    )
    )
  }
}

#' Conditional reliability using RMU
#'
#' Currently experimental function that creates a list object containing one table
#' and two plots showing the range of plausible values at each ordinal sum score
#' level (corresponds to possible theta values). Means and 95% HDCIs are shown in figures
#' and in table. One plot makes mean adjustments for WL estimate thetas, since the
#' plausible values are based on EAP estimates. This is similar to the bias adjustment made
#' when bootstrapping.
#'
#' @param data Dataframe with item responses
#' @param draws Number of plausible values to generate
#' @param n Number of persons to sample from each sum score level
#' @param conf_level Desired confidence interval (HDCI)
#' @param estim Estimation method for theta (latent scores)
#' @param theta_range The range of possible theta values
#' @export
#'
RIcrel <- function(data, draws = 500, n = 10, conf_level = 0.95, estim = "WLE",
                   theta_range = c(-6, 6)) {

  if (min(as.matrix(data), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")
  }
  else if (max(as.matrix(data), na.rm = T) == 1) {
    model <- "RM"
  }
  else if (max(as.matrix(data), na.rm = T) > 1) {
    model <- "PCM"
  }
  if (model == "PCM") {
    mirt_out <- mirt(data, model = 1, itemtype = "Rasch",
                     verbose = FALSE, accelerate = "squarem")
  }
  else if (model == "RM") {
    mirt_out <- mirt(data, model = 1, itemtype = "Rasch",
                     verbose = FALSE, accelerate = "squarem")
  }

  # create df with sumscores and rowids
  d <- data %>%
    mutate(sumscore = rowSums(.)) %>%
    tibble::rowid_to_column("rowid")

  # which sumscores are available in data?
  n_scores <- dplyr::count(d,sumscore)

  # make function to get rowids for each sumscore
  getrows <- function(x) {
    d %>%
      dplyr::filter(sumscore == {x}) %>%
      pull(rowid) %>%
      as.numeric()
  }

  rowids <- map(n_scores$sumscore, ~ getrows(.x))

  wlescores <- mirt::fscores(mirt_out, method = "WLE",
                             theta_lim = theta_range,
                             verbose = FALSE,
                             full.scores = FALSE) %>%
    as.data.frame() %>%
    arrange(F1) %>%
    mutate(wle = round(F1,3)) %>%
    distinct(wle) %>%
    mutate(raw_score = n_scores$sumscore)

  plvals <- mirt::fscores(mirt_out, method = "WLE",
                          theta_lim = theta_range,
                          plausible.draws = draws,
                          plausible.type = "MH",
                          verbose = FALSE)

  rmu_draws <- do.call(cbind.data.frame, plvals)
  names(rmu_draws) <- paste0("pv",1:draws)

  # make new df with each sumscore draws
  score_df <- function(x) {
    rmu_draws[rowids[[x]],] %>%
      slice(1:n) %>%
      tibble::rowid_to_column("rowid") %>%
      pivot_longer(!rowid) %>%
      add_column(score = x - 1)
  }

  plot_data <- map_dfr(1:nrow(n_scores), ~ score_df(.x))

  plot <- ggplot(plot_data,
                 aes(x = value, y = factor(score),
                     fill = factor(score))) +
    stat_halfeye(point_interval = "mean_hdci", .width = c(.84,.95)) +
    labs(y = "Ordinal sum score",
         caption = str_wrap("Note. Point and whiskers indicate mean and highest continuous density interval (HDCI) at 84% and 95%.")) +
    scale_fill_viridis_d(begin = 0.24) +
    theme_minimal(base_size = 14) +
    theme(axis.title = element_text(margin = margin(t = 12)),
          legend.position = "none",
          plot.caption = element_text(hjust = 0, face = "italic",size = 11))

  wle_adj_plot <-
    plot_data %>%
    left_join(wlescores, by = join_by("score" == "raw_score")) %>%
    group_by(score) %>%
    mutate(adj_value = value - (mean(value) - wle)) %>%
    ungroup() %>%

    ggplot(aes(x = adj_value, y = factor(score),
               fill = factor(score))) +
    stat_halfeye(point_interval = "mean_hdci", .width = c(.84,.95)) +
    labs(y = "Ordinal sum score",
         caption = str_wrap("Note. Point and whiskers indicate mean and highest continuous density interval (HDCI) at 84% and 95%.")) +
    scale_fill_viridis_d(begin = 0.24) +
    theme_minimal(base_size = 14) +
    theme(axis.title = element_text(margin = margin(t = 12)),
          legend.position = "none",
          plot.caption = element_text(hjust = 0, face = "italic",size = 11))

  table <- plot_data %>%
    group_by(score) %>%
    summarise(mean_hdci = mean_hdci(value)) %>%
    dplyr::select(!score) %>%
    unnest(cols = c(mean_hdci)) %>%
    add_column(score = n_scores$sumscore) %>%
    mutate(minmax_range = abs(ymax - ymin))

  list(table = table,
       plot = plot +
         labs(x = "EAP latent score",
              subtitle = "EAP plausible values"),
       wle_adj_plot = wle_adj_plot +
         labs(x = "WLE latent score and plausible values",
              subtitle = "WLE mean adjusted plausible values")
  )
}
