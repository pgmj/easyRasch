#' Fits a Rasch PCM using eRm, and conducts a PCA of residuals to get eigenvalues
#' using `psych::pca()` and reports the top 5 values.
#'
#' Proportion of explained variance is calculated using `stats::prcomp()`.
#'
#' Note from `?psych::pca`:
#' The eigenvectors are rescaled by the sqrt of the eigenvalues to produce
#' the component loadings more typical in factor analysis.
#'
#' Possible rotations are:
#' "none", "varimax", "quartimax", "promax", "oblimin", "simplimax", and "cluster".
#'
#' See Chou & Wang (2010, DOI: 10.1177/0013164410379322) for a simulation study
#' testing PCA eigenvalues across multiple conditions.
#'
#' @param dfin Dataframe with item data only
#' @param output Defaults to "table", optional "dataframe"
#' @param fontsize Set font size for table
#' @param maxiter Maximum number of iterations. Increase if convergence not obtained.
#' @param rotation Defaults to "oblimin"
#' @export
RIpcmPCA <- function(dfin, output = "table", fontsize = 15, maxiter = 5000,
                     rotation = "oblimin") {

  df.erm <- PCM(dfin) # run PCM model, replace with RSM (rating scale) or RM (dichotomous) for other models
  # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
  person.locations.estimate <- person.parameter(df.erm)
  item.fit <- eRm::itemfit(person.locations.estimate)
  std.resids <- item.fit$st.res

  if (ncol(dfin) > 5) {
    n_factors = 5
  } else {
    n_factors = ncol(dfin)
  }
  # PCA of Rasch residuals
  pca <- pca(std.resids, nfactors = n_factors,
             rotate = rotation, maxit = maxiter)
  # get proportion of explained variance
  pca2 <- prcomp(std.resids, rank. = n_factors)
  PoV <- pca2$sdev^2/sum(pca2$sdev^2)
  # create table with top (max) 5 eigenvalues
  table <- pca$values %>%
    round(2) %>%
    head(n_factors) %>%
    as.data.frame(nm = "Eigenvalues") %>%
    add_column("Proportion of variance" = paste0(round(100*PoV[1:n_factors],1),"%"))

  if (output == "table") {
    return(kbl_rise(table))
  }

  if (output == "quarto") {
    return(knitr::kable(table))
  }

  if (output == "dataframe") {
    return(table)
  }
}

#' Fits the Rasch model for dichotomous data using `eRm::RM()`, and
#' conducts a PCA of residuals to get eigenvalues.
#'
#' See `?RIpcmPCA` for more details.
#'
#' See Chou & Wang (2010, DOI: 10.1177/0013164410379322) for a simulation study
#' testing PCA eigenvalues across multiple conditions.
#'
#' @param data Dataframe with item data only
#' @param output Optional "dataframe" or "quarto"
#' @param fontsize Set font size
#' @param maxiter Maximum number of iterations. Increase if convergence not obtained.
#' @param rotation Defaults to "oblimin"
#' @export
RIrmPCA <- function(dfin, output = "table", fontsize = 15, maxiter = 5000,
                    rotation = "oblimin") {

  erm_out <- RM(data)
  ple <- eRm::person.parameter(erm_out)
  item.fit <- eRm::itemfit(ple)
  std.resids <- item.fit$st.res
  # PCA of Rasch residuals
  pca <- psych::pca(std.resids, nfactors = ncol(data),
                    rotate = rotation, maxit = maxiter)
  # create table with top 5 eigenvalues
  pca_table <- pca$values %>%
    round(2) %>%
    head(5) %>%
    as.data.frame(nm = "Eigenvalue")

  if (output == "table") {
    kbl_rise(pca_table, tbl_width = 30)
  } else if (output == "dataframe") {
    pca_table
  } else if (output == "quarto") {
    knitr::kable(pca_table)
  }
}

#' Bootstrapped Likelihood Ratio Test
#'
#' Non-parametric bootstrap use of `iarm::clr_tests()`. Intended for use with
#' large sample sizes.
#'
#' @param dat A dataframe with response data
#' @param iterations How many bootstrap samples to run
#' @param samplesize How large sample to use in each bootstrap
#' @param cpu How many CPU's to use
#' @export
RIbootLRT <- function(dat, iterations = 1000, samplesize = 300, cpu = 4) {

  if(min(as.matrix(dat), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")
  } else if (samplesize > nrow(dat)) {
    stop(paste0("`samplesize` (",samplesize,") cannot be larger than the number of rows in your data (",
                nrow(dat),")."))
  } else if(max(as.matrix(dat), na.rm = T) == 1) {
    model <- "RM"
  } else if(max(as.matrix(dat), na.rm = T) > 1) {
    model <- "PCM"
  }

  registerDoParallel(cores = cpu)

  # get vector of random seeds for reproducible simulations
  seeds <- c(.Random.seed, as.integer(.Random.seed + 1), as.integer(.Random.seed + 2))
  if (iterations > length(seeds)) {
    stopImplicitCluster()
    stop(paste0("Maximum possible iterations is ",length(seeds),"."))
  }

  fit <- data.frame()
  fit <- foreach(i = 1:iterations, .combine = rbind) %dopar% {
    # reproducible seed
    set.seed(seeds[i])

    data <- dat[sample(1:nrow(dat), samplesize, replace = TRUE), ]

    if (model == "PCM") {
      lrt_out <- iarm::clr_tests(data, model = "PCM")[3]
    } else if (model == "RM") {
      lrt_out <- iarm::clr_tests(data, model = "RM")[3]
    }

    as.numeric(lrt_out)

  }

  stopImplicitCluster()
  out <- fit %>%
    as.data.frame() %>%
    set_names("pvalue") %>%
    mutate(Result = ifelse(pvalue < .05, "Statistically significant", "Not statistically significant")) %>%
    dplyr::count(Result) %>%
    mutate(Percent = round(n*100/sum(n),1)) %>%
    knitr::kable()

  return(out)
}

#' Parametric bootstrap procedure for PCA of Rasch model residuals
#'
#' Estimates item and person parameters from data and simulates data fitting the
#' RM or PCM, then estimates the largest eigenvalue from residuals.
#'
#' Outputs an object with complete results under `$results` and
#' percentile values at 95%, 99%, 99.5%, and 99.9%.
#'
#' @param data Dataframe with item responses
#' @param iterations Number of bootstrap iterations
#' @param cpu Number of CPU cores to use
#' @param maxiter Maximum number of iterations
#' @param rotation Defaults to "oblimin"
#' @export
#'
RIbootPCA <- function(data, iterations = 200, cpu = 4, rotation = "oblimin",
                      maxiter = 5000) {

  sample_n <- nrow(data)
  items_n <- ncol(data)

  # get vector of random seeds for reproducible simulations
  seeds <- c(.Random.seed, as.integer(.Random.seed + 1))
  if (iterations > length(seeds)) {
    stop(paste0("Maximum possible iterations is ",length(seeds),"."))
  }

  if (min(as.matrix(data), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")
  } else if (max(as.matrix(data), na.rm = T) == 1 && min(as.matrix(data), na.rm = T) == 0) {
    # estimate item threshold locations from data
    erm_out <- eRm::RM(data, se = FALSE)
    item_locations <- erm_out$betapar * -1
    names(item_locations) <- names(data)

    # estimate theta values from data using WLE
    if (any(is.na(as.matrix(data))) == FALSE) {
      thetas <- RIestThetas(data)$WLE
    } else {
      mirt_out <- mirt(data, itemtype = "Rasch", verbose = FALSE)
      thetas <- mirt::fscores(mirt_out, method = "WLE", verbose = FALSE)
    }

    registerDoParallel(cores = cpu)
    fitstats <- list()
    fitstats <- foreach(i = 1:iterations) %dopar% {
      # reproducible seed
      set.seed(seeds[i])
      # resampled vector of theta values (based on sample properties)
      inputThetas <- sample(thetas, size = sample_n, replace = TRUE)

      # simulate response data based on thetas and items above
      testData <-
        psychotools::rrm(inputThetas, item_locations, return_setting = FALSE) %>%
        as.data.frame()

      # TEMPORARY FIX START
      # check that all items have at least 8 positive responses, otherwise eRm::RM() fails
      n_resp <-
        testData %>%
        as.matrix() %>%
        colSums2() %>%
        t() %>%
        as.vector()

      if (min(n_resp, na.rm = TRUE) < 8) {
        return("Missing cells in generated data.")
      }
      # END TEMP FIX

      erm_out <- RM(testData, se = FALSE)
      ple <- eRm::person.parameter(erm_out)
      item.fit <- eRm::itemfit(ple)
      std.resids <- item.fit$st.res
      pca <- psych::pca(std.resids, nfactors = ncol(data),
                        rotate = rotation, maxit = maxiter)
      eigenvalue <- pca$values %>% round(3) %>% head(1)

    }
  } else if (max(as.matrix(data), na.rm = T) > 1 && min(as.matrix(data), na.rm = T) == 0) {

    #check for n < 3 responses in any cell
    if(RIcheckdata(data) == TRUE) {
      # mirt is less unreliable than eRm in this situation
      mirt_out <- mirt(data, model=1, itemtype='Rasch', verbose = FALSE)
      item.locations <- coef(mirt_out, simplify = TRUE, IRTpars = TRUE)$items %>%
        as.data.frame() %>%
        dplyr::select(!a) %>%
        as.matrix()
      item.locations <- item.locations - mean(item.locations, na.rm = TRUE)
      maxcat <- data %>%
        pivot_longer(everything()) %>%
        dplyr::count(name,value) %>%
        pull(value) %>%
        max(na.rm = TRUE)
      item.locations <- item.locations %>%
        as.data.frame() %>%
        set_names(paste0("Threshold ", 1:maxcat))
      # person locations
      thetas <- RIestThetasCATr(data, itemParams = as.matrix(item.locations), cpu = cpu)

    } else if (RIcheckdata(data) == FALSE) {

      # person locations
      thetas <- RIestThetasCATr(data, cpu = cpu)

    }

    # estimate item threshold locations from data
    item_locations <- RIitemparams(data, output = "dataframe") %>%
      dplyr::select(!Location) %>%
      janitor::clean_names() %>%
      as.matrix()

    n_items <- nrow(item_locations)

    # item threshold locations in list format for simulation function
    itemlist <- list()
    for (i in 1:n_items) {
      itemlist[[i]] <- list(na.omit(item_locations[i, ]))
    }

    # get number of response categories for each item for later use in checking complete responses
    itemlength <- list()
    for (i in 1:n_items) {
      itemlength[i] <- length(na.omit(item_locations[i, ]))
      names(itemlength)[i] <- names(data)[i]
    }

    fitstats <- list()
    fitstats <- foreach(i = 1:iterations) %dopar% {
      # reproducible seed
      set.seed(seeds[i])
      # resampled vector of theta values (based on sample properties)
      inputThetas <- sample(thetas, size = sample_n, replace = TRUE)

      # simulate response data based on thetas and items above
      testData <- SimPartialScore(
        deltaslist = itemlist,
        thetavec = inputThetas
      ) %>%
        as.data.frame()

      names(testData) <- names(data)

      # check that data has responses in all categories
      data_check <- testData %>%
        # make factor to not drop any consecutive response categories with 0 responses
        mutate(across(everything(), ~ factor(.x, levels = c(0:itemlength[[as.character(expression(.x))]])))) %>%
        pivot_longer(everything()) %>% # screws up factor levels, which makes the next step necessary
        dplyr::count(name, value, .drop = FALSE) %>%
        pivot_wider(
          names_from = "name",
          values_from = "n"
        ) %>%
        dplyr::select(!value) %>%
        # mark missing cells with NA for later logical examination with if(is.na)
        mutate(across(everything(), ~ car::recode(.x, "0=NA", as.factor = FALSE))) %>%
        as.data.frame() %>%
        dplyr::select(all_of(names(data))) # get item sorting correct

      # match response data generated with itemlength
      item_ccount <- list()
      for (i in 1:n_items) {
        item_ccount[i] <- list(data_check[c(1:itemlength[[i]]), i])
      }

      # check if any item has 0 responses in a response category that should have data
      if (any(is.na(unlist(item_ccount)))) {
        return("Missing cells in generated data.")
      }

      # get PCA eigenvalue
      pcm_out <- eRm::PCM(testData, se = FALSE)
      ple <- eRm::person.parameter(pcm_out)
      item.fit <- eRm::itemfit(ple)
      std.resids <- item.fit$st.res
      pca <- psych::pca(std.resids, nfactors = ncol(data),
                        rotate = rotation, maxit = maxiter)
      eigenvalue <- pca$values %>% round(3) %>% head(1)

    }
  }

  df_fitstats <- unlist(fitstats) %>%
    as.data.frame(nm = "eigenvalue") %>%
    mutate(eigenvalue = car::recode(eigenvalue,"'Missing cells in generated data.'=NA"))

  if (any(is.na(df_fitstats$eigenvalue))) {
    missing <- df_fitstats %>%
      dplyr::count(eigenvalue) %>%
      mutate(percent = n*100/sum(n)) %>%
      filter(is.na(eigenvalue))
    warning(paste0(missing$n," simulated datasets (",missing$percent,"%) contained missing cells."))
  }

  df_fitstats <- na.omit(df_fitstats)

  stats <- list(results = df_fitstats,
                samplesize = sample_n,
                number_of_items = items_n,
                actual_iterations = nrow(df_fitstats),
                p95 = quantile(df_fitstats$eigenvalue, .95),
                p99 = quantile(df_fitstats$eigenvalue, .99),
                p995 = quantile(df_fitstats$eigenvalue, .995),
                p999 = quantile(df_fitstats$eigenvalue, .999),
                max = max(df_fitstats$eigenvalue))
  stopImplicitCluster()
  return(stats)
}
