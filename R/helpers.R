#' A simple ggplot theme for RISE formatting
#'
#' Use is optional :)
#'
#' See ?element_text for more details on available settings.
#'
#' Please note that using this theme does not update the session defaults for
#' geom_text and geom_text_repel. You can add the relevant line(s) manually:
#'
#'     update_geom_defaults("text", list(family = fontfamily)) +
#'     update_geom_defaults("text_repel", list(family = fontfamily))
#'
#' @param fontfamily Font family for all plot text
#' @param axissize Font size for axis labels
#' @param titlesize Font size for plot title
#' @param margins Distance of axis labels to plot
#' @param axisface Set to "bold" if you want bold axis labels
#' @return Add + theme_rise() to your ggplot or RIfunction that outputs a ggplot
#' @export
theme_rise <- function(fontfamily = "Lato", axissize = 13, titlesize = 15,
                       margins = 12, axisface = "plain", panelDist = 0.6, ...) {
  theme_minimal() +
    theme(
      text = element_text(family = fontfamily),
      axis.title.x = element_text(
        margin = margin(t = margins),
        size = axissize
      ),
      axis.title.y = element_text(
        margin = margin(r = margins),
        size = axissize
      ),
      plot.title = element_text(
        face = "bold",
        size = titlesize
      ),
      axis.title = element_text(
        face = axisface
      ),
      plot.caption = element_text(
        hjust = 0,
        face = "italic"
      ),
      legend.text = element_text(family = fontfamily),
      legend.background = element_rect(color = "lightgrey"),
      strip.background = element_rect(color = "lightgrey"),
      panel.spacing = unit(panelDist, "cm", data = NULL),
      panel.border = element_rect(color = "grey", fill = NA),
      ...
    )
}

#' A kableExtra function to simplify table creation
#'
#' @param data Dataframe/tibble to create table from
#' @param tbl_width Width of table (0-100)
#' @param fontsize Font size
#' @param fontfamily Font family
#' @param ... Passes options to kbl()
#' @export
kbl_rise <- function(data, tbl_width = 65, fontsize = 14, fontfamily = "Arial",
                     options = c("striped", "hover"), ...) {
  kbl(data, booktabs = T, escape = F,
      table.attr = paste0("data-quarto-disable-processing='true' style='width:",tbl_width,"%;'")) %>%
    kable_styling(
      bootstrap_options = options,
      position = "left",
      full_width = T,
      font_size = fontsize,
      fixed_thead = T,
      latex_options = c("striped", "scale_down"),
      ...
    ) %>%
    row_spec(0, bold = T) %>%
    kable_classic(html_font = fontfamily)
}

#' Parametric bootstrapping function that outputs simulated datasets
#'
#' For generic use. Outputs datasets to a list object.
#'
#' @param data A dataframe with response data
#' @param iterations How many datasets to generate
#' @param cpu Number of CPU cores to use
#' @export
RIpboot <- function(data, iterations, cpu = 4) {
  sample_n <- nrow(data)

  registerDoParallel(cores = cpu)

  # get vector of random seeds for reproducible simulations
  seeds <- c(.Random.seed, as.integer(.Random.seed + 1))
  if (iterations > length(seeds)) {
    stop(paste0("Maximum possible iterations is ",length(seeds),"."))
  }

  if (missing(iterations)) {
    stop("Please set a number of iterations.")
  }

  if (min(as.matrix(data), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")
  } else if (max(as.matrix(data), na.rm = T) == 1 && min(as.matrix(data), na.rm = T) == 0) {
    # estimate item threshold locations from data
    erm_out <- eRm::RM(data)
    item_locations <- erm_out$betapar * -1
    names(item_locations) <- names(data)

    # estimate theta values from data using WLE
    thetas <- RIestThetas(data)

    datasets <- list()
    datasets <- foreach(i = 1:iterations) %dopar% {

      # reproducible seed
      set.seed(seeds[i])
      # resampled vector of theta values (based on sample properties)
      inputThetas <- sample(thetas$WLE, size = sample_n, replace = TRUE)

      # simulate response data based on thetas and items above
      psychotools::rrm(inputThetas, item_locations, return_setting = FALSE) %>%
        as.data.frame()
    }
  } else if (max(as.matrix(data), na.rm = T) > 1 && min(as.matrix(data), na.rm = T) == 0) {
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

    # estimate theta values from data using WLE
    thetas <- RIestThetas(data)

    datasets <- list()
    datasets <- foreach(i = 1:iterations) %dopar% {

      # reproducible seed
      set.seed(seeds[i])
      # resampled vector of theta values (based on sample properties)
      inputThetas <- sample(thetas$WLE, size = sample_n, replace = TRUE)

      # simulate response data based on thetas and items above
      testData <- SimPartialScore(
        deltaslist = itemlist,
        thetavec = inputThetas
      ) %>%
        as.data.frame()
      names(testData) <- names(data)
      testData
    }
  }
}
