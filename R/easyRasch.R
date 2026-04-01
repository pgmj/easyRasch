### easyRasch R package, https://github.com/pgmj/easyRasch
### (formerly known as `RISEkbmRasch`)
### Created by pgmj@pm.me ORCID: 0000-0003-1669-592X
### The contents of this file is licensed according to the MIT License

### See https://pgmj.github.io/raschrvignette/RaschRvign.html for vignette.


#' Individual item category probability plots
#'
#' Uses `eRm::PCM()` and `eRm::plotICC()`.
#'
#' @param data Dataframe with item data only
#' @param items Optionally a single item `"q4"`, or a vector `c("q4","q2")`
#' @param xlims Start/end point for x-axis
#' @param legend Set to a position such as "left" if desired
#' @export
RIitemCats <- function(data, items = "all", xlims = c(-6,6), legend = FALSE) {

  erm_out <- PCM(data) # run PCM(), Rasch partial credit model

  plotICC(erm_out,
    xlim = xlims, # change the x axis theta interval to display
    legpos = legend, # change legpos to TRUE if you want the legend displayed
    ylab = "Probability",
    xlab = "Person location (logit scale)",
    item.subset = items,
    ask = FALSE
  )
}


#' Targeting, Wright map derivative.
#'
#' Outputs a figure consisting of three figures with the
#' same scale on top of each other.
#' At the top is a histogram of Person Locations, with a dotted line and
#' gray field indicating mean/SD. In the middle is a similar histogram with
#' Item Thresholds. At the bottom is a figure showing the individual
#' item thresholds as dots.
#'
#' The figure is made up from three figures using library(patchwork). If desired,
#' you can output the three figures to a list object instead of a single figure.
#' This allows you to modify each figure (change theming, colors, etc). You can
#' put together the three figures into one using patchwork:
#'
#' `list$p1 / list$p2 / list$p3 + plot_layout(heights = c(1, 1, 1.4))`
#'
#' @param dfin Dataframe with item data only
#' @param model Defaults to "PCM", use "RM" for dichotomous data
#' @param xlim Optionally, set lower/upper limits for x axis
#' @param output Default "figure", or "list" to output 3 figures to a list object
#' @param bins Optionally, set number of bins for histograms
#' @param fast_thetas If you want fast output, but slightly less accurate person locations
#' @export
RItargeting <- function(dfin, model = "PCM", xlim = c(-4,4), output = "figure", bins = 30, fast_thetas = FALSE) {
  if(RIcheckdata(dfin) == TRUE) {
    warning("Warning! Your data has less than 3 responses in some response categories. Results may not be reliable.")
  }

  if(model == "PCM") {
    if(max(as.matrix(dfin), na.rm = TRUE) == 1) {
      stop("Use `model = 'RM'` for dichotomous data.")
    } else {
      if(RIcheckdata(dfin) == TRUE) {
        # mirt is less unreliable than eRm in this situation
        mirt_out <- mirt(dfin, model=1, itemtype='Rasch', verbose = FALSE)
        item.locations <- coef(mirt_out, simplify = TRUE, IRTpars = TRUE)$items %>%
          as.data.frame() %>%
          dplyr::select(!a) %>%
          as.matrix()
        item.locations <- item.locations - mean(item.locations, na.rm = TRUE)
        maxcat <- dfin %>%
          pivot_longer(everything()) %>%
          dplyr::count(name,value) %>%
          pull(value) %>%
          max(na.rm = TRUE)
        item.locations <- item.locations %>%
          as.data.frame() %>%
          set_names(paste0("Threshold ", 1:maxcat))
        # person locations
        pthetas <- RIestThetasCATr(dfin, itemParams = as.matrix(item.locations))

      } else if (RIcheckdata(dfin) == FALSE) {
        erm_out <- PCM(dfin) # run PCM model
        item.locations <- as.data.frame(thresholds(erm_out)[[3]][[1]][, -1] - mean(thresholds(erm_out)[[3]][[1]][, -1], na.rm=T))
        if (fast_thetas == TRUE) {
          pthetas <- RIestThetas(dfin)$WLE
        } else {
        # person locations
        pthetas <- RIestThetasCATr(dfin)
        }
      }

      names(item.locations) <- paste0("t", c(1:ncol(item.locations))) # re-label variables
      itemloc.long <- item.locations %>%
        rownames_to_column() %>%
        dplyr::rename(names = "rowname") %>%
        mutate(names = factor(names, levels = rev(names(dfin)))) %>%
        pivot_longer(
          cols = starts_with("t"),
          names_to = "thresholds",
          values_to = "par_values"
        )
      ### create df for ggplot histograms

      # check if xlim upper is below the highest person locations/thetas
      # and adjust if needed
      if (max(pthetas, na.rm = TRUE) > xlim[2]) {
        xlim[2] <- ceiling(max(pthetas, na.rm = TRUE))
      }
      # check if xlim lower is above the lowest person locations/thetas
      # and adjust if needed
      if (min(pthetas, na.rm = TRUE) < xlim[1]) {
        xlim[1] <- floor(min(pthetas, na.rm = TRUE))
      }
      # and then check if any item threshold is outside xlim
      if (max(itemloc.long$par_values, na.rm = TRUE) > xlim[2]) {
        xlim[2] <- ceiling(max(itemloc.long$par_values, na.rm = TRUE))
      }

      if (min(itemloc.long$par_values, na.rm = TRUE) < xlim[1]) {
        xlim[1] <- floor(min(itemloc.long$par_values, na.rm = TRUE))
      }

      # item locations
      thresholds <- c()
      for (i in 1:ncol(item.locations)) {
        thresholds <- c(thresholds, item.locations[, i])
      }
      ### items and persons in the same variable
      #create data frame with 0 rows and 3 columns
      df.locations <- data.frame(matrix(ncol = 2, nrow = 0))
      # provide column names
      colnames(df.locations) <- c("type", "locations")
      # change type of data
      df.locations$type <- as.character(df.locations$type)
      df.locations$locations <- as.numeric(df.locations$locations)
      # insert labels in accurate amounts (N+items)
      nper <- nrow(dfin)
      nperp <- nper + 1
      nthr <- length(thresholds) + nper
      df.locations[1:nper, 1] <- paste0("Persons")
      df.locations[nperp:nthr, 1] <- paste0("Item thresholds")
      # insert data from vectors with thetas and thresholds
      df.locations$locations <- c(pthetas, thresholds)
      # change type to class factor
      df.locations$type <- as.factor(df.locations$type)

      # get mean/SD for item/person locations
      item.mean <- round(mean(as.matrix(item.locations), na.rm = TRUE), 2)
      item.sd <- round(sd(as.matrix(item.locations), na.rm = TRUE), 2)
      item.thresh.sd <- round(sd(as.matrix(item.locations), na.rm = TRUE), 2)
      person.mean <- round(mean(pthetas, na.rm = TRUE), 2)
      person.sd <- round(sd(pthetas, na.rm = TRUE), 2)

      targeting_plots <- list()

      # Person location histogram
      targeting_plots$p1 <- ggplot() +
        geom_histogram(
          data = subset(df.locations, type == "Persons"),
          aes(locations, fill = "Persons"),
          bins = bins
        ) +
        xlab("") +
        ylab("Persons") +
        scale_x_continuous(limits = xlim, breaks = scales::breaks_extended(n = 10)) +
        geom_vline(xintercept = person.mean, color = "#0e4e65", linetype = 2) +
        annotate("rect", ymin = 0, ymax = Inf, xmin = (person.mean - person.sd), xmax = (person.mean + person.sd), alpha = .2) +
        theme_bw() +
        theme(legend.position = "none")

      # Item Threshold location histogram
      targeting_plots$p2 <- ggplot() +
        geom_histogram(
          data = subset(df.locations, type == "Item thresholds"),
          aes(locations, y = after_stat(count))
        ) +
        xlab("") +
        ylab("Thresholds") +
        scale_x_continuous(limits = xlim, breaks = scales::breaks_extended(n = 10)) +
        scale_y_reverse(breaks = ~round(unique(pretty(.))), minor_breaks = NULL) +
        geom_vline(xintercept = item.mean, color = "#e83c63", linetype = 2) +
        annotate("rect", ymin = 0, ymax = Inf, xmin = (item.mean - item.thresh.sd), xmax = (item.mean + item.thresh.sd), alpha = .2) +
        theme_bw() +
        theme(legend.position = "none")

      # make plot with each items thresholds shown as dots
      targeting_plots$p3 <- ggplot(itemloc.long, aes(x = names, y = par_values, label = thresholds, color = thresholds)) +
        geom_point() +
        geom_text(hjust = 1.2, vjust = 1) +
        scale_color_viridis_d(option = "H", end = 0.97) +
        scale_y_continuous(limits = xlim, breaks = scales::breaks_extended(n = 10)) +
        coord_flip() +
        labs(y = "Location (logit scale)",
             x = "Items",
             caption = paste0(
               "Person location average: ", person.mean, " (SD ", person.sd, "), Item threshold location average: ",
               item.mean, " (SD ", item.thresh.sd, "). Sample size: ",nrow(dfin),"."
             )) +
        theme_bw() +
        theme(plot.caption = element_text(hjust = 0, face = "italic"),
              legend.position = "none")

    }
  } else if (model == "RM") {

    erm_out <- RM(dfin) # run RM model
    item.estimates <- coef(erm_out, "beta")*-1 # item coefficients
    # person locations
    pthetas <- iarm::person_estimates(erm_out, allperson = TRUE) %>%
      as.data.frame() %>%
      pull(WLE)

    item.locations <- as.data.frame(item.estimates)
    itemloc.long <- item.locations %>%
      rownames_to_column() %>%
      tidyr::separate(rowname, c(NA, "names"), sep = " ")

    ### create df for ggplot histograms

    # item locations
    thresholds <- itemloc.long$item.estimates
    ### items and persons in the same variable
    #create data frame with 0 rows and 3 columns
    df.locations <- data.frame(matrix(ncol = 2, nrow = 0))
    #provide column names
    colnames(df.locations) <- c('type', 'locations')
    # change type of data
    df.locations$type<-as.character(df.locations$type)
    df.locations$locations<-as.numeric(df.locations$locations)
    # insert labels in accurate amounts (N+items)
    nper<-nrow(dfin)
    nperp<-nper+1
    nthr<-length(thresholds)+nper
    df.locations[1:nper,1]<-paste0("Persons")
    df.locations[nperp:nthr,1]<-paste0("Item thresholds")
    # insert data from vectors with thetas and thresholds
    df.locations$locations<-c(pthetas,thresholds)
    # change type to class factor
    df.locations$type<-as.factor(df.locations$type)

    # get mean/SD for item/person locations
    pi.locations <- data.frame(matrix(ncol = 3, nrow = 3))
    item_difficulty <- as.data.frame(item.estimates) %>%
      rownames_to_column() %>%
      dplyr::rename(Item = 'rowname', Location = 'item.estimates')

    #
    item.mean <- round(mean(item_difficulty$Location),2)
    item.sd <- round(sd(item_difficulty$Location),2)
    person.mean <- round(mean(pthetas, na.rm = TRUE),2)
    person.sd <- round(sd(pthetas, na.rm = TRUE),2)
    #provide column names
    colnames(pi.locations) <- c('','Mean', 'SD')
    pi.locations[1,1] <- "Items"
    pi.locations[1,2] <- round(mean(item_difficulty$Location),2)
    pi.locations[1,3] <- round(sd(item_difficulty$Location),2)
    pi.locations[2,1] <- "Persons"
    pi.locations[2,2] <- round(mean(pthetas, na.rm = TRUE),2)
    pi.locations[2,3] <- round(sd(pthetas, na.rm = TRUE),2)

    targeting_plots <- list()

    # Person location histogram
    targeting_plots$p1 <- ggplot() +
      geom_histogram(data=subset(df.locations, type=="Persons"),
                     aes(locations, fill="Persons"),
                     bins = bins
      ) +
      xlab('') +
      ylab('Persons') +
      scale_x_continuous(limits = xlim, breaks = scales::breaks_extended(n = 10)) +
      geom_vline(xintercept = person.mean, color = "#0e4e65", linetype = 2) +
      annotate("rect", ymin = 0, ymax = Inf, xmin = (person.mean-person.sd), xmax = (person.mean+person.sd), alpha = .2) +
      theme_bw() +
      theme(legend.position = 'none')

    # Item Threshold location histogram
    targeting_plots$p2 <- ggplot() +
      geom_histogram(data=subset(df.locations, type=="Item thresholds"),
                     aes(locations, y= after_stat(count))) +
      labs(x = "",
           y = "Items aggregated") +
      scale_x_continuous(limits = xlim, breaks = scales::breaks_extended(n = 10)) +
      scale_y_reverse(breaks = ~round(unique(pretty(.))), minor_breaks = NULL) +
      geom_vline(xintercept = item.mean, color = "#e83c63", linetype = 2) +
      annotate("rect", ymin = 0, ymax = Inf, xmin = (item.mean-item.sd), xmax = (item.mean+item.sd), alpha = .2) +
      geom_text(hjust = 1.2, vjust = 1) +
      theme_bw() +
      theme(legend.position = 'none')

    # Plot with each item's thresholds shown as dots
    targeting_plots$p3 <- itemloc.long %>%
      mutate(names = factor(names, levels = names(dfin), labels = names(dfin))) %>%
      ggplot(aes(x = fct_rev(names), y = item.estimates, label = names, color = names)) +      geom_point() +
      geom_text(hjust = 1.2, vjust = 1) +
      scale_y_continuous(limits = xlim, breaks = scales::breaks_extended(n = 10)) +
      theme_bw() +
      coord_flip() +
      labs(y = "Location (logit scale)",
           x = "Items",
           caption = paste0("Person location average: ", pi.locations[2,2], " (SD ", pi.locations[2,3],"), Item location average: ",
                            pi.locations[1,2], " (SD ", pi.locations[1,3], "). Sample size: ",nrow(dfin),"."
           )) +
      theme(plot.caption = element_text(hjust = 0, face = "italic"),
            legend.position = 'none')

  }

  if (output == "figure") {
    # combine plots together to create Wright map, and let the individual item threshold plot have some more space
    targeting_plots$p1 / targeting_plots$p2 / targeting_plots$p3 + plot_layout(heights = c(1, 1, 1.4))
  } else if (output == "list") {
    return(targeting_plots)
  }
}


#' Item parameters summary
#'
#' Displays a table with item threshold locations. Can also output a dataframe or
#' a CSV file.
#'
#' Currently only works with the Partial Credit Model (polytomous data).
#'
#' @param dfin Dataframe with item data only
#' @param fontsize Option to set font size for table
#' @param output Defaults to "table, can be set to "dataframe" or "file"
#' @param detail Set to "all" to get more detailed summary output
#' @param filename Name of file to save output to
#' @param tbl_width Width of table
#' @export
RIitemparams <- function(dfin, fontsize = 15, output = "table",
                         detail = "thresholds", filename = "item_params.csv",
                         tbl_width = 90) {

  if(RIcheckdata(dfin) == TRUE) {
    warning("Warning! Your data has less than 3 responses in some response categories. Results may not be reliable.")
    # mirt is less unreliable than eRm in this situation
    mirt_out <- mirt(dfin, model=1, itemtype='Rasch', verbose = FALSE)
    item.locations <- coef(mirt_out, simplify = TRUE, IRTpars = TRUE)$items %>%
      as.data.frame() %>%
      dplyr::select(!a) %>%
      as.matrix()
    item.locations <- item.locations - mean(item.locations, na.rm = TRUE)
    maxcat <- dfin %>%
      pivot_longer(everything()) %>%
      dplyr::count(name,value) %>%
      pull(value) %>%
      max(na.rm = TRUE)
    item.locations <- item.locations %>%
      as.data.frame() %>%
      set_names(paste0("Threshold ", 1:maxcat))

  } else if(RIcheckdata(dfin) == FALSE) {
    erm_out <- PCM(dfin)
    item.locations <- as.data.frame(thresholds(erm_out)[[3]][[1]][, -1] - mean(thresholds(erm_out)[[3]][[1]][, -1], na.rm=T))
  }

  item_difficulty <- item.locations %>%
    mutate(Location = rowMeans(., na.rm = TRUE), .before = `Threshold 1`) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))

  # detailed df
  item_params <- item_difficulty %>%
    mutate(all_item_avg = mean(Location)) %>%
    mutate(relative_avg_loc = Location - all_item_avg) %>%
    mutate(relative_lowest_tloc = `Threshold 1` - all_item_avg) %>%
    rownames_to_column("itemnr")

  # get the highest threshold value from each item - since the number of thresholds can vary, this needs special treatment
  highest_loc <- item_params %>%
    pivot_longer(cols = starts_with("Threshold"),
                 names_to = "threshold",
                 values_to = "t_location") %>%
    group_by(itemnr) %>%
    dplyr::filter(t_location == max(t_location, na.rm = TRUE)) %>%
    ungroup() %>%
    dplyr::select(itemnr, t_location) %>%
    dplyr::rename(highest_tloc = t_location)

  # join the highest threshold location to the item_params df
  item_params <- item_params %>%
    left_join(highest_loc, by = "itemnr") %>%
    mutate(relative_highest_tloc = highest_tloc - all_item_avg) %>%
    dplyr::relocate(all_item_avg, .after = relative_highest_tloc) %>%
    dplyr::select(-highest_tloc) %>%
    as.data.frame()

  if (output == "file" & detail == "thresholds") {
    item_difficulty %>%
      dplyr::select(!Location) %>%
      set_names(paste0("threshold_", 1:ncol(.))) %>%
      write_csv(., file = filename)
  }
  else if (output == "file" & detail == "all") {
    item_params %>%
      write_csv(., file = filename)
  }
  else if (output == "dataframe" & detail == "thresholds") {
    return(item_difficulty)
  }
  else if (output == "dataframe" & detail == "all") {
    return(item_params)
  }
  else if (output == "table" & detail == "thresholds") {
    item_difficulty %>%
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      dplyr::relocate(Location, .after = last_col()) %>%
      mutate(Location = cell_spec(Location, bold = T, align = "right")) %>%
      dplyr::rename('Item location' = Location) %>%
      kbl(booktabs = T, escape = F,
          table.attr = glue("data-quarto-disable-processing='true' style='width:{tbl_width}%;'")) %>%
      # bootstrap options are for HTML output
      kable_styling(bootstrap_options = c("striped", "hover"),
                    position = "left",
                    full_width = F,
                    font_size = fontsize,
                    fixed_thead = T) %>% # when there is a long list in the table
      column_spec(1, bold = T) %>%
      kable_classic(html_font = "Lato") %>%
      # for latex/PDF output
      kable_styling(latex_options = c("striped","scale_down")) %>%
      kableExtra::footnote(general = "Item location is the average of the thresholds for each item.")
  }
  else if (output == "table" & detail == "all") {
    item_params %>%
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      mutate(Location = cell_spec(Location, bold = T, align = "right")) %>%
      dplyr::rename('Item location' = Location,
                    'Relative item location' = relative_avg_loc,
                    'Relative lowest threshold' = relative_lowest_tloc,
                    'Relative highest threshold' = relative_highest_tloc) %>%
      dplyr::select(!all_item_avg) %>%
      kbl(booktabs = T, escape = F,
          table.attr = glue("data-quarto-disable-processing='true' style='width:{tbl_width}%;'")) %>%
      # bootstrap options are for HTML output
      kable_styling(bootstrap_options = c("striped", "hover"),
                    position = "left",
                    full_width = F,
                    font_size = fontsize,
                    fixed_thead = T) %>% # when there is a long list in the table
      column_spec(1, bold = T) %>%
      kable_classic(html_font = "Lato") %>%
      kable_styling(latex_options = c("striped","scale_down")) %>%
      kableExtra::footnote(general = "Item location is the average of the thresholds for each item.
      Relative item location is the difference between the item location and the average of the item locations for all items.
               Relative lowest threshold is the difference between the lowest threshold and the average of all item locations.
               Relative highest threshold is the difference between the highest threshold and the average of all item locations.")
  }

}

#' Generates a plot showing the first residual contrast loadings based on a PCA
#' of Rasch model residuals vs item locations.
#'
#' Defaults to PCM, use `model = "RM"` for dichotomous data.
#'
#' Note. This function does not work with missing responses in the dataset.
#' Missing data is automatically filtered out.
#'
#' @param dfin Dataframe with item data only
#' @param output Either "figure" (default) or "dataframe"
#' @param model Defaults to "PCM", use "RM" for dichotomous data
#' @param pcx Number of principal components to output for "dataframe"
#' @export
#' @return A plot with item locations (y) and loadings (x)
RIloadLoc <- function(dfin, output = "figure", pcx = c("PC1","PC2","PC3"), model = "PCM") {

  dfin <- na.omit(dfin)

  if(model == "PCM") {
    if(max(as.matrix(dfin), na.rm = TRUE) == 1) {
      stop("Use `model = 'RM'` for dichotomous data.")
    } else {
    erm_out <- PCM(dfin)
    item.locations <- as.data.frame(thresholds(erm_out)[[3]][[1]][, -1] - mean(thresholds(erm_out)[[3]][[1]][, -1], na.rm=T))
    item_difficulty <- item.locations %>%
      mutate(Location = rowMeans(., na.rm = TRUE), .before = `Threshold 1`) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))
    }

  } else if (model == "RM") {
    erm_out <- RM(dfin)
    item_difficulty <- as.data.frame(coef(erm_out, "beta")*-1)
    names(item_difficulty) <- "Location"
  }

  ple <- person.parameter(erm_out)
  item.fit <- eRm::itemfit(ple)
  std.resids <- item.fit$st.res

  pca2 <- prcomp(std.resids)
  pcaloadings <- as.data.frame(pca2$rotation)
  pcaloadings$Location <- item_difficulty$Location

  # find limits of contrast 1 loadings and location
  xlims <- c(floor(min(pcaloadings$PC1)),ceiling(max(pcaloadings$PC1)))
  ylims <- c(floor(min(pcaloadings$Location)),ceiling(max(pcaloadings$Location)))
  # how many steps between min/max
  xbreaks <- seq(floor(min(pcaloadings$PC1)),ceiling(max(pcaloadings$PC1)), 1)
  ybreaks <- seq(floor(min(pcaloadings$Location)),ceiling(max(pcaloadings$Location)), 0.5)
  xdiff <- diff(c(floor(min(pcaloadings$PC1)),ceiling(max(pcaloadings$PC1))))
  ydiff <- diff(c(floor(min(pcaloadings$Location)),ceiling(max(pcaloadings$Location))))

  if(output == "figure") {

    pcaloadings %>%
      rownames_to_column() %>%
      ggplot(aes(x=PC1, y=Location, label = rowname)) +
      geom_point(size = 3, color = "black") +
      xlab("Loadings on first residual contrast") +
      ylab("Item location (logit scale)") +
      geom_vline(xintercept = 0, color = "#e83c63", linetype = 2) +
      geom_hline(yintercept = 0, color = "#e83c63", linetype = 2) +
      scale_x_continuous(limits = c(-1,1), breaks = seq(-1,1, by = 0.25)) +
      scale_y_continuous(limits = ylims, breaks = ybreaks) +
      geom_text_repel() +
      theme(
        panel.background = element_rect(fill = "#ebf5f0",
                                        colour = "#ebf5f0",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "white")
      )
  } else {
    pcaloadings <- pcaloadings %>%
      rownames_to_column(var = "itemnr") %>%
      dplyr::select(itemnr,Location,all_of(pcx))
    return(pcaloadings)
  }
}



#' Create a figure showing items and thresholds (with CI)
#'
#' Items are sorted by item average location. Confidence intervals are 84% by
#' default to enable visual interpretation of statistically significant
#' differences (Payton et al., 2003). The CI can be changed using the
#' `sem_multiplier` option (ie. use 1.96 for 95% CI).
#'
#' Only works with partial credit models currently. For dichotomous data, use
#' `df.erm <- RM(data)` followed by `plotPImap(df.erm, sorted = T)`
#'
#' @param dfin Dataframe with item data only
#' @param numbers Display text in figure with item threshold locations
#' @param sem_multiplier For confidence intervals displayed in figure
#' @export
RIitemHierarchy <- function(dfin, numbers = TRUE, sem_multiplier = 1.405){

  # create temporary itemlabels object if none is found
  if (is.data.frame(itemlabels) != TRUE) {
    itemlabels <- data.frame(itemnr = names(dfin),
                             item = names(dfin))
  }

  if(max(as.matrix(dfin), na.rm = TRUE) == 1) {
    stop("Dichotomous data currently not supported. See `?RIitemHierarchy` for workaround.")
  } else {

    erm_out <- PCM(dfin)
    item.locations <- as.data.frame(thresholds(erm_out)[[3]][[1]][, -1] - mean(thresholds(erm_out)[[3]][[1]][, -1], na.rm=T))
    item_difficulty <- item.locations %>%
      mutate(Location = rowMeans(.,na.rm = TRUE), .before = `Threshold 1`) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))

    names(item.locations) <- paste0("T", c(1:ncol(item.locations)))
    itemloc.long <- item.locations %>%
      rownames_to_column() %>%
      dplyr::rename(names = "rowname") %>%
      pivot_longer(
        cols = starts_with("T"),
        names_to = "thresholds",
        values_to = "par_values"
      ) %>%
      dplyr::rename(itemnr = names,
                    Threshold = thresholds,
                    Locations = par_values
      )
    # get SEM estimates for each threshold
    item.estimates <- eRm::thresholds(erm_out)
    itemSE <- as.data.frame(item.estimates[["se.thresh"]]) %>%
      rownames_to_column(var = 'itemThresh') %>%
      dplyr::rename(ThreshSEM = 'item.estimates[["se.thresh"]]')
    # long format dataframe with separate variables for item and threshold

    # vector of threshold names as "T1" etc
    Tthresh <- paste0("T",c(1:100))
    # vector with threshold names as "c1" etc (since eRm outputs these)
    names(Tthresh) <- paste0("c",c(1:100))
    # create df and recode c1 to T1, etc
    itemSE <- itemSE %>%
      tidyr::separate(itemThresh, c(NA,"itemThresh"), sep = "beta ") %>%
      tidyr::separate(itemThresh, c("itemnr","threshnr"), sep = "\\.") %>%
      mutate(Threshold = dplyr::recode(threshnr, !!!Tthresh)) %>%
      dplyr::select(!threshnr)

    # join all dataframes together
    itemLocs <- item_difficulty %>%
      rownames_to_column(var = "itemnr") %>%
      dplyr::select(!any_of(starts_with("Thresh"))) %>%
      left_join(itemloc.long, ., by = "itemnr") %>%
      left_join(., itemlabels, by = "itemnr") %>%
      dplyr::rename(itemDescr = item) %>%
      left_join(., itemSE, by = c("itemnr","Threshold"))

    # get order of items
    itemOrder <- itemLocs %>%
      arrange(Location) %>%
      distinct(itemnr) %>%
      pull()

    # and itemlabels in the same order
    itemLabels <- itemLocs %>%
      arrange(Location) %>%
      distinct(itemDescr) %>%
      pull()

    # use the ordering and create plot

    if(numbers == FALSE){
      itemLocs %>%
        mutate(Item = factor(itemnr, levels = itemOrder)) %>%
        ggplot(aes(x = Item, color = Threshold)) +
        geom_point(aes(y = Location),
                   size = 4,
                   shape = 18,
                   color = "black"
        ) +
        geom_point(aes(y = Locations),
                   position = position_nudge()) +
        geom_errorbar(aes(ymin = Locations - sem_multiplier*ThreshSEM, ymax = Locations + sem_multiplier*ThreshSEM),
                      width = 0.11
        ) +
        geom_text(aes(y = Locations, label = Threshold), hjust = 0.5, vjust = 1.4,
                  show.legend = FALSE) +
        geom_rug(aes(y = Locations), color = "darkgrey", sides = "l", length = unit(0.02, "npc")) +
        scale_x_discrete(labels = str_wrap(paste0(itemOrder, " - ", itemLabels), width = 36)) +
        coord_flip() +
        #scale_color_brewer(palette = "Dark2", guide = "none") +
        scale_color_viridis_d(guide = "none", option = "H") + # enables any number of thresholds to be colorized with good contrast between adjacent categories
        labs(caption = glue("Note. Item locations are indicated by black diamond shapes.
            Item threshold locations are indicated by colored dots and colored text.
            Horizontal error bars indicate 84% confidence intervals around threshold locations.")) +
        theme(plot.caption = element_text(hjust = 0, face = "italic"),
              legend.position = "none") +
        theme_bw()

    }
    else {
      itemLocs %>%
        mutate(Item = factor(itemnr, levels = itemOrder)) %>%
        ggplot(aes(x = Item, color = Threshold)) +
        geom_point(aes(y = Location),
                   size = 4,
                   shape = 18,
                   color = "black"
        ) +
        geom_point(aes(y = Locations),
                   position = position_nudge()) +
        geom_text(aes(y = Location, label = round(Location,2)),
                  hjust = 0.5, vjust = -1.3, color = "black", size = 3,
                  show.legend = FALSE) +
        geom_errorbar(aes(ymin = Locations - sem_multiplier*ThreshSEM, ymax = Locations + sem_multiplier*ThreshSEM),
                      width = 0.11
        ) +
        geom_text(aes(y = Locations, label = Threshold), hjust = 0.5, vjust = 1.4,
                  show.legend = FALSE) +
        geom_text(aes(y = Locations, label = round(Locations,2)), hjust = 0.5, vjust = -1.1, size = 3,
                  show.legend = FALSE) +
        geom_hline(aes(yintercept = mean(Location)),
                   linetype = "dashed",
                   color = "darkgrey") +
        geom_rug(aes(y = Locations), color = "darkgrey", sides = "l", length = unit(0.02, "npc")) +
        scale_x_discrete(labels = str_wrap(paste0(itemOrder, " - ", itemLabels), width = 36)) +
        coord_flip() +
        #scale_color_brewer(palette = "Dark2", guide = "none") +
        scale_color_viridis_d(guide = "none", option = "H") + # enables any number of thresholds to be colorized with good contrast between adjacent categories
        labs(caption = glue("Note. Item locations are indicated by black diamond shapes and black text.
            Item threshold locations are indicated by colored dots and colored text.
            Horizontal error bars indicate 84% confidence intervals around threshold locations.")) +
        theme(plot.caption = element_text(hjust = 0, face = "italic"),
              legend.position = "none") +
        theme_bw()
    }
  }
}


#' Calculate conditional infit MSQ statistics
#'
#' Automatically uses RM (dichotomous data) or PCM (polytomous data) depending
#' on data structure.
#'
#' Uses `iarm::out_infit()` to calculate conditional mean square fit statistics
#' for all items. See Müller (2020, DOI: 10.1186/s40488-020-00108-7) for details.
#' Note: only uses complete cases! This is explicitly mentioned in the automatic
#' table caption text.
#'
#' Cutoff threshold values from simulation data (using option `simcut`) are
#' used with the `quantile()` function with .001 and .999 values to filter out
#' extremes. Actual cutoff values are shown in the output.
#'
#' Simulated datasets that have zero responses in any response category that
#' should have data will automatically be removed/skipped from analysis,
#' which means that final set of iterations may be lower than specified by user.
#'
#' Optional sorting (only) for table output with conditional highlighting based
#' on simulation cutoff values, `sort = "infit"`.
#'
#' @param data Dataframe with response data
#' @param simcut Object output from `RIgetfit()`
#' @param output Optional "dataframe" or "quarto"
#' @param sort Optional "infit"
#' @param cutoff Default `c(.001,.999)`
#' @param ... Options passed on to `kbl_rise()` for table creation
#' @export
RIitemfit <- function(data, simcut, output = "table", sort = "items", cutoff = c(.001,.999), ...) {

  if(min(as.matrix(data), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")
  } else if(na.omit(data) %>% nrow() == 0) {
    stop("No complete cases in data.")
  } else if(max(as.matrix(data), na.rm = T) == 1) {
    erm_out <- eRm::RM(data)
    item_avg_locations <- coef(erm_out, "beta")*-1 # item coefficients
    person_avg_locations <- eRm::person.parameter(erm_out)[["theta.table"]][["Person Parameter"]] %>%
      mean(na.rm = TRUE)
    relative_item_avg_locations <- item_avg_locations - person_avg_locations
  } else if(max(as.matrix(data), na.rm = T) > 1) {
    erm_out <- eRm::PCM(data)
    item_avg_locations <- RIitemparams(data, output = "dataframe") %>%
      pull(Location)
    person_avg_locations <- RIestThetasCATr(data) %>%
      mean(na.rm = TRUE)
    relative_item_avg_locations <- item_avg_locations - person_avg_locations
  }

  options(rgl.useNULL = TRUE) # temp MacOS fix for iarm dependency vcdExtra->rgl

  # get conditional MSQ
  cfit <- iarm::out_infit(erm_out)
  # get count of complete cases
  n_complete <- nrow(na.omit(data))

  # create dataframe
  item.fit.table <- data.frame(InfitMSQ = cfit$Infit) %>%
    round(3) %>%
    rownames_to_column("Item") %>%
    add_column(`Relative location` = round(relative_item_avg_locations,2))

  if (!missing(simcut)) {

    # get number of iterations used to get simulation based cutoff values
    iterations <- length(simcut) - 2

    nodata <- lapply(simcut, is.character) %>% unlist()
    iterations_nodata <- which(nodata)

    actual_iterations <- iterations - length(iterations_nodata)

    # summarise simulations and set cutoff values
    if (actual_iterations == iterations) {
      lo_hi <-
        bind_rows(simcut[1:iterations]) %>%
        group_by(Item) %>%
        summarise(min_infit_msq = quantile(InfitMSQ, cutoff[1]),
                  max_infit_msq = quantile(InfitMSQ, cutoff[2])
        )
    } else {
      lo_hi <-
        bind_rows(simcut[1:iterations][-iterations_nodata]) %>%
        group_by(Item) %>%
        summarise(min_infit_msq = quantile(InfitMSQ, cutoff[1]),
                  max_infit_msq = quantile(InfitMSQ, cutoff[2])
        )
    }

    lo_hi$Item <- names(data)

    # get upper/lower values into a dataframe
    if (actual_iterations == iterations) {
      fit_table <-
        bind_rows(simcut[1:iterations]) %>%
        group_by(Item) %>%
        summarise(inf_thresh = paste0("[",round(quantile(InfitMSQ, cutoff[1]),3),", ",round(quantile(InfitMSQ, cutoff[2]),3),"]")
        )
    } else {
      fit_table <-
        bind_rows(simcut[1:iterations][-iterations_nodata]) %>%
        group_by(Item) %>%
        summarise(inf_thresh = paste0("[",round(quantile(InfitMSQ, cutoff[1]),3),", ",round(quantile(InfitMSQ, cutoff[2]),3),"]")
        )
    }
    # add thresholds to dataframe and calculate differences between thresholds and observed values
    item.fit.table <-
      item.fit.table %>%
      add_column(`Infit thresholds` = fit_table$inf_thresh, .after = "InfitMSQ") %>%
      left_join(lo_hi, by = "Item") %>%
      mutate(infit_lo = abs(InfitMSQ - min_infit_msq),
             infit_hi = abs(InfitMSQ - max_infit_msq),
             `Infit diff` = round(pmin(infit_lo,infit_hi),3)
      ) %>%
      mutate(`Infit diff` = ifelse(yes = "no misfit", no = `Infit diff`, InfitMSQ > min_infit_msq & InfitMSQ < max_infit_msq)) %>%
      dplyr::select(!contains(c("lo","hi","min","max"))) %>%
      add_column(`Relative location` = round(relative_item_avg_locations,2))

    options(rgl.useNULL = FALSE) # reset temp fix iarm

    if (output == "table" & sort == "items") {
      # set conditional highlighting based on cutoffs
      for (i in 1:nrow(lo_hi)) {

        item.fit.table[i,"InfitMSQ"] <- cell_spec(item.fit.table[i,"InfitMSQ"],
                                                  color = ifelse(item.fit.table[i,"InfitMSQ"] < lo_hi[i,"min_infit_msq"], "red",
                                                                 ifelse(item.fit.table[i,"InfitMSQ"] > lo_hi[i,"max_infit_msq"], "red", "black")))
      }

      # output table
      item.fit.table %>%
        kbl_rise(...) %>%
        kableExtra::footnote(general = paste0("MSQ values based on conditional calculations (n = ", n_complete," complete cases).
                                Simulation based thresholds from ", actual_iterations," simulated datasets."))

    } else if (output == "table" & sort == "infit") {
      for (i in 1:nrow(lo_hi)) {

        item.fit.table[i,"InfitMSQ"] <- cell_spec(item.fit.table[i,"InfitMSQ"],
                                                  color = ifelse(item.fit.table[i,"InfitMSQ"] < lo_hi[i,"min_infit_msq"], "red",
                                                                 ifelse(item.fit.table[i,"InfitMSQ"] > lo_hi[i,"max_infit_msq"], "red", "black")))
      }

      item.fit.table %>%
        arrange(desc(`Infit diff`)) %>%
        kbl_rise(...) %>%
        kableExtra::footnote(general = paste0("MSQ values based on conditional calculations (n = ", n_complete," complete cases).
                                Simulation based thresholds from ", actual_iterations," simulated datasets."))

    } else if(output == "dataframe") {
      return(janitor::clean_names(item.fit.table))

    } else if (output == "quarto") {
      knitr::kable(item.fit.table)
    }
  } else if (missing(simcut)) {
    if (output == "table" & missing(cutoff)) {
      kbl_rise(item.fit.table, ...) %>%
        kableExtra::footnote(general = paste0("MSQ values based on conditional estimation (n = ", n_complete," complete cases)."))

    } else if (output == "table" & cutoff == "Smith98") {
      # calculate cutoff values for conditional highlighting based on Smith et al, 1998.
      msq_infit_lo <- round(1 - 2/sqrt(n_complete),3)
      msq_infit_hi <- round(1 + 2/sqrt(n_complete),3)

      item.fit.table %>%
        mutate(InfitMSQ = cell_spec(InfitMSQ, color = ifelse(InfitMSQ < msq_infit_lo, "red",
                                                             ifelse(InfitMSQ > msq_infit_hi, "red", "black")
        ))) %>%
        add_column(!! paste0("1 ","\u00b1"," 2 / ","\u221A","n") := paste0("[",msq_infit_lo,", ", msq_infit_hi,"]")) %>%
        kbl_rise() %>%
        kableExtra::footnote(general = paste0("MSQ values based on conditional estimation (n = ", n_complete," complete cases)."))
    }
    else if (output == "dataframe") {
      return(janitor::clean_names(item.fit.table))
    } else if (output == "quarto") {
      knitr::kable(item.fit.table)
    }
  }
}

#' Get simulation based cutoff values for item fit values
#'
#' This function uses your response data to simulate datasets that fit the
#' Rasch model to find a credible range of item fit values. The function
#' outputs an object that is strongly recommended to save to an object, since it
#' takes some time to run this function when using many iterations/simulations.
#'
#' The output is a list object, which can in turn be used with two different
#' functions. Most importantly, you can use it with `RIitemfit()` to get
#' conditional highlighting of cutoff values based on your sample size and
#' item parameters. Each item gets its own cutoff thresholds.
#'
#' The function `RIgetfitPlot()` uses the package `ggdist` to plot the
#' distribution of fit values from the simulation results.
#'
#' Uses multi-core processing. To find how many cores you have on your computer,
#' use `parallel::detectCores()`. Remember to keep 1-2 cores free.
#'
#' Since version 0.2.4.2, the default is to only use complete cases in the
#' simulations, since this is what the conditional item fit function uses and
#' numbers should be more comparable using this method.
#'
#' @param data Dataframe with response data
#' @param iterations Number of simulation iterations (use 200-400)
#' @param cpu Number of CPU cores to use
#' @param na.omit Defaults to TRUE to produce conditional fit comparable values
#' @param seed For random number generation and reproducibility
#' @export
RIgetfit <- function(data, iterations = 250, cpu = 4, na.omit = TRUE, seed = 123) {
  # since we want comparable values to conditional item fit, which only uses
  # complete cases, we remove any missing responses by default
  if (na.omit == TRUE) {
    data <- na.omit(data)
  }
  sample_n <- nrow(data)
  options(rgl.useNULL = TRUE) # temp MacOS fix for iarm dependency vcdExtra->rgl

  # Use doRNG for proper reproducible parallel processing
  # This ensures each worker gets independent random streams
  registerDoParallel(cores = cpu)
  registerDoRNG(seed)  # Set a master seed for reproducibility

  if (min(as.matrix(data), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")
  } else if (max(as.matrix(data), na.rm = T) == 1 && min(as.matrix(data), na.rm = T) == 0) {
    # estimate item threshold locations from data
    erm_out <- eRm::RM(data)
    item_locations <- erm_out$betapar * -1
    names(item_locations) <- names(data)

    # estimate theta values from data using WLE
    if (na.omit == TRUE) {
      thetas <- RIestThetas(data)$WLE
    } else {
      mirt_out <- mirt(data, itemtype = "Rasch", verbose = FALSE)
      thetas <- mirt::fscores(mirt_out, method = "WLE", verbose = FALSE)
    }

    fitstats <- list()

    fitstats <- foreach(i = 1:iterations) %dopar% { # .packages = c("eRm", "iarm", "psychotools", "dplyr", "tidyr")

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

      # get conditional MSQ
      rm_out <- eRm::RM(testData, se = FALSE)
      cfit <- iarm::out_infit(rm_out)

      # create dataframe
      item.fit.table <- data.frame(
        InfitMSQ = cfit$Infit,
        OutfitMSQ = cfit$Outfit
      ) %>%
        round(3) %>%
        rownames_to_column("Item")
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

    # get number of response categories for each item for later use in checking complete responses
    itemlength <- list()
    for (i in 1:n_items) {
      itemlength[i] <- length(na.omit(item_locations[i, ]))
      names(itemlength)[i] <- names(data)[i]
    }

    # estimate theta values from data using WLE
    thetas <- RIestThetasCATr(data, cpu = cpu)

    fitstats <- list()

    fitstats <- foreach(i = 1:iterations) %dopar% { # , .packages = c("eRm", "iarm", "psychotools", "dplyr", "tidyr", "car")

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

      # get conditional MSQ
      pcm_out <- psychotools::pcmodel(testData, hessian = FALSE)
      cfit <- iarm::out_infit(pcm_out)

      # create dataframe
      item.fit.table <- data.frame(
        InfitMSQ = cfit$Infit,
        OutfitMSQ = cfit$Outfit
      ) %>%
        round(3) %>%
        rownames_to_column("Item")
    }
  }

  fitstats$sample_n <- sample_n
  fitstats$sample_summary <- summary(thetas)

  stopImplicitCluster() # reset multicore
  options(rgl.useNULL = FALSE) # reset rgl workaround
  return(fitstats)
}


#' Creates a plot with distribution of simulation based item fit values
#'
#' Uses the output from `RIgetfit()` as input. Uses
#' and `.width = c(.66,.99)` with `ggdist::stat_dots()`.
#'
#' @param simcut Output object from `RIgetfit()`
#' @param data Optional response dataframe for plotting observed item fit
#' @param cutoff Defaults to quantile(.001) and .999 to match `RIitemfit()`
#' @param output Optional setting "both" for infit and outfit
#' @export
RIgetfitPlot <- function(simcut, data, cutoff = c(.001, .999), output = "infit") {

  # get number of iterations used to get simulation based cutoff values
  iterations <- length(simcut) - 2

  # check which iterations have incomplete data
  nodata <- lapply(simcut, is.character) %>% unlist()
  iterations_nodata <- which(nodata)
  actual_iterations <- iterations - length(iterations_nodata)
  # in case the first iteration does not have complete data
  if (actual_iterations == iterations) {
    first_iteration <- 1
  } else {
    first_iteration <- c(1:iterations)[-iterations_nodata][1]
  }

  # summarise simulations and set cutoff values
  if (actual_iterations == iterations) {
    lo_hi <-
      bind_rows(simcut[1:iterations]) %>%
      group_by(Item) %>%
      summarise(min_infit_msq = quantile(InfitMSQ, cutoff[1]),
                max_infit_msq = quantile(InfitMSQ, cutoff[2]),
                p66lo_infit_msq = quantile(InfitMSQ, .167),
                p66hi_infit_msq = quantile(InfitMSQ, .833),
                median_infit = median(InfitMSQ),
                min_outfit_msq = quantile(OutfitMSQ, cutoff[1]),
                max_outfit_msq = quantile(OutfitMSQ, cutoff[2]),
                p66lo_outfit_msq = quantile(OutfitMSQ, .167),
                p66hi_outfit_msq = quantile(OutfitMSQ, .833),
                median_outfit = median(OutfitMSQ),
      )
  } else {
    lo_hi <-
      bind_rows(simcut[1:iterations][-iterations_nodata]) %>%
      group_by(Item) %>%
      summarise(min_infit_msq = quantile(InfitMSQ, cutoff[1]),
                max_infit_msq = quantile(InfitMSQ, cutoff[2]),
                p66lo_infit_msq = quantile(InfitMSQ, .167),
                p66hi_infit_msq = quantile(InfitMSQ, .833),
                median_infit = median(InfitMSQ),
                min_outfit_msq = quantile(OutfitMSQ, cutoff[1]),
                max_outfit_msq = quantile(OutfitMSQ, cutoff[2]),
                p66lo_outfit_msq = quantile(OutfitMSQ, .167),
                p66hi_outfit_msq = quantile(OutfitMSQ, .833),
                median_outfit = median(OutfitMSQ),
      )
  }

  if (missing(data)) {
    # summarise simulation results
    if (actual_iterations == iterations) {
      results <- bind_rows(simcut[1:iterations]) %>%
        pivot_longer(contains("MSQ"),
                     names_to = "statistic",
                     values_to = "Value")
    } else {
      results <- bind_rows(simcut[1:iterations][-iterations_nodata]) %>%
        pivot_longer(contains("MSQ"),
                     names_to = "statistic",
                     values_to = "Value")
    }

    # plot
    results %>%
      ggplot(aes(x = Value, y = factor(Item, levels = rev(simcut[[first_iteration]][["Item"]])))) +
      stat_dotsinterval(aes(slab_fill = after_stat(level)),
                        quantiles = iterations, point_interval = "median_qi",
                        layout = "weave", slab_color = NA,
                        .width = c(0.66, 0.999)) +
      #geom_point(aes(x = median(Value))) +
      labs(x = "Conditional MSQ",
           y = "Item") +
      scale_color_manual(values = scales::brewer_pal()(3)[-1], aesthetics = "slab_fill", guide = "none") +
      labs(caption = str_wrap(paste0("Note: Results from ",actual_iterations," simulated datasets with ",
                                     simcut$sample_n," respondents."))
      ) +
      facet_wrap(~statistic, ncol = 2) +
      scale_x_continuous(breaks = seq(0.5,1.5,0.1), minor_breaks = NULL) +
      theme_minimal() +
      theme(panel.spacing = unit(0.7, "cm", data = NULL))
  }
  else if (!missing(data)) {
    if(min(as.matrix(data), na.rm = T) > 0) {
      stop("The lowest response category needs to coded as 0. Please recode your data.")
    } else if(max(as.matrix(data), na.rm = T) == 1 && min(as.matrix(data), na.rm = T) == 0) {
      erm_out <- RM(data)
    } else if(max(as.matrix(data), na.rm = T) > 1 && min(as.matrix(data), na.rm = T) == 0) {
      erm_out <- PCM(data)
    }

    # get conditional MSQ
    cfit <- iarm::out_infit(erm_out)
    #bfit <- iarm::boot_fit(erm_out, B = 100)

    # create dataframe with observed MSQ
    item.fit.table <- data.frame(InfitMSQ = cfit$Infit,
                                 OutfitMSQ = cfit$Outfit,
                                 InfitSE = cfit$Infit.se,
                                 OutfitSE = cfit$Outfit.se) %>%
      rownames_to_column("Item")

    observed <- item.fit.table %>%
      pivot_longer(contains("MSQ"),
                   names_to = "statistic",
                   values_to = "observed")

    # join simulated and observed MSQ
    if (actual_iterations == iterations) {
      infit <-
        bind_rows(simcut[1:iterations]) %>%
        pivot_longer(contains("MSQ"),
                     names_to = "statistic",
                     values_to = "Value") %>%
        left_join(observed, by = c("Item","statistic")) %>%
        dplyr::filter(statistic == "InfitMSQ")
    } else {
      infit <-
        bind_rows(simcut[1:iterations][-iterations_nodata]) %>%
        pivot_longer(contains("MSQ"),
                     names_to = "statistic",
                     values_to = "Value") %>%
        left_join(observed, by = c("Item","statistic")) %>%
        dplyr::filter(statistic == "InfitMSQ")
    }

    # and plot
    infit_p <-
      infit %>%
      ggplot(aes(x = Value, y = factor(Item, levels = rev(simcut[[first_iteration]][["Item"]])))) +
      stat_dots(aes(slab_fill = after_stat(level)),
                quantiles = iterations,
                layout = "weave", slab_color = NA,
                .width = c(0.666, 0.999)) +
      geom_segment(data = lo_hi,
                   aes(x = min_infit_msq, xend = max_infit_msq),
                   color = "black",
                   linewidth = 0.7) +
      geom_segment(data = lo_hi,
                   aes(x = p66lo_infit_msq, xend = p66hi_infit_msq),
                   color = "black",
                   linewidth = 1.2) +
      geom_point(data = lo_hi,
                 aes(x = median_infit),
                 size = 3.6) +
      geom_point(aes(x = observed),
                 color = "sienna2", shape = 18,
                 position = position_nudge(y = -0.1), size = 4) +
      labs(x = "Conditional Infit MSQ",
           y = "Item") +
      scale_color_manual(values = scales::brewer_pal()(3)[-1], aesthetics = "slab_fill", guide = "none") +
      scale_x_continuous(breaks = seq(0.5,1.5,0.1), minor_breaks = NULL) +
      theme_minimal() +
      theme(panel.spacing = unit(0.7, "cm", data = NULL))

    if (actual_iterations == iterations) {
      outfit <-
        bind_rows(simcut[1:iterations]) %>%
        pivot_longer(contains("MSQ"),
                     names_to = "statistic",
                     values_to = "Value") %>%
        left_join(observed, by = c("Item","statistic")) %>%
        dplyr::filter(statistic == "OutfitMSQ")
    } else {
      outfit <-
        bind_rows(simcut[1:iterations][-iterations_nodata]) %>%
        pivot_longer(contains("MSQ"),
                     names_to = "statistic",
                     values_to = "Value") %>%
        left_join(observed, by = c("Item","statistic")) %>%
        dplyr::filter(statistic == "OutfitMSQ")
    }
    # and plot
    outfit_p <-
      outfit %>%
      ggplot(aes(x = Value, y = factor(Item, levels = rev(simcut[[first_iteration]][["Item"]])))) +
      stat_dots(aes(slab_fill = after_stat(level)),
                quantiles = iterations,
                layout = "weave", slab_color = NA,
                .width = c(0.666, 0.999)) +
      geom_segment(data = lo_hi,
                   aes(x = min_outfit_msq, xend = max_outfit_msq),
                   color = "black",
                   linewidth = 0.7) +
      geom_segment(data = lo_hi,
                   aes(x = p66lo_outfit_msq, xend = p66hi_outfit_msq),
                   color = "black",
                   linewidth = 1.2) +
      geom_point(data = lo_hi,
                 aes(x = median_outfit),
                 size = 3.6) +
      geom_point(aes(x = observed),
                 color = "sienna2", shape = 18,
                 position = position_nudge(y = -0.1), size = 4) +
      labs(x = "Conditional Outfit MSQ",
           y = "Item") +
      scale_color_manual(values = scales::brewer_pal()(3)[-1], aesthetics = "slab_fill", guide = "none") +
      labs(caption = str_wrap(paste0("Note: Results from ",actual_iterations," simulated datasets with ",
                                     simcut$sample_n," respondents.\n
                                     Orange dots indicate observed conditional item fit. Black dots indicate median fit from simulations."))
      ) +
      scale_x_continuous(breaks = seq(0.5,1.5,0.1), minor_breaks = NULL) +
      theme_minimal() +
      theme(panel.spacing = unit(0.7, "cm", data = NULL))

    if (output == "both") {
      infit_p + outfit_p
    } else if (output == "infit") {
      infit_p +
        labs(caption = str_wrap(paste0("Note: Results from ",actual_iterations," simulated datasets with ",
                                       simcut$sample_n," respondents.\n
                                     Orange dots indicate observed conditional item fit. Black dots indicate median fit from simulations.")))
    }

  }
}

#' Item-restscore
#'
#' A simple wrapper for `iarm::item_restscore()`, adding information about
#' absolute difference in expected and observed values, and item (average)
#' location.
#'
#' Please note that item-restscore is likely to produce false positives when sample
#' size is > 600. It is recommended to use `RIbootRestscore()` with large samples.
#'
#' @param data A dataframe with response data
#' @param output Defaults to a HTML table, optional "quarto" and "dataframe"
#' @param sort Optional sorting on absolute difference (descending)
#' @param p.adj See `?item_restscore()` for options (BH is default)
#' @export
RIrestscore <- function(data, output = "table", sort, p.adj = "BH") {

  if(min(as.matrix(data), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")
  } else if(na.omit(data) %>% nrow() == 0) {
    stop("No complete cases in data.")
  } else if(max(as.matrix(data), na.rm = T) == 1) {
    erm_out <- eRm::RM(data)
    item_avg_locations <- coef(erm_out, "beta")*-1 # item coefficients
    person_avg_locations <- eRm::person.parameter(erm_out)[["theta.table"]][["Person Parameter"]] %>%
      mean(na.rm = TRUE)
    relative_item_avg_locations <- item_avg_locations - person_avg_locations
  } else if(max(as.matrix(data), na.rm = T) > 1) {
    erm_out <- eRm::PCM(data)
    item_avg_locations <- RIitemparams(data, output = "dataframe") %>%
      pull(Location)
    person_avg_locations <- RIestThetasCATr(data) %>%
      mean(na.rm = TRUE)
    relative_item_avg_locations <- item_avg_locations - person_avg_locations
  }

  options(rgl.useNULL = TRUE) # temp MacOS fix for iarm dependency vcdExtra->rgl

  i1 <- iarm::item_restscore(erm_out, p.adj = p.adj)
  i1 <- as.data.frame(i1)

  i2 <- data.frame("Observed" = as.numeric(i1[[1]][1:ncol(data),1]) %>% round(2),
                   "Expected" = as.numeric(i1[[1]][1:ncol(data),2]) %>% round(2),
                   #"se" = as.numeric(i1[[1]][1:ncol(data),3]),
                   #"p.value" = as.numeric(i1[[1]][1:ncol(data),4]),
                   "p_adj" = as.numeric(i1[[1]][1:ncol(data),5]) %>% round(3),
                   "sig" = as.character(i1[[1]][1:ncol(data),6])
  ) %>%
    mutate("Absolute difference" = abs(Expected - Observed), .before = "p_adj") %>%
    add_column(Item = names(data), .before = "Observed") %>%
    dplyr::rename(!!paste0("Adjusted p-value (",p.adj,")") := p_adj,
                  `Statistical significance level` = sig,
                  `Observed value` = Observed,
                  `Model expected value` = Expected) %>%
    add_column(Location = round(item_avg_locations,2),
               `Relative location` = round(relative_item_avg_locations,2))

  options(rgl.useNULL = FALSE) # temp MacOS fix for iarm dependency vcdExtra->rgl

  if (output == "table" & missing(sort)) {
    kbl_rise(i2)
  } else if (output == "table" & sort == "diff") {
    i2 %>%
      arrange(desc(`Absolute difference`)) %>%
      kbl_rise()
  } else if (output == "quarto" & sort == "diff") {
    i2 %>%
      arrange(desc(`Absolute difference`)) %>%
      knitr::kable()
  } else if (output == "quarto" & missing(sort)) {
    knitr::kable(i2)
  } else if (output == "dataframe") {
    return(i2)
  }
}


#' Item-restscore bootstrapped
#'
#' See this simulation study preprint: https://pgmj.github.io/rasch_itemfit/
#'
#' Item-restscore will often indicate false positives (item misfit when it is not
#' misfitting) if the sample size is above 400 and there is one truly misfitting
#' item in the data. If there is more than one misfitting item, false positives
#' can occur at such small sample sizes as n = 150-250 with increasing rates
#' as n goes up.
#'
#' Conversely, when sample size is below n = 800, the detection rate of truly
#' misfitting items is below 90%, particularly if misfitting items have
#' location > 1.5 logits from the sample mean.
#'
#' Thus, if one has a large dataset it may be useful to be able to use
#' non-parametric bootstrapping with replacement to get a more nuanced view of
#' the probability of items actually being misfit.
#'
#' @param data Dataframe with only response data, with 0 as lowest response
#' @param iterations How many bootstrap samples to run
#' @param samplesize How large sample to use in each bootstrap
#' @param cpu How many CPU's to use
#' @param output Optional "dataframe", or "quarto" for `knitr::kable()` output
#' @param cutoff Percentage values below this are not shown in table/quarto output
#' @export
RIbootRestscore <- function(dat, iterations = 200, samplesize = 600, cpu = 4,
                            output = "table", cutoff = 5) {

  n_items <- ncol(dat)
  options(rgl.useNULL = TRUE) # temp MacOS fix for iarm dependency vcdExtra->rgl

  # get vector of random seeds for reproducible simulations
  seeds <- c(.Random.seed, as.integer(.Random.seed + 1))
  if (iterations > length(seeds)) {
    stop(paste0("Maximum possible iterations is ",length(seeds),"."))
  }

  if(min(as.matrix(dat), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")
  } else if (samplesize > nrow(dat)) {
    stop(paste0("`samplesize` (",samplesize,") cannot be larger than the number of rows in your data (",
                nrow(dat),")."))
  } else if(max(as.matrix(dat), na.rm = T) == 1) {
    model <- "RM"
    erm_out <- eRm::RM(dat, se = F)
    item_avg_locations <- coef(erm_out, "beta")*-1 # item coefficients
    person_avg_locations <- eRm::person.parameter(erm_out)[["theta.table"]][["Person Parameter"]] %>%
      mean(na.rm = TRUE)
    relative_item_avg_locations <- item_avg_locations - person_avg_locations
  } else if(max(as.matrix(dat), na.rm = T) > 1) {
    model <- "PCM"
    erm_out <- PCM(dat, se = F)
    item_avg_locations <- RIitemparams(dat, output = "dataframe") %>%
      pull(Location)
    person_avg_locations <- RIestThetasCATr(dat) %>%
      mean(na.rm = TRUE)
    relative_item_avg_locations <- item_avg_locations - person_avg_locations
  }

  itemlocs <- data.frame(rel_loc = round(relative_item_avg_locations,2),
                         item = names(dat))

  # get conditional MSQ
  cfit <- iarm::out_infit(erm_out)

  # get count of complete cases
  n_complete <- nrow(na.omit(dat))

  # create df for later join
  cfit_df <- data.frame(item = names(dat),
                        infit = round(cfit$Infit,2))

  registerDoParallel(cores = cpu)

  fit <- data.frame()
  fit <- foreach(i = 1:iterations, .combine = rbind) %dopar% {
    # reproducible seed
    set.seed(seeds[i])

    data <- dat[sample(1:nrow(dat), samplesize, replace = TRUE), ]

    if (model == "PCM") {
      erm_out <- psychotools::PCModel.fit(data, hessian = FALSE)
    } else if (model == "RM") {
      erm_out <- eRm::RM(data, se = FALSE)
    }
    i1 <- iarm::item_restscore(erm_out)
    i1 <- as.data.frame(i1)

    i1d <- data.frame("observed" = as.numeric(i1[[1]][1:ncol(data),1]),
                      "expected" = as.numeric(i1[[1]][1:ncol(data),2]),
                      "se" = as.numeric(i1[[1]][1:ncol(data),3]),
                      "p.value" = as.numeric(i1[[1]][1:ncol(data),4]),
                      "p.adj.BH" = as.numeric(i1[[1]][1:ncol(data),5])
    ) %>%
      mutate(diff_abs = abs(expected - observed),
             diff = expected - observed,
             item_restscore = case_when(p.adj.BH < .05 & diff < 0 ~ "overfit",
                                        p.adj.BH < .05 & diff > 0 ~ "underfit",
                                        TRUE ~"no misfit")) %>%
      dplyr::select(item_restscore, diff, diff_abs) %>%
      mutate(item = names(data))

    i1d
  }

  fit_tbl <- fit %>%
    group_by(item) %>%
    dplyr::count(item_restscore) %>%
    mutate(percent = round(n*100/sum(n),1)) %>%
    left_join(cfit_df, by = "item") %>%
    ungroup()

  test <- fit_tbl %>%
    filter(!item_restscore == "no misfit") %>%
    dplyr::slice_max(percent) %>%
    pull(percent)

  if (isTRUE(test < cutoff)) {
    stopImplicitCluster()
    options(rgl.useNULL = FALSE) # temp MacOS fix for iarm dependency vcdExtra->rgl
    message(paste0("No item indicates misfit in more than ",cutoff,"% of iterations."))
  }

  if (output == "table") {
    stopImplicitCluster()
    options(rgl.useNULL = FALSE) # temp MacOS fix for iarm dependency vcdExtra->rgl
    fit_tbl %>%
      left_join(itemlocs, by = "item") %>%
      filter(!item_restscore == "no misfit",
             percent > cutoff) %>%
      dplyr::select(!n) %>%
      arrange(desc(item_restscore),desc(percent)) %>%
      set_names(c("Item","Item-restscore result","% of iterations","Conditional MSQ infit",
                  "Relative average item location")) %>%
      kbl_rise() %>%
      kableExtra::footnote(general = paste0("Results based on ",iterations,
                                " bootstrap iterations with n = ",samplesize,
                                " and ",n_items," items. Conditional mean-square infit based on complete responders only (n = ",
                                n_complete,")."))

  } else if (output == "dataframe") {
    stopImplicitCluster()
    options(rgl.useNULL = FALSE) # temp MacOS fix for iarm dependency vcdExtra->rgl
    return(fit_tbl)

  } else if (output == "quarto") {
    stopImplicitCluster()
    options(rgl.useNULL = FALSE) # temp MacOS fix for iarm dependency vcdExtra->rgl

    fit_tbl %>%
      left_join(itemlocs, by = "item") %>%
      filter(!item_restscore == "no misfit",
             percent > cutoff) %>%
      dplyr::select(!n) %>%
      arrange(desc(item_restscore),desc(percent)) %>%
      set_names(c("Item","Item-restscore result","% of iterations","Conditional MSQ infit",
                  "Relative average item location")) %>%
      knitr::kable()
  }
}


#' Conditional Item Characteristic Curves
#'
#' A wrapper function to simplify getting CICC curves from `iarm::ICCplot()` for
#' any number of items in the same figure. Uses the `patchwork` package, which
#' also allows for further additions to a plot using for instance:
#'
#' `+ plot_annotation(subtitle = "Some subtitle")`. See `?plot_annotation` for
#' more possibilities.
#'
#' A useful option is for DIF analysis, which requires two optional settings:
#' `dif = "yes"` and `dif_var = your$difvariable`.
#'
#' Text from `?iarm::ICCplot`:
#'
#' Plots Item Characteristic Curves for dichotomous and polytomous items using
#' average scores within adjacent class intervals (method="cut").
#'
#' @param data Dataframe/tibble with only item response data coded as integers
#' @param classintervals Number of groups to divide respondents into
#' @param method Either "cut" (default) or "score" for all possible total scores
#' @param dif Defaults to "no". Needs a defined `dif_var` if set to "yes"
#' @param dif_var An exogenous variable (ie. age group, sex) coded as a factor
#' @export

RIciccPlot <- function(data, class_intervals = 5, method = "cut",
                        dif = "no", dif_var = NA) {

  if(min(as.matrix(data), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")
  } else if(na.omit(data) %>% nrow() == 0) {
    stop("No complete cases in data.")
  }

  if (dif == "no") {
    labels <- c("Expected Item Score",
                "Average Observed Item Score")

  } else if (dif == "yes") {
    labels <- c("Expected Item Score",
                levels(dif_var))
  }

  sink(nullfile()) # suppress output from the rows below

  iccplots <- map(1:ncol(data), ~ RI_iarm_ICCplot(as.data.frame(data),
                                                itemnumber = .x,
                                                method = method,
                                                cinumber = class_intervals,
                                                axis.rumm = "yes",
                                                title = NULL,
                                                icclabel = "yes",
                                                itemdescrip = names(data)[.x],
                                                difvar = dif_var,
                                                dif = dif,
                                                diflabels = levels(dif_var),
                                                difstats = "yes") +
                    theme(legend.direction = "vertical")
                  )

  sink() # disable suppress output

  plots <- patchwork::wrap_plots(iccplots,
                                 axes = "collect",
                                 guides = "collect",
                                 axis_titles = "collect"
  ) +
    patchwork::plot_annotation(title = "Conditional Item Characteristic Curves") #+
  #patchwork::guide_area()
  return(plots)
}


#' Cross-validation of conditional item infit
#'
#' Creates k random folds using `rsample::vfold_cv()`;
#' "V-fold cross-validation (also known as k-fold cross-validation) randomly
#' splits the data into V groups of roughly equal size (called "folds").
#' A resample of the analysis data consists of V-1 of the folds",
#' see <https://rsample.tidymodels.org/reference/vfold_cv.html>
#'
#' Each V-1 dataset is used both for calculating item fit and expected item fit
#' critical values (using `RIgetfit()`). If `output = "table"` (default), results
#' are summarized indicating upper and lower bounds for each item's calculated
#' infit and simulated expected range. This is based on all V-1 fold combinations.
#'
#' @param data Dataframe with item responses
#' @param k Number of folds to use (default is 5)
#' @param output Default `raw`, options `dataframe`, `table`
#' @param sim_iter Number of iterations (depends on sample size)
#' @param sim_cpu Number of CPU cores to use
#' @param cutoff Truncation at percentile values (see `?RIitemfit`)
#' @export
#'
RIinfitKfold <- function(data, k = 5, output = "raw", sim_iter = 100,
                         sim_cpu = 4, cutoff = c(.001,.999)) {

  if(min(as.matrix(data), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")
  } else if(na.omit(data) %>% nrow() == 0) {
    stop("No complete cases in data.")
  } else if(max(as.matrix(data), na.rm = T) == 1) {
    rmodel = "RM"
  } else if(max(as.matrix(data), na.rm = T) > 1) {
    rmodel = "PCM"
  }

  options(rgl.useNULL = TRUE) # temp MacOS fix for iarm dependency vcdExtra->rgl

  #require(rsample) # install package if necessary
  datafold <- rsample::vfold_cv(data, v = k)
  samplesize <- nrow(data) - round(nrow(data) / k, 0)

  if (rmodel == "PCM") {
    infit_folds <- map(
      1:k,
      function(x) {
        cfit <- iarm::out_infit(pcmodel(analysis(datafold$splits[[x]]), hessian = FALSE))
        data.frame(InfitMSQ = cfit$Infit) %>%
          round(3) %>%
          rownames_to_column("Item")
      })

  } else if (rmodel == "RM") {
    infit_folds <- map(
      1:k,
      function(x) {
        cfit <- iarm::out_infit(RM(analysis(datafold$splits[[x]]), se = FALSE))
        data.frame(InfitMSQ = cfit$Infit) %>%
          round(3) %>%
          rownames_to_column("Item")
      })
  }

  tbl <- bind_rows(infit_folds) %>%
    group_by(Item) %>%
    summarise(lowest_infit = min(InfitMSQ),
              #mean_infit = mean(InfitMSQ),
              highest_infit = max(InfitMSQ)) %>%
    mutate(across(where(is.numeric), ~ round(.x,3)))

  # get reference values based on simulations from each fold
  simcut <- map(1:k, ~ RIgetfit(analysis(datafold$splits[[.x]]), iterations = sim_iter, cpu = sim_cpu))

  # get number of iterations used to get simulation based cutoff values
  iterations <- length(simcut[[1]]) - 2

  # remove the sample info (last 2 list objects within each simulation iteration)
  simcut_trimmed <- lapply(simcut, function(sublist) {
    lapply(sublist[1:sim_iter], function(inner_list) {
      inner_list
    })
  })

  # get results into one list, unnested
  simcut2 <- unlist(simcut_trimmed, recursive = FALSE)

  # check for faulty simulation runs
  nodata <- lapply(simcut2, is.character) %>% unlist()
  iterations_nodata <- which(nodata)

  actual_iterations <- iterations - length(iterations_nodata)

  # summarise simulations and set cutoff values
  if (actual_iterations == iterations) {
    lo_hi <-
      bind_rows(simcut2[1:iterations]) %>%
      group_by(Item) %>%
      summarise(sim_min_infit_msq = quantile(InfitMSQ, cutoff[1]),
                sim_max_infit_msq = quantile(InfitMSQ, cutoff[2])
      ) %>%
      mutate(across(where(is.numeric), ~ round(.x,3)))

  } else {
    lo_hi <-
      bind_rows(simcut2[1:iterations][-iterations_nodata]) %>%
      group_by(Item) %>%
      summarise(sim_min_infit_msq = quantile(InfitMSQ, cutoff[1]),
                sim_max_infit_msq = quantile(InfitMSQ, cutoff[2])
      ) %>%
      mutate(across(where(is.numeric), ~ round(.x,3)))

  }

  tbl <- tbl %>%
    left_join(lo_hi, by = "Item") %>%
    relocate(sim_min_infit_msq, .after = "Item")

  if (output == "dataframe") {
    stopImplicitCluster()
    options(rgl.useNULL = FALSE) # temp MacOS fix for iarm dependency vcdExtra->rgl

    return(tbl)
  } else if (output == "raw") {
    stopImplicitCluster()
    options(rgl.useNULL = FALSE) # temp MacOS fix for iarm dependency vcdExtra->rgl

    list(table = tbl,
         results = infit_folds,
         data = datafold,
         simcut = simcut2)

  } else if (output == "table") {
    stopImplicitCluster()
    options(rgl.useNULL = FALSE) # temp MacOS fix for iarm dependency vcdExtra->rgl

    tbl %>%
      dplyr::rename(`Lower cutoff` = sim_min_infit_msq,
                    `Upper cutoff` = sim_max_infit_msq,
                    `Lowest infit MSQ` = lowest_infit,
                    #`Mean infit MSQ` = mean_infit,
                    `Highest infit MSQ` = highest_infit) %>%
      kbl_rise() %>%
      kableExtra::footnote(general = paste0("Infit MSQ values based on conditional estimation using n = ",
                                            samplesize," cases (",k," folds of data from a dataset of ",nrow(data),"). Cutoff values based on ",sim_iter," simulations from the each fold of data."))
  }
}

#' Plot k-fold infit based on raw output
#'
#' NOTE: currently this function seems to not work on Windows OS.
#'
#' Outputs a figure showing highest and lowest expected values based on all
#' simulations and cross-validation folds from an object created with
#' `RIinfitKfold(data)`.
#'
#' @param kfold Object with "raw" output from `RIinfitKfold()` (default)
#' @export
#'
RIinfitKfoldPlot <- function(kfold) {

  infit_limits <- data.frame(Item = names(kfold[["data"]][["splits"]][[1]]$data),
                             infit_min_sim = kfold$table$sim_min_infit_msq,
                             infit_max_sim = kfold$table$sim_max_infit_msq)

  item_names <- names(analysis(kfold[["data"]][["splits"]][[1]]))
  n_items <- length(item_names)
  k = length(kfold$results)
  n = length(kfold[["data"]][["splits"]][[1]][["in_id"]])

  underfit <- bind_rows(kfold$results) %>%
    mutate(underfit = case_when(InfitMSQ > infit_limits$infit_max_sim ~ 1,
                                TRUE ~ 0)) %>%
    group_by(Item) %>%
    dplyr::count(underfit) %>%
    dplyr::filter(underfit == 1) %>%
    mutate(percent = n/sum(length(kfold$results))*100) %>%
    left_join(infit_limits, by = join_by(Item))

  overfit <- bind_rows(kfold$results) %>%
    mutate(overfit = case_when(InfitMSQ < infit_limits$infit_min_sim ~ 1,
                               TRUE ~ 0)) %>%
    group_by(Item) %>%
    dplyr::count(overfit) %>%
    dplyr::filter(overfit == 1) %>%
    mutate(percent = n/sum(length(kfold$results))*100) %>%
    left_join(infit_limits, by = join_by(Item))

  overunderfit <- bind_rows(underfit[,1:2],overfit[,1:2])

  infit_limits <- infit_limits %>%
    left_join(overunderfit, by = join_by(Item)) %>%
    mutate(underfit_color = case_when(underfit == 1 ~ "sienna2",
                                      TRUE ~ "lightgrey")) %>%
    mutate(overfit_color = case_when(overfit == 1 ~ "sienna2",
                                     TRUE ~ "lightgrey"))

  stacksize <- bind_rows(kfold$results) %>%
    count(round(InfitMSQ,2), .by = "Item") %>%
    arrange(n) %>%
    tail(1) %>%
    pull(n)

  if (stacksize > 6) {
    stacksize <- 6
  }

  p <-
    bind_rows(kfold$results) %>%
    mutate(Item = factor(Item, levels = rev(item_names))) %>%
    ggplot(aes(x = InfitMSQ, y = Item)) +
    stat_dots(stackratio = 0.8) +
    geom_point(data = infit_limits,
               aes(x = infit_min_sim,
                   color = overfit_color),
               shape = 18, size = 8+(8-stacksize),
               position = position_nudge(y = -0.1)) +
    geom_point(data = infit_limits,
               aes(x = infit_max_sim,
                   color = underfit_color),
               shape = 18, size = 8+(8-stacksize),
               position = position_nudge(y = -0.1)) +
    scale_color_identity() +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(x = "Conditional infit",
         caption = str_wrap(paste0("Note. Results based on ",k,
                                   " folds of data (n = ",n,
                                   " per fold). Diamonds indicate cutoff values based on all folds.
                                   Orange diamonds indicate misfit and show percentage misfit."), 80))

  if (nrow(underfit) > 0) {
    p <-
      p +
      geom_text(data = underfit,
                aes(label = round(percent,0),
                    x = infit_max_sim),
                position = position_nudge(y = -0.1),
                size = 2+(7-stacksize), color = "white")

  }

  if (nrow(overfit) > 0) {
    p <-
      p +
      geom_text(data = overfit,
                aes(label = round(percent,0),
                    x = infit_min_sim),
                position = position_nudge(y = -0.1),
                size = 2+(7-stacksize), color = "white")

  }

  p

}

#' Cross-validation of item-restscore
#'
#' Creates k random folds using `rsample::vfold_cv()`;
#' "V-fold cross-validation (also known as k-fold cross-validation) randomly
#' splits the data into V groups of roughly equal size (called "folds").
#' A resample of the analysis data consists of V-1 of the folds",
#' see <https://rsample.tidymodels.org/reference/vfold_cv.html>
#'
#' Each V-1 dataset is used both for calculating item-restscore and summarises
#' results based on BH adjusted p-values.
#'
#' @param data Dataframe with item responses
#' @param k Number of folds to use (default is 5)
#' @param output Default `table`, option `dataframe`.
#' @export
#'
RIrestscoreKfold <- function(data, k = 5, output = "table") {

  if(min(as.matrix(data), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")
  } else if(na.omit(data) %>% nrow() == 0) {
    stop("No complete cases in data.")
  } else if(max(as.matrix(data), na.rm = T) == 1) {
    rmodel = "RM"
  } else if(max(as.matrix(data), na.rm = T) > 1) {
    rmodel = "PCM"
  }

  options(rgl.useNULL = TRUE)
  #require(rsample) # install package if necessary
  datafold <- rsample::vfold_cv(data, v = k)
  samplesize <- nrow(data) - round(nrow(data) / k, 0)

  if (rmodel == "PCM") {
    restscore_folds <- map(
      1:k,
      function(x) {

        i1 <- PCM(analysis(datafold$splits[[x]]), se = FALSE) %>%
          iarm::item_restscore() %>%
          as.data.frame()

        i1d <- data.frame("observed" = as.numeric(i1[[1]][1:ncol(data),1]),
                          "expected" = as.numeric(i1[[1]][1:ncol(data),2]),
                          "se" = as.numeric(i1[[1]][1:ncol(data),3]),
                          "p.value" = as.numeric(i1[[1]][1:ncol(data),4]),
                          "p.adj.BH" = as.numeric(i1[[1]][1:ncol(data),5])) %>%
          add_column(Item = names(data))
      })

  } else if (rmodel == "RM") {
    restscore_folds <- map(
      1:k,
      function(x) {
        i1 <- RM(analysis(datafold$splits[[x]]), se = FALSE) %>%
          iarm::item_restscore() %>%
          as.data.frame()

        i1d <- data.frame("observed" = as.numeric(i1[[1]][1:ncol(data),1]),
                          "expected" = as.numeric(i1[[1]][1:ncol(data),2]),
                          "se" = as.numeric(i1[[1]][1:ncol(data),3]),
                          "p.value" = as.numeric(i1[[1]][1:ncol(data),4]),
                          "p.adj.BH" = as.numeric(i1[[1]][1:ncol(data),5])) %>%
          add_column(Item = names(data))
      })
  }

  tbl <- bind_rows(restscore_folds) %>%
    mutate(item_restscore = case_when(p.adj.BH < .05 & observed < expected ~ "Underfit",
                                      p.adj.BH < .05 & observed > expected ~ "Overfit",
                                      TRUE ~ "No misfit")) %>%
    group_by(Item) %>%
    dplyr::count(item_restscore) %>%
    mutate(Percent = paste0(round(n*100/sum(n),1),"%")) %>%
    dplyr::select(!n)

  # tbl <- tbl %>%
  #   left_join(itemlabels, by = join_by("Item" == "itemnr"))

  if (output == "dataframe") {
    options(rgl.useNULL = FALSE)
    return(tbl)

  } else if (output == "table") {
    options(rgl.useNULL = FALSE)
    tbl %>%
      dplyr::rename(`Item-restscore\nresult` = item_restscore) %>%
      kbl_rise() %>%
      kableExtra::footnote(general = paste0("Item-restscore summary of statistically significant BH adjusted p-values based on n = ",
                                samplesize," cases (",k," folds of data from a dataset of ",nrow(data),")."))
  }

}

