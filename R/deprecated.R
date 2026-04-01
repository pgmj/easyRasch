#' Create table with Rasch PCM model item fit values for each item.
#'
#' Defaults to using conditional estimates for MSQ values (Müller, 2020)
#' estimated using the `iarm` package. Use `method = "unconditional"` for the
#' "old" unconditional MSQ values (using `eRm`).
#'
#' Since version 0.2.0 (2024-08-15), it is highly recommended to replace
#' rule-of-thumb cutoff values with simulation based cutoffs. See details in
#' `?RIgetfit()` for an easy way to get and set appropriate cutoff values.
#'
#' ZSTD is inflated with large samples (N > 500). There is an optional function
#' to use a reduced sample size and run analysis using multiple random samples
#' to get the average ZSTD for each item over all runs.
#'
#' If you are using Quarto, the YAML execute setting "cache: yes" will be a
#' useful chunk option to speed things up if you render often. 30-50 samples
#' seems to produce stable output, but 4-8 is probably sufficient for a quick
#' look at the approximate ZSTD statistics.
#' It is recommended to use sample size 200-500, based on
#' Hagell & Westergren (2016) & Müller (2020).
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' @param dfin Dataframe with item data only
#' @param samplesize Desired sample size in multisampling (recommended range 200-500)
#' @param nsamples Desired number of samples (recommended range 10-50)
#' @param zstd_min Lower cutoff level for ZSTD
#' @param zstd_max Upper cutoff level for ZSTD
#' @param msq_min Lower cutoff level for MSQ
#' @param msq_max Upper cutoff level for MSQ
#' @param fontsize Set fontsize for table
#' @param fontfamily Set font family for table
#' @param output Defaults to output a table. Optional "dataframe" or "quarto"
#' @param tbl_width Set table width in percent
#' @param method Defaults to "conditional". Optional "unconditional"
#' @param simcut Set to TRUE if you want to use simulation based cutoff values
#' @param gf The output object from `RIgetfit()` is needed when `simcut = TRUE`
#' @export
RIitemfitPCM <- function(dfin, samplesize, nsamples, zstd_min = -1.96, zstd_max = 1.96,
                         msq_min = 0.7, msq_max = 1.3, fontsize = 15, fontfamily = "Lato",
                         output = "table", tbl_width = 65, method = "conditional",
                         simcut = FALSE, gf) {

  if (missing(samplesize)) {
    df.erm <- PCM(dfin) # run PCM model

    if (method == "unconditional") {
      # get unconditional estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
      ple <- person.parameter(df.erm)
      item.fit <- eRm::itemfit(ple)

      # collect data to df
      item.fit.table <- as.data.frame(cbind(item.fit$i.outfitMSQ,
                                            item.fit$i.infitMSQ,
                                            item.fit$i.outfitZ,
                                            item.fit$i.infitZ)) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
        rownames_to_column("Item")

      colnames(item.fit.table) <- c("Item","OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")

    } else if (method == "conditional") {
      # get conditional MSQ
      cfit <- iarm::out_infit(df.erm)
      # get unconditional MSQ
      ple <- person.parameter(df.erm)
      item.fit <- eRm::itemfit(ple)

      item.fit.table <- data.frame(OutfitMSQ = cfit$Outfit,
                                   InfitMSQ = cfit$Infit,
                                   OutfitZSTD = item.fit$i.outfitZ,
                                   InfitZSTD = item.fit$i.infitZ) %>%
        round(3) %>%
        rownames_to_column("Item")
    }

    if (simcut == TRUE) {
      if(missing(gf)) {
        stop("When `simcut = TRUE` you need to specify a `gf` object, output from `RIgetfit()`")
      }
      iterations <- length(gf) - 3

      lo_hi <- bind_rows(gf[1:(length(gf)-3)]) %>%
        summarise(max_infit_msq = quantile(infit_msq, .99),
                  min_infit_msq = quantile(infit_msq, .01),
                  max_outfit_msq = quantile(outfit_msq, .99),
                  min_outfit_msq = quantile(outfit_msq, .01),
                  max_infit_zstd = quantile(infit_zstd, .99),
                  min_infit_zstd = quantile(infit_zstd, .01),
                  max_outfit_zstd = quantile(outfit_zstd, .99),
                  min_outfit_zstd = quantile(outfit_zstd, .01),
        )

      item.fit.table %>%
        mutate(OutfitZSTD = cell_spec(OutfitZSTD, color = ifelse(OutfitZSTD < lo_hi$min_outfit_zstd, "red",
                                                                 ifelse(OutfitZSTD > lo_hi$max_outfit_zstd, "red", "black")
        ))) %>%
        mutate(InfitZSTD = cell_spec(InfitZSTD, color = ifelse(InfitZSTD < lo_hi$min_infit_zstd, "red",
                                                               ifelse(InfitZSTD > lo_hi$max_infit_zstd, "red", "black")
        ))) %>%
        mutate(OutfitMSQ = cell_spec(OutfitMSQ, color = ifelse(OutfitMSQ < lo_hi$min_outfit_msq, "red",
                                                               ifelse(OutfitMSQ > lo_hi$max_outfit_msq, "red", "black")
        ))) %>%
        mutate(InfitMSQ = cell_spec(InfitMSQ, color = ifelse(InfitMSQ < lo_hi$min_infit_msq, "red",
                                                             ifelse(InfitMSQ > lo_hi$max_infit_msq, "red", "black")
        ))) %>%
        kbl(booktabs = T, escape = F,
            table.attr = paste0("data-quarto-disable-processing='true' style='width:",tbl_width,"%;'")) %>%
        # bootstrap options are for HTML output
        kable_styling(
          bootstrap_options = c("striped", "hover"),
          position = "left",
          full_width = F,
          font_size = fontsize,
          fixed_thead = T
        ) %>%
        column_spec(1, bold = T) %>%
        row_spec(0, bold = T) %>%
        kable_classic(html_font = fontfamily) %>%
        # latex_options are for PDF output
        kable_styling(latex_options = c("striped", "scale_down")) %>%
        kableExtra::footnote(general = paste0("MSQ values based on ", method," estimation. All values\n are based on a sample size of ", nrow(dfin),"."))
    } else {

      if (output == "table") {
        # create table that highlights cutoff values in red
        item.fit.table %>%
          mutate(OutfitZSTD = cell_spec(OutfitZSTD, color = ifelse(OutfitZSTD < zstd_min, "red",
                                                                   ifelse(OutfitZSTD > zstd_max, "red", "black")
          ))) %>%
          mutate(InfitZSTD = cell_spec(InfitZSTD, color = ifelse(InfitZSTD < zstd_min, "red",
                                                                 ifelse(InfitZSTD > zstd_max, "red", "black")
          ))) %>%
          mutate(OutfitMSQ = cell_spec(OutfitMSQ, color = ifelse(OutfitMSQ < msq_min, "red",
                                                                 ifelse(OutfitMSQ > msq_max, "red", "black")
          ))) %>%
          mutate(InfitMSQ = cell_spec(InfitMSQ, color = ifelse(InfitMSQ < msq_min, "red",
                                                               ifelse(InfitMSQ > msq_max, "red", "black")
          ))) %>%
          kbl(booktabs = T, escape = F,
              table.attr = paste0("data-quarto-disable-processing='true' style='width:",tbl_width,"%;'")) %>%
          # bootstrap options are for HTML output
          kable_styling(
            bootstrap_options = c("striped", "hover"),
            position = "left",
            full_width = F,
            font_size = fontsize,
            fixed_thead = T
          ) %>%
          column_spec(1, bold = T) %>%
          row_spec(0, bold = T) %>%
          kable_classic(html_font = fontfamily) %>%
          # latex_options are for PDF output
          kable_styling(latex_options = c("striped", "scale_down")) %>%
          kableExtra::footnote(general = paste0("MSQ values based on ", method," estimation. All values\n are based on a sample size of ", nrow(dfin),"."))

      } else if (output == "dataframe") {
        return(item.fit.table)
      } else if (output == "quarto") {
        knitr::kable(item.fit.table)
      }
    }

  } else {
    df.erm <- PCM(dfin) # run PCM model
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    ple <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(ple)

    # ZSTD multisample
    outfitZ <- c()
    infitZ <- c()
    for (i in 1:nsamples) {
      df.new <- dfin[sample(1:nrow(dfin), samplesize), ]
      #df.new <- na.omit(df.new)
      df.z <- PCM(df.new)
      ple <- person.parameter(df.z)
      item.fit.z <- eRm::itemfit(ple)
      outfitZ <- cbind(outfitZ, item.fit.z$i.outfitZ)
      infitZ <- cbind(infitZ, item.fit.z$i.infitZ)
    }

    if (method == "unconditional") {
      # get unconditional estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
      ple <- person.parameter(df.erm)
      item.fit <- eRm::itemfit(ple)

      # collect data to df
      item.fit.table <- as.data.frame(cbind(item.fit$i.outfitMSQ,
                                            item.fit$i.infitMSQ,
                                            rowMeans(outfitZ),
                                            rowMeans(infitZ)
      )) %>%
        round(3) %>%
        rownames_to_column("Item")

      colnames(item.fit.table) <- c("Item","OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")

    } else if (method == "conditional") {
      # get conditional MSQ
      cfit <- iarm::out_infit(df.erm)
      # get unconditional MSQ
      ple <- person.parameter(df.erm)
      item.fit <- eRm::itemfit(ple)

      item.fit.table <- data.frame(OutfitMSQ = cfit$Outfit,
                                   InfitMSQ = cfit$Infit,
                                   OutfitZSTD = rowMeans(outfitZ),
                                   InfitZSTD = rowMeans(infitZ)
      ) %>%
        round(3) %>%
        rownames_to_column("Item")
    }

    if (output == "table") {
      # create table that highlights cutoff values in red
      item.fit.table %>%
        mutate(OutfitZSTD = cell_spec(OutfitZSTD, color = ifelse(OutfitZSTD < zstd_min, "red",
                                                                 ifelse(OutfitZSTD > zstd_max, "red", "black")
        ))) %>%
        mutate(InfitZSTD = cell_spec(InfitZSTD, color = ifelse(InfitZSTD < zstd_min, "red",
                                                               ifelse(InfitZSTD > zstd_max, "red", "black")
        ))) %>%
        mutate(OutfitMSQ = cell_spec(OutfitMSQ, color = ifelse(OutfitMSQ < msq_min, "red",
                                                               ifelse(OutfitMSQ > msq_max, "red", "black")
        ))) %>%
        mutate(InfitMSQ = cell_spec(InfitMSQ, color = ifelse(InfitMSQ < msq_min, "red",
                                                             ifelse(InfitMSQ > msq_max, "red", "black")
        ))) %>%
        kbl(booktabs = T, escape = F,
            table.attr = paste0("data-quarto-disable-processing='true' style='width:",tbl_width,"%;'")) %>%
        # bootstrap options are for HTML output
        kable_styling(
          bootstrap_options = c("striped", "hover"),
          position = "left",
          full_width = F,
          font_size = fontsize,
          fixed_thead = T
        ) %>%
        column_spec(1, bold = T) %>%
        row_spec(0, bold = T) %>%
        kable_classic(html_font = fontfamily) %>%
        # latex_options are for PDF output
        kable_styling(latex_options = c("striped", "scale_down")) %>%
        kableExtra::footnote(general = paste0("MSQ values are based on a sample size of ", nrow(dfin)," respondents,\n using ",method," estimation.\n",
                                              "ZSTD values are the means from ", nsamples, " subsamples, each consisting\n of ", samplesize, " random respondents."))

    } else if (output == "dataframe") {
      return(item.fit.table)
    } else if (output == "quarto") {
      knitr::kable(item.fit.table)
    }

  }
}


#' Create table with Rasch PCM model item fit values for each item.
#'
#' Special version of `RIitemfitPCM()` that utilizes multiple CPU cores to improve
#' performance. Requires `library(doParallel)`. To find how many cores you
#' have on your computer, use `parallel::detectCores()`, but remember to keep
#' some cores free.
#'
#' See documentation for `RIitemfitPCM()` for more complete information.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' @param dfin Dataframe with item data only
#' @param samplesize Desired sample size in multisampling (recommended range 200-500)
#' @param nsamples Desired number of samples (recommended range 8-50)
#' @param zstd_min Lower cutoff level for ZSTD
#' @param zstd_max Upper cutoff level for ZSTD
#' @param msq_min Lower cutoff level for MSQ
#' @param msq_max Upper cutoff level for MSQ
#' @param cpu Number of CPU cores to utilize (default = 4)
#' @param fontsize Set fontsize for table
#' @param fontfamily Set font family for table
#' @param output Defaults to output a table. Optional "dataframe" or "quarto"
#' @param tbl_width Set table width in percent
#' @param method Defaults to "conditional". Optional "unconditional"
#' @export
RIitemfitPCM2 <- function(dfin, samplesize = 200, nsamples = 8, cpu = 4,
                          zstd_min = -1.96, zstd_max = 1.96, msq_min = 0.7,
                          msq_max = 1.3, fontsize = 15, fontfamily = "Lato",
                          output = "table", tbl_width = 65,
                          method = "conditional") {
  require(doParallel)
  registerDoParallel(cores = cpu)
  df.erm <- PCM(dfin) # run PCM model

  # ZSTD multisample
  outfitZ <- c()
  infitZ <- c()

  infitZ <- foreach(icount(nsamples), .combine = cbind) %dopar% {
    library(eRm)
    df.new <- dfin[sample(1:nrow(dfin), samplesize), ]
    df.new <- na.omit(df.new)
    df.z <- PCM(df.new)
    ple <- person.parameter(df.z)
    item.fit.z <- eRm::itemfit(ple)
    item.fit.z$i.infitZ
  }

  outfitZ <- foreach(icount(nsamples), .combine = cbind) %dopar% {
    library(eRm)
    df.new <- dfin[sample(1:nrow(dfin), samplesize), ]
    df.new <- na.omit(df.new)
    df.z <- PCM(df.new)
    ple <- person.parameter(df.z)
    item.fit.z <- eRm::itemfit(ple)
    item.fit.z$i.outfitZ
  }

  if (method == "unconditional") {
    # get unconditional estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    ple <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(ple)

    # collect data to df
    item.fit.table <- as.data.frame(cbind(item.fit$i.outfitMSQ,
                                          item.fit$i.infitMSQ,
                                          rowMeans(outfitZ),
                                          rowMeans(infitZ)
    )) %>%
      round(3) %>%
      rownames_to_column("Item")

    colnames(item.fit.table) <- c("Item","OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")

  } else if (method == "conditional") {
    # get conditional MSQ
    cfit <- iarm::out_infit(df.erm)
    # get unconditional MSQ
    ple <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(ple)

    item.fit.table <- data.frame(OutfitMSQ = cfit$Outfit,
                                 InfitMSQ = cfit$Infit,
                                 OutfitZSTD = rowMeans(outfitZ),
                                 InfitZSTD = rowMeans(infitZ)
    ) %>%
      round(3) %>%
      rownames_to_column("Item")
  }

  if (output == "table") {
    # create table that highlights cutoff values in red
    item.fit.table %>%
      mutate(OutfitZSTD = cell_spec(OutfitZSTD, color = ifelse(OutfitZSTD < zstd_min, "red",
                                                               ifelse(OutfitZSTD > zstd_max, "red", "black")
      ))) %>%
      mutate(InfitZSTD = cell_spec(InfitZSTD, color = ifelse(InfitZSTD < zstd_min, "red",
                                                             ifelse(InfitZSTD > zstd_max, "red", "black")
      ))) %>%
      mutate(OutfitMSQ = cell_spec(OutfitMSQ, color = ifelse(OutfitMSQ < msq_min, "red",
                                                             ifelse(OutfitMSQ > msq_max, "red", "black")
      ))) %>%
      mutate(InfitMSQ = cell_spec(InfitMSQ, color = ifelse(InfitMSQ < msq_min, "red",
                                                           ifelse(InfitMSQ > msq_max, "red", "black")
      ))) %>%
      kbl(booktabs = T, escape = F,
          table.attr = paste0("data-quarto-disable-processing='true' style='width:",tbl_width,"%;'")) %>%
      # bootstrap options are for HTML output
      kable_styling(
        bootstrap_options = c("striped", "hover"),
        position = "left",
        full_width = F,
        font_size = fontsize,
        fixed_thead = T
      ) %>%
      column_spec(1, bold = T) %>%
      row_spec(0, bold = T) %>%
      kable_classic(html_font = fontfamily) %>%
      # latex_options are for PDF output
      kable_styling(latex_options = c("striped", "scale_down")) %>%
      kableExtra::footnote(general = paste0("MSQ values are based on a sample size of ", nrow(dfin)," respondents,\n using ",method," estimation.\n",
                                            "ZSTD values are the means from ", nsamples, " subsamples, each consisting\n of ", samplesize, " random respondents."))

  } else if (output == "dataframe") {
    return(item.fit.table)
  } else if (output == "quarto") {
    knitr::kable(item.fit.table)
  }
}

#' Create table with Rasch dichotomous model item fit values for each item.
#'
#' Defaults to using conditional estimates for MSQ values (Müller, 2020)
#' estimated using the `iarm` package. Use `method = "unconditional"` for the
#' "old" unconditional MSQ values (using `eRm`).
#'
#' ZSTD is inflated with large samples (N > 500). Optional function to reduce
#' sample size and run analysis using multiple random samples to get average ZSTD
#' If you are using Quarto/Rmarkdown, "cache: yes" will be a useful chunk option to
#' speed things up. 50 samples seems to give stable output, but 4-8 is probably
#' sufficient for a quick look at the approximate ZSTD statistics. It is recommended
#' to use sample size 200-500, based on Hagell & Westergren, 2016.
#'
#' @description `r lifecycle::badge("deprecated")`
#' @param dfin Dataframe with item data only
#' @param samplesize Desired sample size in multisampling (recommended range 250-500)
#' @param nsamples Desired number of samples (recommended range 10-50)
#' @param zstd_min Lower cutoff level for ZSTD
#' @param zstd_max Upper cutoff level for ZSTD
#' @param msq_min Lower cutoff level for MSQ
#' @param msq_max Upper cutoff level for MSQ
#' @param fontsize Set font size for table
#' @param fontfamily Set font family for table
#' @param output Defaults to output a table. Optional "dataframe" or "quarto"
#' @param tbl_width Set table width in percent
#' @param method Defaults to "conditional". Optional "unconditional"
#' @export
RIitemfitRM <- function(dfin, samplesize, nsamples, zstd_min = -1.96, zstd_max = 1.96,
                        msq_min = 0.7, msq_max = 1.3, fontsize = 15, fontfamily = "Lato",
                        output = "table", tbl_width = 65,
                        method = "conditional") {
  if(missing(samplesize)) {
    df.erm <- RM(dfin) # run Rasch model

    if (method == "unconditional") {
      # get unconditional estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
      ple <- person.parameter(df.erm)
      item.fit <- eRm::itemfit(ple)

      # collect data to df
      item.fit.table <- as.data.frame(cbind(item.fit$i.outfitMSQ,
                                            item.fit$i.infitMSQ,
                                            item.fit$i.outfitZ,
                                            item.fit$i.infitZ)) %>%
        round(3) %>%
        rownames_to_column("Item")

      colnames(item.fit.table) <- c("Item","OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")

    } else if (method == "conditional") {
      # get conditional MSQ
      cfit <- iarm::out_infit(df.erm)
      # get unconditional MSQ
      ple <- person.parameter(df.erm)
      item.fit <- eRm::itemfit(ple)

      item.fit.table <- data.frame(OutfitMSQ = cfit$Outfit,
                                   InfitMSQ = cfit$Infit,
                                   OutfitZSTD = item.fit$i.outfitZ,
                                   InfitZSTD = item.fit$i.infitZ
      ) %>%
        round(3) %>%
        rownames_to_column("Item")
    }

    if (output == "table") {

      # create table that highlights cutoff values in red
      item.fit.table %>%
        mutate(OutfitZSTD = cell_spec(OutfitZSTD, color = ifelse(OutfitZSTD < zstd_min, "red",
                                                                 ifelse(OutfitZSTD > zstd_max, "red", "black")))) %>%
        mutate(InfitZSTD = cell_spec(InfitZSTD, color = ifelse(InfitZSTD < zstd_min, "red",
                                                               ifelse(InfitZSTD > zstd_max, "red", "black")))) %>%
        mutate(OutfitMSQ = cell_spec(OutfitMSQ, color = ifelse(OutfitMSQ < msq_min, "red",
                                                               ifelse(OutfitMSQ > msq_max, "red", "black")))) %>%
        mutate(InfitMSQ = cell_spec(InfitMSQ, color = ifelse(InfitMSQ < msq_min, "red",
                                                             ifelse(InfitMSQ > msq_max, "red", "black")))) %>%
        kbl(booktabs = T, escape = F,
            table.attr = glue("data-quarto-disable-processing='true' style='width:{tbl_width}%;'")) %>%
        # bootstrap options are for HTML output
        kable_styling(bootstrap_options = c("striped", "hover"),
                      position = "left",
                      full_width = F,
                      font_size = fontsize,
                      fixed_thead = T) %>% # when there is a long list in the table
        column_spec(1, bold = T) %>%
        kable_classic(html_font = fontfamily) %>%
        # latex_options are for PDF output
        kable_styling(latex_options = c("striped","scale_down")) %>%
        kableExtra::footnote(general = paste0("MSQ values based on ", method," estimation. All values\n are based on a sample size of ", nrow(dfin),"."))

    } else if (output == "dataframe") {
      return(item.fit.table)
    } else if (output == "quarto") {
      knitr::kable(item.fit.table)
    }

  } else { # for multisampling
    df.erm <- RM(dfin) # run Rasch model

    # ZSTD multisample
    outfitZ <- c()
    infitZ <- c()
    for (i in 1:nsamples) {
      df.new <- dfin[sample(1:nrow(dfin), samplesize), ]
      df.new <- na.omit(df.new)
      df.z <- RM(df.new)
      ple <- person.parameter(df.z)
      item.fit.z <- eRm::itemfit(ple)
      outfitZ <- cbind(outfitZ, item.fit.z$i.outfitZ)
      infitZ <- cbind(infitZ, item.fit.z$i.infitZ)
    }

    if (method == "unconditional") {
      # get unconditional estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
      ple <- person.parameter(df.erm)
      item.fit <- eRm::itemfit(ple)

      # collect data to df
      item.fit.table <- as.data.frame(cbind(item.fit$i.outfitMSQ,
                                            item.fit$i.infitMSQ,
                                            rowMeans(outfitZ),
                                            rowMeans(infitZ)
      )) %>%
        round(3) %>%
        rownames_to_column("Item")

      colnames(item.fit.table) <- c("Item","OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")

    } else if (method == "conditional") {
      # get conditional MSQ
      cfit <- iarm::out_infit(df.erm)
      # get unconditional MSQ
      ple <- person.parameter(df.erm)
      item.fit <- eRm::itemfit(ple)

      item.fit.table <- data.frame(OutfitMSQ = cfit$Outfit,
                                   InfitMSQ = cfit$Infit,
                                   OutfitZSTD = rowMeans(outfitZ),
                                   InfitZSTD = rowMeans(infitZ)
      ) %>%
        round(3) %>%
        rownames_to_column("Item")
    }

    if (output == "table") {
      # create table that highlights cutoff values in red
      item.fit.table %>%
        mutate(OutfitZSTD = cell_spec(OutfitZSTD, color = ifelse(OutfitZSTD < zstd_min, "red",
                                                                 ifelse(OutfitZSTD > zstd_max, "red", "black")
        ))) %>%
        mutate(InfitZSTD = cell_spec(InfitZSTD, color = ifelse(InfitZSTD < zstd_min, "red",
                                                               ifelse(InfitZSTD > zstd_max, "red", "black")
        ))) %>%
        mutate(OutfitMSQ = cell_spec(OutfitMSQ, color = ifelse(OutfitMSQ < msq_min, "red",
                                                               ifelse(OutfitMSQ > msq_max, "red", "black")
        ))) %>%
        mutate(InfitMSQ = cell_spec(InfitMSQ, color = ifelse(InfitMSQ < msq_min, "red",
                                                             ifelse(InfitMSQ > msq_max, "red", "black")
        ))) %>%
        kbl(booktabs = T, escape = F,
            table.attr = paste0("data-quarto-disable-processing='true' style='width:",tbl_width,"%;'")) %>%
        # bootstrap options are for HTML output
        kable_styling(
          bootstrap_options = c("striped", "hover"),
          position = "left",
          full_width = F,
          font_size = fontsize,
          fixed_thead = T
        ) %>%
        column_spec(1, bold = T) %>%
        row_spec(0, bold = T) %>%
        kable_classic(html_font = fontfamily) %>%
        # latex_options are for PDF output
        kable_styling(latex_options = c("striped", "scale_down")) %>%
        kableExtra::footnote(general = paste0("MSQ values are based on a sample size of ", nrow(dfin)," respondents,\n using ",method," estimation.\n",
                                              "ZSTD values are the means from ", nsamples, " subsamples, each consisting\n of ", samplesize, " random respondents."))

    } else if (output == "dataframe") {
      return(item.fit.table)
    } else if (output == "quarto") {
      knitr::kable(item.fit.table)
    }
  }
}

#' Reliability of test
#'
#' NOTE: TIF is not reliable with small numbers of items, and should not be used
#' as your primary indication of reliability (see doi: 10.1111/bmsp.12033).
#' Please use `RIreliability()` and `RIrelRep()` instead.
#'
#' Test information shows the reliability curve of the test (not the sample).
#'
#' Use option `samplePSI = TRUE` to add graphical and written representation of
#' the current sample's theta mean/SD, test information (TIF) mean/SD, and
#' Person Separation Index (PSI). According to Wright & Stone (1999), PSI is
#' calculated as \eqn{\frac{\mathrm{SSD}-\mathrm{MSE}}{\mathrm{SSD}}}{(SSD-MSE)/SSD},
#' see `?eRm::SepRel` for details. According to Embretson & Reise (2000),
#' PSI = 1 - SEM^2, and TIF = 1/SEM^2, and the values reported in
#' this function are based on sample average SEM.
#'
#' For reference:
#' TIF 2.5 corresponds to PSI 0.6
#' TIF 3.33 -> PSI 0.7
#' TIF 5 -> PSI 0.8
#' TIF 10 -> PSI 0.9
#'
#' @references
#' Milanzi, et al. (2015). Reliability measures in item response theory: Manifest versus latent correlation functions. \href{https://doi.org/10.1111/bmsp.12033}{doi:10.1111/bmsp.12033}
#' @description
#' `r lifecycle::badge("deprecated")`
#' @param dfin Dataframe with item data only
#' @param lo Lower limit of x axis (default = -5)
#' @param hi Upper limit of x axis (default = 5)
#' @param samplePSI Adds information about sample characteristics
#' @param cutoff Caption text will generate information relative to this TIF value
#' @param model Defaults to "PCM", use "RM" for dichotomous data
#' @export
RItif <- function(dfin, lo = -5, hi = 5, samplePSI = FALSE, cutoff = 3.33, model = "PCM") {

  message("NOTE: TIF is not reliable, particularly with small numbers of items,
          and should not be used as your primary indication of reliability (see doi: 10.1111/bmsp.12033).
          Please use `RIreliability()`.")
  # convert TIF to PSI, if cutoff is set manually
  psi_tif <- round(1-(1/sqrt(cutoff))^2,2)

  if (model == "PCM") {
    if(max(as.matrix(dfin), na.rm = TRUE) == 1) {
      stop("Use `model = 'RM'` for dichotomous data.")
    } else {
      erm_out <- PCM(dfin)
      # item locations
      item.locations <- as.data.frame(thresholds(erm_out)[[3]][[1]][, -1] - mean(thresholds(erm_out)[[3]][[1]][, -1], na.rm=T))

      # person locations
      pthetas <- iarm::person_estimates(erm_out, allperson = TRUE) %>%
        as.data.frame() %>%
        pull(WLE)
      # item locations
      thresholds<-c()
      for (i in 1:ncol(item.locations)) {
        thresholds<-c(thresholds,item.locations[,i])
      }
    }
  }

  if (model == "RM") {
    erm_out <- RM(dfin)

    # item locations
    item.estimates <- coef(erm_out, "beta")*-1 # item coefficients
    item.locations <- as.data.frame(item.estimates) %>%
      rownames_to_column() %>%
      tidyr::separate(rowname, c(NA, "item"), sep = " ") %>%
      dplyr::rename(location = item.estimates)

    # person locations
    pthetas <- iarm::person_estimates(erm_out, allperson = TRUE) %>%
      as.data.frame() %>%
      pull(WLE)
    # item locations
    thresholds <- item.locations$location
  }

  #create data frame with 0 rows and 2 columns
  df.locations <- data.frame(matrix(ncol = 2, nrow = 0))
  #provide column names
  colnames(df.locations) <- c('type', 'locations')
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
  df.locations$locations<-c(pthetas,thresholds)
  # change type to class factor
  df.locations$type<-as.factor(df.locations$type)

  # we need to make a new dataframe for the test information plot/curve
  psimatrix <- data.frame(matrix(ncol = 2, nrow = 201))
  names(psimatrix) <- c("psY","psX")
  # this gets 1001 "dots" for the scale information variable y
  psimatrix$psY <- test_info(erm_out, seq(lo, hi, length.out = 201L))
  # this is the x variable in the TIF figure
  psimatrix$psX <- seq(lo, hi, length.out = 201L)

  # check if TIF goes above 3.3
  peak.tif <- psimatrix %>% dplyr::slice(which.max(psY)) %>% dplyr::select(psY) %>% pull()

  if (peak.tif > cutoff - 0.01) {
    # Indicate which values are above below by a new variable with TRUE/FALSE
    psimatrix <- psimatrix %>%
      mutate(tif_above_cutoff = case_when(psY >= cutoff ~ TRUE,
                                          TRUE ~ FALSE))
    # now find where the cutoff points are for 3.33 on the theta (x) variable
    # this provides the highest and lowest value into two variables
    psep_min <- psimatrix %>%
      dplyr::filter(tif_above_cutoff == TRUE) %>%
      dplyr::slice(which.min(psX)) %>%
      pull(psX)

    psep_max <- psimatrix %>%
      dplyr::filter(tif_above_cutoff == TRUE) %>%
      dplyr::slice(which.max(psX)) %>%
      pull(psX)

    # calculate how many participants cross the cutoffs
    nCeilingRel<-length(which(pthetas > psep_max))
    nFloorRel<-length(which(pthetas < psep_min))
    nWithinRel<-(length(pthetas)-(nCeilingRel+nFloorRel))
    # Retrieve the lowest and highest item thresholds into vector variables
    min_thresh <- df.locations %>%
      dplyr::filter(type == "Item thresholds") %>%
      arrange(locations) %>%
      dplyr::slice(1) %>%
      pull()
    max_thresh <- df.locations %>%
      dplyr::filter(type == "Item thresholds") %>%
      arrange(desc(locations)) %>%
      dplyr::slice(1) %>%
      pull()

    # calculate how many participants cross the cutoffs
    nCeilingThresh<-length(which(pthetas > max_thresh))
    nFloorThresh<-length(which(pthetas < min_thresh))

    psep_caption <- paste0("Test Information ",cutoff, " (PSI = ",psi_tif,") is reached between ", round(psep_min,2), " and ", round(psep_max,2), " logits, where ",
                           round(nWithinRel/length(pthetas)*100,1), "% of the participants are located. \n",
                           round(nCeilingRel/length(pthetas)*100,1), "% of participants have locations above the area where the scale reaches TIF = ", cutoff,
                           " and ",
                           round(nFloorRel/length(pthetas)*100,1), "% are located below. \n",
                           round(nCeilingThresh/length(pthetas)*100,1), "% have person locations above the highest item threshold (",
                           round(max_thresh,2), ") and ", round(nFloorThresh/length(pthetas)*100,1), "% are below the lowest item threshold (",
                           round(min_thresh,2), ").")
  } else {
    psep_min = 0
    psep_max = 0
    psep_caption <- paste0("Test information is not above ",cutoff, " at any part of the scale.")
  }

  # make basic plot
  TIFplot <- ggplot(psimatrix) +
    geom_line(aes(x = psX, y = psY, group = 1), color = "black", linewidth = 1) +
    geom_hline(yintercept = 3.33, color = "#e83c63", linetype = 2, linewidth = 0.6) +
    geom_hline(yintercept = 5, color = "#e83c63", linetype = 2, linewidth = 0.6) +
    annotate("text", label = "PSI = 0.7", fontface = "italic",
             x = lo+0.2, y = 3.12,
             color = "#e83c63") +
    annotate("text", label = "PSI = 0.8", fontface = "italic",
             x = lo+0.2, y = 4.8,
             color = "#e83c63") +
    scale_y_continuous(breaks = seq(0, 8, by = 1)) +
    scale_x_continuous(breaks = seq(lo, hi, by = 1)) +
    labs(x = "Location (logit scale)", y = "Test information") +
    labs(caption = paste0(psep_caption)) +
    theme(plot.caption = element_text(hjust = 0, face = "italic")) +
    theme(
      panel.background = element_rect(fill = "#ebf5f0",
                                      colour = "#ebf5f0",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                      colour = "white"),
      panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                      colour = "white")
    )

  ## Add PSI info for optional use

  # estimate person location/theta mean and SD
  person.locations.estimate <- person.parameter(erm_out)
  ple <- person.locations.estimate$theta.table %>%
    as.data.frame() %>%
    dplyr::filter(Interpolated == FALSE)
  pleMean <- mean(ple$`Person Parameter`)
  pleSD <- sd(ple$`Person Parameter`)

  # estimate person theta SE mean and sd
  ple.se <- person.locations.estimate$se.theta %>%
    as_tibble()
  pleSEmean <- round(mean(ple.se$NAgroup1),2)
  pleSEsd <- round(sd(ple.se$NAgroup1),2)
  # test information = 1/SE^2, and PSI = 1-SE^2
  ple.se <- ple.se %>%
    mutate(TIF = 1/NAgroup1^2,
           PSI = 1-NAgroup1^2)

  sampleTIFmean <- 1/pleSEmean^2
  sampleTIFsd <- sd(ple.se$TIF)
  sampleTIFse <- sampleTIFsd/sqrt(length(ple.se$NAgroup1))
  sampleTIFci95 <- sampleTIFse*1.96
  samplePSImean <- round(mean(ple.se$PSI),2)
  samplePSIsd <- sd(ple.se$PSI)
  samplePSIse <- samplePSIsd/sqrt(length(ple.se$NAgroup1))
  samplePSIci95 <- round(samplePSIse*1.96,2)
  ermpsi <- eRm::SepRel(person.locations.estimate)

  TIFplotPSI <- TIFplot +
    geom_segment(aes(x = pleMean-pleSD, xend = pleMean+pleSD, y = sampleTIFmean, yend = sampleTIFmean),
                 alpha = 0.9, color = "darkgrey", linetype = 1) +
    geom_errorbar(aes(x = pleMean, ymin = sampleTIFmean-sampleTIFsd, ymax = sampleTIFmean+sampleTIFsd),
                  alpha = 0.8, color = "darkgrey", linetype = 1, width = 0.2) +
    geom_point(aes(x = pleMean, y = sampleTIFmean,),
               size = 4, shape = 18, alpha = 0.8, color = "#e83c63") +
    annotate('label', label = glue("Characteristics of current sample:\n
                                      Person theta mean (red dot) and standard deviation (horizontal line)\n
                                      and TIF mean (dot) and SD (vertical line). SEM mean/SD is {pleSEmean}/{pleSEsd}.\n
                                      Person Separation Index (Wright & Stone, 1999) = {round(ermpsi$sep.rel,2)},\n
                                      (Embretson & Reise, 2000) = {samplePSImean}."),
             x = -2, y = 0.7, lineheight = .5, hjust = 0, vjust = 0.5,
             label.padding = unit(0.4, "lines"), alpha = 0.7)

  if (cutoff != 3.33 && samplePSI == FALSE) {
    TIFplot +
      geom_hline(yintercept = cutoff, color = "orange", linetype = 2, linewidth = 0.6) +
      annotate("text", label = paste0("PSI = ",psi_tif), fontface = "italic",
               x = lo+0.2, y = cutoff-0.15,
               color = "orange")
  }
  else if (cutoff == 3.33 && samplePSI == FALSE) {
    TIFplot
  } else if (cutoff == 3.33 && samplePSI == TRUE) {
    TIFplotPSI
  }
  else if (cutoff != 3.33 && samplePSI == TRUE) {
    TIFplotPSI +
      geom_hline(yintercept = cutoff, color = "orange", linetype = 2, linewidth = 0.6) +
      annotate("text", label = paste0("PSI = ",psi_tif), fontface = "italic",
               x = lo+0.2, y = cutoff-0.18,
               color = "orange")
  }
}

##### Construct alley plots

#' Plot with infit ZSTD and item location
#' ZSTD is sample size sensitive, see "RIitemfitPCM"
#' for options
#'
#' @param dfin Dataframe with item data only
#' @param samplesize Desired sample size in multisampling (recommended 250-500)
#' @param nsamples Desired number of samples (recommended range 10-30)
#' @export
RIinfitLoc <- function(dfin, samplesize, nsamples) {
  if(missing(samplesize)) {
    df.erm<-PCM(dfin) # run PCM model
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    item.estimates <- eRm::thresholds(df.erm)
    item_difficulty <- item.estimates[["threshtable"]][["1"]]
    item_difficulty<-as.data.frame(item_difficulty)
    item.se <- item.estimates$se.thresh
    person.locations.estimate <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(person.locations.estimate)
    # collect data to df
    item.fit.table<-as.data.frame(cbind(item.fit$i.outfitMSQ, item.fit$i.infitMSQ,
                                        item.fit$i.outfitZ, item.fit$i.infitZ))
    colnames(item.fit.table)<-c("OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")
    item.fit.table$Location <- item_difficulty$Location

    # find limits of ZSTD and location
    xlims <- c(floor(min(item.fit.table$InfitZSTD)),ceiling(max(item.fit.table$InfitZSTD)))
    ylims <- c(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location)))
    # how many steps between min/max
    xbreaks <- seq(floor(min(item.fit.table$InfitZSTD)),ceiling(max(item.fit.table$InfitZSTD)), 1)
    ybreaks <- seq(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location)), 0.5)
    xdiff <- diff(c(floor(min(item.fit.table$InfitZSTD)),ceiling(max(item.fit.table$InfitZSTD))))
    ydiff <- diff(c(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location))))

    item.fit.table %>%
      rownames_to_column() %>%
      ggplot(aes(x=InfitZSTD, y=Location, label = rowname)) +
      geom_point(size = 3, color = "black") +
      geom_vline(xintercept = -2, color = "#e83c63", linetype = 2) +
      geom_vline(xintercept = 2, color = "#e83c63", linetype = 2) +
      scale_y_continuous(limits = ylims, breaks = ybreaks,
                         minor_breaks = waiver(), n.breaks = ydiff) +
      scale_x_continuous(limits = xlims, breaks = xbreaks) +
      geom_text(hjust=1.5) +
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
    df.erm<-PCM(dfin) # run PCM model
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    item.estimates <- eRm::thresholds(df.erm)
    item_difficulty <- item.estimates[["threshtable"]][["1"]]
    item_difficulty<-as.data.frame(item_difficulty)
    item.se <- item.estimates$se.thresh
    person.locations.estimate <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(person.locations.estimate)

    # ZSTD multisample
    outfitZ<-c()
    infitZ<-c()
    for (i in 1:nsamples) {
      df.new <- dfin[sample(1:nrow(dfin), samplesize), ]
      df.new <- na.omit(df.new)
      df.z <- PCM(df.new)
      ple <- person.parameter(df.z)
      item.fit.z <- eRm::itemfit(ple)
      outfitZ<-cbind(outfitZ,item.fit.z$i.outfitZ)
      infitZ<-cbind(infitZ,item.fit.z$i.infitZ)
    }
    item.fit.table<-as.data.frame(cbind(item.fit$i.outfitMSQ, item.fit$i.infitMSQ, rowMeans(outfitZ), rowMeans(infitZ)))
    colnames(item.fit.table)<-c("OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")

    item.fit.table$Location <- item_difficulty$Location

    # find limits of ZSTD and location
    xlims <- c(floor(min(item.fit.table$InfitZSTD)),ceiling(max(item.fit.table$InfitZSTD)))
    ylims <- c(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location)))
    # how many steps between min/max
    xbreaks <- seq(floor(min(item.fit.table$InfitZSTD)),ceiling(max(item.fit.table$InfitZSTD)), 1)
    ybreaks <- seq(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location)), 0.5)
    xdiff <- diff(c(floor(min(item.fit.table$InfitZSTD)),ceiling(max(item.fit.table$InfitZSTD))))
    ydiff <- diff(c(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location))))

    item.fit.table %>%
      rownames_to_column() %>%
      ggplot(aes(x=InfitZSTD, y=Location, label = rowname)) +
      geom_point(size = 3, color = "black") +
      geom_vline(xintercept = -2, color = "#e83c63", linetype = 2) +
      geom_vline(xintercept = 2, color = "#e83c63", linetype = 2) +
      scale_y_continuous(limits = ylims, breaks = ybreaks,
                         minor_breaks = waiver(), n.breaks = ydiff) +
      scale_x_continuous(limits = xlims, breaks = xbreaks) +
      geom_text(hjust=1.5) +
      theme(
        panel.background = element_rect(fill = "#ebf5f0",
                                        colour = "#ebf5f0",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "white")
      )
  }
}

#' Plot with outfit ZSTD and item location
#' ZSTD is sample size sensitive, see "RIitemfitPCM"
#'
#' @param dfin Dataframe with item data only
#' @param samplesize Desired sample size in multisampling
#' @param nsamples Desired number of samples (recommended range 10-50)
#' @export
RIoutfitLoc <- function(dfin, samplesize, nsamples) {
  if(missing(samplesize)) {
    df.erm<-PCM(dfin) # run PCM model
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    item.estimates <- eRm::thresholds(df.erm)
    item_difficulty <- item.estimates[["threshtable"]][["1"]]
    item_difficulty<-as.data.frame(item_difficulty)
    item.se <- item.estimates$se.thresh
    person.locations.estimate <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(person.locations.estimate)
    # collect data to df
    item.fit.table<-as.data.frame(cbind(item.fit$i.outfitMSQ, item.fit$i.infitMSQ,
                                        item.fit$i.outfitZ, item.fit$i.infitZ))
    colnames(item.fit.table)<-c("OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")
    item.fit.table$Location <- item_difficulty$Location

    # find limits of ZSTD and location
    xlims <- c(floor(min(item.fit.table$OutfitZSTD)),ceiling(max(item.fit.table$OutfitZSTD)))
    ylims <- c(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location)))
    # how many steps between min/max
    xbreaks <- seq(floor(min(item.fit.table$OutfitZSTD)),ceiling(max(item.fit.table$OutfitZSTD)), 1)
    ybreaks <- seq(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location)), 0.5)
    xdiff <- diff(c(floor(min(item.fit.table$OutfitZSTD)),ceiling(max(item.fit.table$OutfitZSTD))))
    ydiff <- diff(c(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location))))

    item.fit.table %>%
      rownames_to_column() %>%
      ggplot(aes(x=OutfitZSTD, y=Location, label = rowname)) +
      geom_point(size = 3, color = "black") +
      geom_vline(xintercept = -2, color = "#e83c63", linetype = 2) +
      geom_vline(xintercept = 2, color = "#e83c63", linetype = 2) +
      scale_y_continuous(limits = ylims, breaks = ybreaks,
                         minor_breaks = waiver(), n.breaks = ydiff) +
      scale_x_continuous(limits = xlims, breaks = xbreaks) +
      geom_text(hjust=1.5) +
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
    df.erm<-PCM(dfin) # run PCM model
    # get estimates, code borrowed from https://bookdown.org/chua/new_rasch_demo2/PC-model.html
    item.estimates <- eRm::thresholds(df.erm)
    item_difficulty <- item.estimates[["threshtable"]][["1"]]
    item_difficulty<-as.data.frame(item_difficulty)
    item.se <- item.estimates$se.thresh
    person.locations.estimate <- person.parameter(df.erm)
    item.fit <- eRm::itemfit(person.locations.estimate)

    # ZSTD multisample
    outfitZ<-c()
    infitZ<-c()
    for (i in 1:nsamples) {
      df.new <- dfin[sample(1:nrow(dfin), samplesize), ]
      df.new <- na.omit(df.new)
      df.z <- PCM(df.new)
      ple <- person.parameter(df.z)
      item.fit.z <- eRm::itemfit(ple)
      outfitZ<-cbind(outfitZ,item.fit.z$i.outfitZ)
      infitZ<-cbind(infitZ,item.fit.z$i.infitZ)
    }
    item.fit.table<-as.data.frame(cbind(item.fit$i.outfitMSQ, item.fit$i.infitMSQ, rowMeans(outfitZ), rowMeans(infitZ)))
    colnames(item.fit.table)<-c("OutfitMSQ", "InfitMSQ", "OutfitZSTD", "InfitZSTD")

    item.fit.table$Location <- item_difficulty$Location

    # find limits of ZSTD and location
    xlims <- c(floor(min(item.fit.table$OutfitZSTD)),ceiling(max(item.fit.table$OutfitZSTD)))
    ylims <- c(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location)))
    # how many steps between min/max
    xbreaks <- seq(floor(min(item.fit.table$OutfitZSTD)),ceiling(max(item.fit.table$OutfitZSTD)), 1)
    ybreaks <- seq(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location)), 0.5)
    xdiff <- diff(c(floor(min(item.fit.table$OutfitZSTD)),ceiling(max(item.fit.table$OutfitZSTD))))
    ydiff <- diff(c(floor(min(item.fit.table$Location)),ceiling(max(item.fit.table$Location))))

    item.fit.table %>%
      rownames_to_column() %>%
      ggplot(aes(x=OutfitZSTD, y=Location, label = rowname)) +
      geom_point(size = 3, color = "black") +
      geom_vline(xintercept = -2, color = "#e83c63", linetype = 2) +
      geom_vline(xintercept = 2, color = "#e83c63", linetype = 2) +
      scale_y_continuous(limits = ylims, breaks = ybreaks,
                         minor_breaks = waiver(), n.breaks = ydiff) +
      scale_x_continuous(limits = xlims, breaks = xbreaks) +
      geom_text(hjust=1.5) +
      theme(
        panel.background = element_rect(fill = "#ebf5f0",
                                        colour = "#ebf5f0",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "white")
      )
  }
}

