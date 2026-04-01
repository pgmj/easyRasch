#' DIF analysis - requires having set up dif.variables previously
#'
#' Makes use of the psychotree package, which also allows for interactions
#' between DIF variables, see `RIdifTable2()`.
#'
#' DIF variables need to be vectors with the same length as the number of rows
#' in the dataset.
#'
#' sample usage: RIdifTable(df, dif.age)
#'
#' @param dfin Dataframe with item data only
#' @param dif.var DIF variable
#' @param cutoff Cutoff in item location logit difference for table highlighting
#' @export
RIdifTable <- function(dfin, dif.var, cutoff = 0.5) {
  df.tree <- data.frame(matrix(ncol = 0, nrow = nrow(dfin))) # we need to make a new dataframe
  df.tree$difdata <- as.matrix(dfin) # containing item data in a nested dataframe
  # and DIF variables:
  df.tree$dif.var<-dif.var
  pctree.out<-pctree(difdata ~ dif.var, data = df.tree)

  if(min(as.matrix(dfin), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")
  } else if(na.omit(dfin) %>% nrow() == 0) {
    stop("No complete cases in data.")
  } else if(max(as.matrix(dfin), na.rm = T) == 1) {
    pctree.out <- raschtree(difdata ~ dif.var, data = df.tree)
  } else if(max(as.matrix(dfin), na.rm = T) > 1) {
    pctree.out <- pctree(difdata ~ dif.var, data = df.tree)
  }

  if(nrow(itempar(pctree.out) %>% as.data.frame() %>% t()) > 1) {
    plot(pctree.out)

    table <- itempar(pctree.out) %>% # identify the nodes to compare (see plot above)
      as.data.frame() %>%
      t() %>%
      as.data.frame() %>%
      mutate('Mean location' = rowMeans(., na.rm = TRUE),
             StDev = rowSds(as.matrix(.))) %>%
      rowwise() %>%
      mutate(MaxDiff = (max(c_across(c(1:(ncol(.)-2))))) - min(c_across(c(1:(ncol(.)-2))))) %>%
      ungroup() %>%
      mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
      rownames_to_column(var = "Item") %>%
      mutate(Item = names(dfin)) %>%
      dplyr::relocate(MaxDiff, .after = last_col()) %>%
      formattable(list(
        'MaxDiff' =
          formatter("span", style = ~ formattable::style(color = ifelse(MaxDiff < -cutoff, "red",
                                                                        ifelse(MaxDiff > cutoff, "red",  "black"))))),
        table.attr = 'class=\"table table-striped\" style="font-size: 15px; font-family: Sans"')

    return(list(plot = plot(pctree.out),
                table = table)
    )

  } else {
    print("No statistically significant DIF found.")
  }
}


#' DIF PCM interaction analysis
#'
#' Makes use of the psychotree package. This function is for interaction between
#' two DIF variables
#'
#' DIF variables need to be vectors with the same length as the number of rows
#' in the dataset.
#'
#' sample usage: `RIdifTable2(df, dif.age, dif.gender)`
#'
#' @param dfin Dataframe with item data only
#' @param dif.var DIF variable
#' @param cutoff Cutoff in item location logit difference for table highlighting
#' @export
RIdifTable2 <- function(dfin, dif.var1, dif.var2, cutoff = 0.5) {

  df.tree <- data.frame(matrix(ncol = 0, nrow = nrow(dfin))) # we need to make a new dataframe
  df.tree$difdata <- as.matrix(dfin) # containing item data in a nested dataframe
  # and DIF variables:
  df.tree$dif.var1 <- dif.var1
  df.tree$dif.var2 <- dif.var2

  if(min(as.matrix(dfin), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")
  } else if(na.omit(dfin) %>% nrow() == 0) {
    stop("No complete cases in data.")
  } else if(max(as.matrix(dfin), na.rm = T) == 1) {
    pctree.out <- raschtree(difdata ~ dif.var1 + dif.var2, data = df.tree)
  } else if(max(as.matrix(dfin), na.rm = T) > 1) {
    pctree.out <- pctree(difdata ~ dif.var1 + dif.var2, data = df.tree)
  }



  if(nrow(itempar(pctree.out) %>% as.data.frame() %>% t()) > 1) {
    plot(pctree.out)

    table <- itempar(pctree.out) %>% # identify the nodes to compare (see plot above)
      as.data.frame() %>%
      t() %>%
      as.data.frame() %>%
      mutate('Mean location' = rowMeans(., na.rm = TRUE), StDev = rowSds(as.matrix(.))) %>%
      rowwise() %>%
      mutate(MaxDiff = (max(c_across(c(1:(ncol(.)-2))))) - min(c_across(c(1:(ncol(.)-2))))) %>%
      ungroup() %>%
      mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
      rownames_to_column(var = "Item") %>%
      mutate(Item = names(dfin)) %>%
      dplyr::relocate(MaxDiff, .after = last_col()) %>%
      formattable(list(
        'MaxDiff' =
          formatter("span", style = ~ formattable::style(color = ifelse(MaxDiff < -cutoff, "red",
                                                                        ifelse(MaxDiff > cutoff, "red",  "black"))))),
        table.attr = 'class=\"table table-striped\" style="font-size: 15px; font-family: Lato"')

    return(list(plot = plot(pctree.out),
                table = table)
    )

  } else {
    print("No statistically significant DIF found.")
  }
}

#' Create a DIF line graph, showing groups' PCM item locations
#'
#' @param dfin Dataframe with item data only
#' @param dif.var DIF variable
#' @export
RIdifFigure <- function(dfin, dif.var) {
  df.tree <- data.frame(matrix(ncol = 0, nrow = nrow(dfin))) # we need to make a new dataframe
  df.tree$difdata <- as.matrix(dfin) # containing item data in a nested dataframe
  # and DIF variables:
  df.tree$dif.var<-dif.var
  pctree.out<-pctree(difdata ~ dif.var, data = df.tree)

  if(nrow(itempar(pctree.out) %>% as.data.frame() %>% t()) > 1) {
    # create dataframe for ggplot
    pctree.par <- itempar(pctree.out) %>%
      as.data.frame() %>%
      t() %>%
      as.data.frame()
    pctree.par$Item<-names(dfin)
    pctree.par$item <- NULL
    rownames(pctree.par)<-NULL
    pctree.par <- melt(pctree.par, id.vars = "Item")
    names(pctree.par)<-c("Item", "Group", "Logits")
    pctree.par$Item <- factor(pctree.par$Item, levels = names(dfin)) # order items
    # make plot
    ggplot(pctree.par, aes(x=Item, y=Logits, color=Group, group = Group)) +
      geom_line(linewidth = 1.3) +
      geom_point(size = 2.5)
  } else {
    print("No statistically significant DIF found.")
  }
}

#' Create a DIF line graph over time, showing groups' PCM item locations
#'
#' This function is specifically intended for examining DIF over time, to
#' create a figure that follows each item location on the y axis
#' with time on the x axis.
#'
#' @param dfin Dataframe with item data only
#' @param dif.var DIF variable
#' @export
RIdifFigTime <- function(dfin, dif.var) {
  df.tree <- data.frame(matrix(ncol = 0, nrow = nrow(dfin))) # we need to make a new dataframe
  df.tree$difdata <- as.matrix(dfin) # containing item data in a nested dataframe
  # and DIF variables:
  df.tree$dif.var<-dif.var
  pctree.out<-pctree(difdata ~ dif.var, data = df.tree)

  if(nrow(itempar(pctree.out) %>% as.data.frame() %>% t()) > 1) {
    # create dataframe for ggplot
    pctree.par <- itempar(pctree.out) %>%
      as.data.frame() %>%
      t() %>%
      as.data.frame()
    pctree.par$Item<-names(dfin)
    pctree.par$item <- NULL
    rownames(pctree.par)<-NULL
    pctree.par <- melt(pctree.par, id.vars = "Item")
    names(pctree.par)<-c("Item", "Group", "Logits")
    pctree.par$Item <- factor(pctree.par$Item, levels = names(dfin))
    # make plot
    ggplot(pctree.par, aes(x=Group, y=Logits, color=Item, group = Item)) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 3) +
      xlab("DIF node/Time point (see DIF table)")
  } else {
    print("No statistically significant DIF found.")
  }
}

#' Create a DIF line graph for item PCM thresholds
#'
#' Produces a panel of linegraphs showing item thresholds over DIF nodes.
#'
#' NOTE: only works with PCM data where all variables have multiple thresholds,
#' since the `threshpar()` function has problems when dichotomous data are
#' included.
#'
#' @param dfin Dataframe with item data only
#' @param dif.var DIF variable
#' @export
RIdifFigThresh <- function(dfin, dif.var) {
  df.tree <- data.frame(matrix(ncol = 0, nrow = nrow(dfin))) # we need to make a new dataframe
  df.tree$difdata <- as.matrix(dfin) # containing item data in a nested dataframe
  # and DIF variables:
  df.tree$dif.var <- dif.var
  pctree.out <- pctree(difdata ~ dif.var, data = df.tree)

  if (nrow(itempar(pctree.out) %>% as.data.frame() %>% t()) > 1) {
    # create dataframe for ggplot
    unidif <- threshpar(pctree.out) %>%
      as.data.frame() %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column("Threshh") %>%
      pivot_longer(where(is.numeric)) %>%
      tidyr::separate(Threshh, c("Item", "Threshold"), sep = "-") %>%
      tidyr::separate(Item, c(NA, "Item"), sep = "ata") %>%
      dplyr::rename(
        "DIF node" = name,
        Location = value
      ) %>%
      mutate(`DIF node` = as.numeric(`DIF node`)) %>%
      mutate(Threshold = dplyr::recode(Threshold,"C1"="T1",
                                       "C2"="T2",
                                       "C3"="T3",
                                       "C4"="T4",
                                       "C5"="T5",
                                       "C6"="T6",
                                       "C7"="T7",
                                       "C8"="T8",
                                       "C9"="T9",
                                       "C10"="T10")) %>%
      mutate(Item = factor(Item, levels = names(dfin)))

    ggplot(unidif, (aes(
      x = factor(`DIF node`),
      y = Location,
      group = Threshold,
      color = Threshold
    ))) +
      geom_line() +
      geom_point() +
      xlab("DIF node") +
      facet_wrap(~Item)
  } else {
    print("No statistically significant DIF found.")
  }
}


#' DIF analysis dichotomous - requires having set up dif.variables
#'
#' Makes use of the psychotree package, which also allows for interactions
#' between DIF variables, which is not implemented in this function (yet).
#'
#' DIF variables need to be vectors with the same length as the number of rows
#' in the dataset.
#'
#' sample usage: RIdifTable(df, dif.age)
#'
#' @param dfin Dataframe with item data only
#' @param dif.var DIF variable
#' @param cutoff Cutoff in item location logit difference for table highlighting
#' @export
RIdifTableRM <- function(dfin, dif.var, cutoff = 0.5) {
  df.tree <- data.frame(matrix(ncol = 0, nrow = nrow(dfin))) # we need to make a new dataframe
  df.tree$difdata <- as.matrix(dfin) # containing item data in a nested dataframe
  # and DIF variables:
  df.tree$dif.var<-dif.var
  pctree.out<-raschtree(difdata ~ dif.var, data = df.tree)

  if(nrow(itempar(pctree.out) %>% as.data.frame() %>% t()) > 1) {
    plot(pctree.out)

    itempar(pctree.out) %>% # identify the nodes to compare (see plot above)
      as.data.frame() %>%
      t() %>%
      as.data.frame() %>%
      mutate('Mean location' = rowMeans(., na.rm = TRUE), StDev = rowSds(as.matrix(.))) %>%
      rowwise() %>%
      mutate(MaxDiff = (max(c_across(c(1:(ncol(.)-2))))) - min(c_across(c(1:(ncol(.)-2))))) %>%
      ungroup() %>%
      mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
      rownames_to_column(var = "Item") %>%
      mutate(Item = names(dfin)) %>%
      dplyr::relocate(MaxDiff, .after = last_col()) %>%
      formattable(list(
        'MaxDiff' =
          formatter("span", style = ~ formattable::style(color = ifelse(MaxDiff < -cutoff, "red",
                                                                        ifelse(MaxDiff > cutoff, "red",  "black"))))),
        table.attr = 'class=\"table table-striped\" style="font-size: 15px; font-family: Lato"')

  } else {
    print("No statistically significant DIF found.")
  }
}


#' Create a DIF line graph, showing groups' RM item locations
#'
#' @param dfin Dataframe with item data only
#' @param dif.var DIF variable
#' @export
RIdifFigureRM <- function(dfin, dif.var) {
  df.tree <- data.frame(matrix(ncol = 0, nrow = nrow(dfin))) # we need to make a new dataframe
  df.tree$difdata <- as.matrix(dfin) # containing item data in a nested dataframe
  # and DIF variables:
  df.tree$dif.var<-dif.var
  pctree.out<-raschtree(difdata ~ dif.var, data = df.tree)
  if(nrow(itempar(pctree.out) %>% as.data.frame() %>% t()) > 1) {
    # make a line graph to visualize differences
    pctree.par<- itempar(pctree.out) %>%
      as.data.frame() %>%
      t() %>%
      as.data.frame()
    pctree.par$Item<-names(dfin)
    pctree.par$item <- NULL
    rownames(pctree.par)<-NULL
    #names(pctree.par)<-c("Åk 9","Gy 2","Item")
    pctree.par <- melt(pctree.par, id.vars = "Item")
    names(pctree.par)<-c("Item", "Group", "Logits")
    #pctree.par$Item<-str_remove_all(pctree.par$Item, "[difdata]") # remove IF from item labels
    ggplot(pctree.par, aes(x=Item, y=Logits, color=Group, group = Group)) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 2.5)
  } else {
    print("No statistically significant DIF found.")
  }
}

#' DIF PCM analysis with table output for item locations
#'
#' Makes use of the eRm package function `LRtest()`. Outputs a table with item
#' average locations, group differences, and standard errors.
#'
#' DIF variables need to be factors with the same length as the number of rows
#' in the dataset.
#'
#' sample usage: RIdifTableE(df, dif.age)
#'
#' @param dfin Dataframe with item data only
#' @param dif.var DIF variable
#' @param model Defaults to "PCM", optional "RM" under development
#' @param sort Set to TRUE to sort the table based on DIF size
#' @param cutoff Cutoff in item location logit difference for table highlighting
#' @param fontfamily Set table font
#' @export
RIdifTableLR <- function(dfin, dif.var, model = "PCM", sort = FALSE,
                         fontfamily = "sans-serif", cutoff = 0.5) {
  if (model == "PCM") {
    erm.out <- PCM(dfin)
    lrt.out <- LRtest(erm.out, splitcr = droplevels(dif.var))
    groups <- levels(droplevels(dif.var)) # remove unused factor levels
    nr.groups <- length(groups) # get number of subgroups

    # get item location for each subgroupx
    itemthresh <- thresholds(lrt.out[["fitobj"]][["1"]]) %>%
      .[["threshpar"]] %>%
      as.data.frame() %>%
      rownames()

    lrt.locs <- data.frame(matrix(ncol = 1, nrow = length(lrt.out[["betalist"]][[1]])))
    for (i in 1:nr.groups){
      lrt.locs[[i]] <- thresholds(lrt.out[["fitobj"]][[i]]) %>%
        .[["threshpar"]] %>%
        as.data.frame(nm = groups[i]) %>%
        pull(groups[i])
    }
    lrt.locs <- setNames(lrt.locs, groups)
    lrt.locs$Item <- itemthresh

    # get thresholds from non-DIF-split model
    erm.par <- eRm::thresholds(erm.out)
    lrt.locs$All <- erm.par[["threshpar"]] %>%
      as.data.frame(nm = "Location") %>%
      pull(Location)

    # bind in one df
    lrt.diff <- lrt.locs %>%
      mutate_if(is.numeric, ~ round(.x, 3)) %>%
      tidyr::separate(Item, c(NA,"Item"), sep = "beta ") %>%
      tidyr::separate(Item, c("Item","Threshold"), sep = "\\.") %>%
      mutate(Item = factor(Item, levels = names(dfin)))

    # add standard errors for all subgroups + whole group
    lrt.se <- data.frame(matrix(ncol = 1, nrow = length(lrt.out$selist[[1]])))
    for (i in 1:nr.groups){
      lrt.se[[i]] <- thresholds(lrt.out[["fitobj"]][[i]]) %>%
        .[["se.thresh"]] %>%
        as.data.frame(nm = "Location") %>%
        pull(Location)
    }
    lrt.se$All <- erm.par$se.thresh %>%
      as.data.frame(nm = "sem") %>%
      pull(sem)
    lrt.se <- setNames(lrt.se, c(groups,"All"))
    lrt.se$Item <- lrt.diff$Item
    lrt.se$Threshold <- lrt.diff$Threshold

    # make version with average item location
    lrt.avg <- lrt.diff %>%
      pivot_wider(names_from = "Item",
                  values_from = all_of(c(groups,"All")),
                  names_sep = ".") %>%
      t() %>%
      as.data.frame()
    lrt.avg <- lrt.avg[-1,]

    lrt.avg <- lrt.avg %>%
      mutate(across(everything(), ~ as.numeric(.x))) %>%
      mutate(Location = rowMeans(., na.rm = T)) %>%
      mutate_if(is.double, round, digits = 3) %>%
      rownames_to_column("groupitem") %>%
      tidyr::separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

    ### add SE
    # pivot_wider for easier calculation
    lrt.avg.se <- lrt.se %>%
      pivot_wider(names_from = "Item",
                  values_from = all_of(c(groups,"All")),
                  names_sep = ".") %>%
      t() %>%
      as.data.frame()
    lrt.avg.se <- lrt.avg.se[-1,] # remove first row

    lrt.avg.se <- lrt.avg.se %>%
      mutate(across(everything(), ~ as.numeric(.x))) %>%
      mutate(SE = rowMeans(., na.rm = T)) %>%
      mutate_if(is.double, round, digits = 3) %>%
      rownames_to_column("groupitem") %>%
      tidyr::separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

    lrt.avg$SE <- lrt.avg.se$SE

    # prepare table output
    lrt.table <- lrt.avg %>%
      dplyr::select(!starts_with("V")) %>%
      pivot_wider(
        names_from = "DIFgroup",
        values_from = c("Location", "SE")
      ) %>%
      dplyr::rename_with(.fn = ~ gsub("Location_","", .x),
                         .cols = contains("Location")) %>%
      rowwise() %>%
      mutate(MaxDiff = max(c_across(c(2:(nr.groups+1)))) - min(c_across(c(2:(nr.groups+1)))),
             .before = "All") %>%
      ungroup() %>%
      mutate_if(is.double, round, digits = 3) %>%
      mutate(MaxDiff = cell_spec(MaxDiff,
                                 color = case_when(
                                   MaxDiff > cutoff ~ "red",
                                   MaxDiff < -cutoff ~ "red",
                                   TRUE ~ "black"
                                 )
      )) %>%
      rowwise() %>%
      mutate(across(all_of(groups), ~ cell_spec(.x,
                                                background = case_when(
                                                  .x == max(c_across(c(2:(nr.groups+1)))) ~ "lightblue",
                                                  .x == min(c_across(c(2:(nr.groups+1)))) ~ "burlywood",
                                                  TRUE ~ ""
                                                ))))

    if (sort == TRUE) {
      lrt.table %>%
        arrange(desc(MaxDiff)) %>%
        kbl_rise() %>%
        column_spec(length(groups)+2,
                    bold = T) %>%
        column_spec(c(length(groups)+3,ncol(lrt.table)),
                    italic = T) %>%
        add_header_above(c(" " = 1, "Item locations" = nr.groups+2, "Standard errors" = nr.groups+1),
                         bold = T,
                         line_sep = 5) %>%
        kableExtra::footnote(general = paste0("Values highlighted in red are above the chosen cutoff ",
                                              cutoff,
                                              " logits. Background color brown and blue indicate the lowest and highest values among the DIF groups."))
    } else {

      lrt.table %>%
        kbl_rise() %>%
        column_spec(length(groups)+2,
                    bold = T) %>%
        column_spec(c(length(groups)+3,ncol(lrt.table)),
                    italic = T) %>%
        add_header_above(c(" " = 1, "Item locations" = nr.groups+2, "Standard errors" = nr.groups+1),
                         bold = T,
                         line_sep = 5) %>%
        kableExtra::footnote(general = paste0("Values highlighted in red are above the chosen cutoff ",
                                              cutoff,
                                              " logits. Background color brown and blue indicate the lowest and highest values among the DIF groups."))
    }
  } else if (model == "RM") {
    erm.out <- RM(dfin)
    lrt.out <- LRtest(erm.out, splitcr = droplevels(dif.var))
    groups <- levels(droplevels(dif.var)) # remove unused factor levels
    nr.groups <- length(groups) # get number of subgroups
  }

}

#' DIF PCM analysis with table output for item thresholds
#'
#' Makes use of the eRm package function `LRtest()`. Outputs a table with item
#' average locations, group differences, and standard errors.
#'
#' DIF variables need to be factors with the same length as the number of rows
#' in the dataset.
#'
#' sample usage: RIdifTableThreshE(df, dif.age, fontfamily = "Arial")
#'
#' @param dfin Dataframe with item data only
#' @param dif.var DIF variable
#' @param cutoff Cutoff in item location logit difference for table highlighting
#' @param fontfamily Set table font
#' @export
RIdifThreshTblLR <- function(dfin, dif.var,
                             fontfamily = "sans-serif", cutoff = 0.5) {
  erm.out <- PCM(dfin)
  lrt.out <- LRtest(erm.out, splitcr = droplevels(dif.var))
  groups <- levels(droplevels(dif.var)) # remove unused factor levels
  nr.groups <- length(groups) # get number of subgroups

  # get item location for each subgroupx
  itemthresh <- thresholds(lrt.out[["fitobj"]][["1"]]) %>%
    .[["threshpar"]] %>%
    as.data.frame() %>%
    rownames()

  lrt.locs <- data.frame(matrix(ncol = 1, nrow = length(lrt.out[["betalist"]][[1]])))
  for (i in 1:nr.groups){
    lrt.locs[[i]] <- thresholds(lrt.out[["fitobj"]][[i]]) %>%
      .[["threshpar"]] %>%
      as.data.frame(nm = groups[i]) %>%
      pull(groups[i])
  }
  lrt.locs <- setNames(lrt.locs, groups)
  lrt.locs$Item <- itemthresh

  # get thresholds from non-DIF-split model
  erm.par <- eRm::thresholds(erm.out)
  lrt.locs$All <- erm.par[["threshpar"]] %>%
    as.data.frame(nm = "Location") %>%
    pull(Location)

  # bind in one df
  lrt.diff <- lrt.locs %>%
    mutate_if(is.numeric, ~ round(.x, 3)) %>%
    tidyr::separate(Item, c(NA,"Item"), sep = "beta ") %>%
    tidyr::separate(Item, c("Item","Threshold"), sep = "\\.") %>%
    mutate(Item = factor(Item, levels = names(dfin)))

  # add standard errors for all subgroups + whole group
  lrt.se <- data.frame(matrix(ncol = 1, nrow = length(lrt.out$selist[[1]])))
  for (i in 1:nr.groups){
    lrt.se[[i]] <- thresholds(lrt.out[["fitobj"]][[i]]) %>%
      .[["se.thresh"]] %>%
      as.data.frame(nm = "Location") %>%
      pull(Location)
  }
  lrt.se$All <- erm.par$se.thresh %>%
    as.data.frame(nm = "sem") %>%
    pull(sem)
  lrt.se <- setNames(lrt.se, c(groups,"All"))
  lrt.se$Item <- lrt.diff$Item
  lrt.se$Threshold <- lrt.diff$Threshold

  # make version with average item location
  lrt.avg <- lrt.diff %>%
    pivot_wider(names_from = "Item",
                values_from = all_of(c(groups,"All")),
                names_sep = ".") %>%
    t() %>%
    as.data.frame()
  lrt.avg <- lrt.avg[-1,]

  lrt.avg <- lrt.avg %>%
    mutate(across(everything(), ~ as.numeric(.x))) %>%
    mutate(Location = rowMeans(., na.rm = T)) %>%
    mutate_if(is.double, round, digits = 3) %>%
    rownames_to_column("groupitem") %>%
    tidyr::separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

  ### add SE
  # pivot_wider for easier calculation
  lrt.avg.se <- lrt.se %>%
    pivot_wider(names_from = "Item",
                values_from = all_of(c(groups,"All")),
                names_sep = ".") %>%
    t() %>%
    as.data.frame()
  lrt.avg.se <- lrt.avg.se[-1,] # remove first row

  lrt.avg.se <- lrt.avg.se %>%
    mutate(across(everything(), ~ as.numeric(.x))) %>%
    mutate(SE = rowMeans(., na.rm = T)) %>%
    mutate_if(is.double, round, digits = 3) %>%
    rownames_to_column("groupitem") %>%
    tidyr::separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

  lrt.avg$SE <- lrt.avg.se$SE

  # prepare table
  lrt.plot <- lrt.diff %>%
    pivot_longer(any_of(c(groups,"All")),
                 names_to = "DIFgroup",
                 values_to = "Location")
  lrt.plot.se <- lrt.se %>%
    pivot_longer(any_of(c(groups,"All")),
                 names_to = "DIFgroup",
                 values_to = "SE")
  lrt.plot <- full_join(lrt.plot,lrt.plot.se, by = c("Item","Threshold","DIFgroup"))

  lrt.table <- lrt.plot %>%
    dplyr::select(!starts_with("V")) %>%
    pivot_wider(names_from = "DIFgroup",
                values_from = c("Location","SE")) %>%
    dplyr::rename_with(.fn = ~ gsub("Location_","", .x),
                       .cols = contains("Location")) %>%
    rowwise() %>%
    mutate(MaxDiff = max(c_across(c(3:(nr.groups+2)))) - min(c_across(c(3:(nr.groups+2)))),
           .before = "All") %>%
    ungroup() %>%
    mutate_if(is.double, round, digits = 3) %>%
    mutate(MaxDiff = cell_spec(MaxDiff,
                               color = case_when(
                                 MaxDiff > cutoff ~ "red",
                                 MaxDiff < -cutoff ~ "red",
                                 TRUE ~ "black"
                               )
    )) %>%
    rowwise() %>%
    mutate(across(all_of(groups), ~ cell_spec(.x,
                                              background = case_when(
                                                .x == max(c_across(c(3:(nr.groups+2)))) ~ "lightblue",
                                                .x == min(c_across(c(3:(nr.groups+2)))) ~ "burlywood",
                                                TRUE ~ ""
                                              ))))

  lrt.table %>%
    dplyr::select(!Item) %>%
    dplyr::rename(`Item threshold` = Threshold) %>%
    kbl_rise() %>%
    pack_rows(index = table(lrt.table$Item)) %>%
    column_spec(column = c(1,nr.groups+2),
                bold = T) %>%
    column_spec(column = c(nr.groups+3,(ncol(lrt.table)-1)),
                italic = T) %>%
    add_header_above(c(" " = 1, "Threshold locations" = nr.groups+2, "Standard errors" = nr.groups+1),
                     bold = T,
                     line_sep = 5) %>%
    kableExtra::footnote(general = paste0("Values highlighted in red are above the chosen cutoff ",
                                          cutoff,
                                          " logits. Background color brown and blue indicate the lowest and highest values among the DIF groups.")
    )

}


#' DIF PCM analysis with panel figure output for items' average locations
#'
#' Makes use of the eRm package function `LRtest()`. Outputs a panel of figures
#' with item average locations and 95% confidence intervals.
#'
#' DIF variables need to be factors with the same length as the number of rows
#' in the dataset.
#'
#' sample usage: RIdifTableE(df, dif.age)
#'
#' @param dfin Dataframe with item data only
#' @param dif.var DIF variable
#' @export
RIdifFigureLR <- function(dfin, dif.var) {
  erm.out <- PCM(dfin)
  lrt.out <- LRtest(erm.out, splitcr = droplevels(dif.var))
  groups <- levels(droplevels(dif.var)) # remove unused factor levels
  nr.groups <- length(groups) # get number of subgroups

  # get item location for each subgroupx
  itemthresh <- thresholds(lrt.out[["fitobj"]][["1"]]) %>%
    .[["threshpar"]] %>%
    as.data.frame() %>%
    rownames()

  lrt.locs <- data.frame(matrix(ncol = 1, nrow = length(lrt.out[["betalist"]][[1]])))
  for (i in 1:nr.groups){
    lrt.locs[[i]] <- thresholds(lrt.out[["fitobj"]][[i]]) %>%
      .[["threshpar"]] %>%
      as.data.frame(nm = groups[i]) %>%
      pull(groups[i])
  }
  lrt.locs <- setNames(lrt.locs, groups)
  lrt.locs$Item <- itemthresh

  # get thresholds from non-DIF-split model
  erm.par <- eRm::thresholds(erm.out)
  lrt.locs$All <- erm.par[["threshpar"]] %>%
    as.data.frame(nm = "Location") %>%
    pull(Location)

  # bind in one df
  lrt.diff <- lrt.locs %>%
    mutate_if(is.numeric, ~ round(.x, 3)) %>%
    tidyr::separate(Item, c(NA,"Item"), sep = "beta ") %>%
    tidyr::separate(Item, c("Item","Threshold"), sep = "\\.") %>%
    mutate(Item = factor(Item, levels = names(dfin)))

  # add standard errors for all subgroups + whole group
  lrt.se <- data.frame(matrix(ncol = 1, nrow = length(lrt.out$selist[[1]])))
  for (i in 1:nr.groups){
    lrt.se[[i]] <- thresholds(lrt.out[["fitobj"]][[i]]) %>%
      .[["se.thresh"]] %>%
      as.data.frame(nm = "Location") %>%
      pull(Location)
  }
  lrt.se$All <- erm.par$se.thresh %>%
    as.data.frame(nm = "sem") %>%
    pull(sem)
  lrt.se <- setNames(lrt.se, c(groups,"All"))
  lrt.se$Item <- lrt.diff$Item
  lrt.se$Threshold <- lrt.diff$Threshold

  # make version with average item location
  lrt.avg <- lrt.diff %>%
    pivot_wider(names_from = "Item",
                values_from = all_of(c(groups,"All")),
                names_sep = ".") %>%
    t() %>%
    as.data.frame()
  lrt.avg <- lrt.avg[-1,]

  lrt.avg <- lrt.avg %>%
    mutate(across(everything(), ~ as.numeric(.x))) %>%
    mutate(Location = rowMeans(., na.rm = T)) %>%
    mutate_if(is.double, round, digits = 3) %>%
    rownames_to_column("groupitem") %>%
    tidyr::separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

  ### add SE
  # pivot_wider for easier calculation
  lrt.avg.se <- lrt.se %>%
    pivot_wider(names_from = "Item",
                values_from = all_of(c(groups,"All")),
                names_sep = ".") %>%
    t() %>%
    as.data.frame()
  lrt.avg.se <- lrt.avg.se[-1,] # remove first row

  lrt.avg.se <- lrt.avg.se %>%
    mutate(across(everything(), ~ as.numeric(.x))) %>%
    mutate(SE = rowMeans(., na.rm = T)) %>%
    mutate_if(is.double, round, digits = 3) %>%
    rownames_to_column("groupitem") %>%
    tidyr::separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

  lrt.avg$SE <- lrt.avg.se$SE

  ggplot(data = subset(lrt.avg, DIFgroup %in% groups),
         aes(
           x = factor(DIFgroup, levels = groups),
           y = Location,
           group = Item,
           color = Item
         )) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(y = Location, ymin = Location - 1.96*SE, ymax = Location + 1.96*SE),
                  width = 0.1
    ) +
    geom_point(data = subset(lrt.avg, DIFgroup == "All"),
               aes(x = (nr.groups+1)/2+0.15, y = Location),
               shape = 18,
               color = "black",
               size = 3,
               alpha = 0.6) +
    xlab("DIF node") +
    facet_wrap(~Item) +
    labs(title = "DIF for item location",
         subtitle = "Item locations are calculated as the mean of each item's threshold locations",
         caption = "Note. Error bars indicate 95% confidence interval.\nDark grey diamonds indicate item location for all participants as one group.",
         x = "DIF group",
         y = "Item location (logit scale") +
    theme(legend.position = "none",
          plot.caption = element_text(hjust = 0, face = "italic"))

}

#' DIF PCM analysis with panel figure output for item thresholds
#'
#' Makes use of the eRm package function `LRtest()`. Outputs a panel of figures
#' with item threshold locations and 95% confidence intervals.
#'
#' DIF variables need to be factors with the same length as the number of rows
#' in the dataset.
#'
#' sample usage: RIdifTableE(df, dif.age)
#'
#' @param dfin Dataframe with item data only
#' @param dif.var DIF variable
#' @export
RIdifThreshFigLR <- function(dfin, dif.var) {
  erm.out <- PCM(dfin)
  lrt.out <- LRtest(erm.out, splitcr = droplevels(dif.var))
  groups <- levels(droplevels(dif.var)) # remove unused factor levels
  nr.groups <- length(groups) # get number of subgroups

  # get item location for each subgroupx
  itemthresh <- thresholds(lrt.out[["fitobj"]][["1"]]) %>%
    .[["threshpar"]] %>%
    as.data.frame() %>%
    rownames()

  lrt.locs <- data.frame(matrix(ncol = 1, nrow = length(lrt.out[["betalist"]][[1]])))
  for (i in 1:nr.groups){
    lrt.locs[[i]] <- thresholds(lrt.out[["fitobj"]][[i]]) %>%
      .[["threshpar"]] %>%
      as.data.frame(nm = groups[i]) %>%
      pull(groups[i])
  }
  lrt.locs <- setNames(lrt.locs, groups)
  lrt.locs$Item <- itemthresh

  # get thresholds from non-DIF-split model
  erm.par <- eRm::thresholds(erm.out)
  lrt.locs$All <- erm.par[["threshpar"]] %>%
    as.data.frame(nm = "Location") %>%
    pull(Location)

  # bind in one df
  lrt.diff <- lrt.locs %>%
    mutate_if(is.numeric, ~ round(.x, 3)) %>%
    tidyr::separate(Item, c(NA,"Item"), sep = "beta ") %>%
    tidyr::separate(Item, c("Item","Threshold"), sep = "\\.") %>%
    mutate(Item = factor(Item, levels = names(dfin)))

  # add standard errors for all subgroups + whole group
  lrt.se <- data.frame(matrix(ncol = 1, nrow = length(lrt.out$selist[[1]])))
  for (i in 1:nr.groups){
    lrt.se[[i]] <- thresholds(lrt.out[["fitobj"]][[i]]) %>%
      .[["se.thresh"]] %>%
      as.data.frame(nm = "Location") %>%
      pull(Location)
  }
  lrt.se$All <- erm.par$se.thresh %>%
    as.data.frame(nm = "sem") %>%
    pull(sem)
  lrt.se <- setNames(lrt.se, c(groups,"All"))
  lrt.se$Item <- lrt.diff$Item
  lrt.se$Threshold <- lrt.diff$Threshold

  # make version with average item location
  lrt.avg <- lrt.diff %>%
    pivot_wider(names_from = "Item",
                values_from = all_of(c(groups,"All")),
                names_sep = ".") %>%
    t() %>%
    as.data.frame()
  lrt.avg <- lrt.avg[-1,]

  lrt.avg <- lrt.avg %>%
    mutate(across(everything(), ~ as.numeric(.x))) %>%
    mutate(Location = rowMeans(., na.rm = T)) %>%
    mutate_if(is.double, round, digits = 3) %>%
    rownames_to_column("groupitem") %>%
    tidyr::separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

  ### add SE
  # pivot_wider for easier calculation
  lrt.avg.se <- lrt.se %>%
    pivot_wider(names_from = "Item",
                values_from = all_of(c(groups,"All")),
                names_sep = ".") %>%
    t() %>%
    as.data.frame()
  lrt.avg.se <- lrt.avg.se[-1,] # remove first row

  lrt.avg.se <- lrt.avg.se %>%
    mutate(across(everything(), ~ as.numeric(.x))) %>%
    mutate(SE = rowMeans(., na.rm = T)) %>%
    mutate_if(is.double, round, digits = 3) %>%
    rownames_to_column("groupitem") %>%
    tidyr::separate(groupitem, c("DIFgroup","Item"), sep = "\\.")

  lrt.avg$SE <- lrt.avg.se$SE

  lrt.plot <- lrt.diff %>%
    pivot_longer(any_of(c(groups,"All")),
                 names_to = "DIFgroup",
                 values_to = "Location")
  lrt.plot.se <- lrt.se %>%
    pivot_longer(any_of(c(groups,"All")),
                 names_to = "DIFgroup",
                 values_to = "SE")
  lrt.plot <- full_join(lrt.plot,lrt.plot.se, by = c("Item","Threshold","DIFgroup"))

  ggplot(data = subset(lrt.plot, DIFgroup %in% groups),
         aes(
           x = factor(DIFgroup, levels = groups),
           y = Location,
           group = Threshold,
           color = Threshold
         )) +
    geom_line() +
    geom_point(alpha = 0.9) +
    geom_errorbar(aes(y = Location, ymin = Location - 1.96*SE, ymax = Location + 1.96*SE),
                  width = 0.1
    ) +
    geom_errorbar(data = subset(lrt.plot, DIFgroup == "All"),
                  aes(x = (nr.groups+1)/2+0.15,
                      y = Location,
                      ymin = Location - 1.96*SE,
                      ymax = Location + 1.96*SE),
                  width = 0.1,
                  color = "darkgrey"
    ) +
    geom_point(data = subset(lrt.plot, DIFgroup == "All"),
               aes(x = (nr.groups+1)/2+0.15,
                   y = Location
               ),
               shape = 18,
               color = "black",
               size = 3,
               alpha = 0.6
    ) +
    xlab("DIF group") +
    facet_wrap(~Item) +
    labs(title = "Item threshold locations",
         caption = "Note. Error bars indicate 95% confidence interval.\nDark grey diamonds indicate item location for all participants as one group.") +
    theme(legend.position = "none",
          plot.caption = element_text(hjust = 0, face = "italic"))

}

#' Partial gamma analysis of Differential Item Functioning
#'
#' A simple wrapper for `iarm::partgam_DIF()`. Filters results to only show
#' statistically significant relationships and sorts the table on the absolute
#' value of partial gamma.
#'
#' Conditional highlighting in HTML table output set to partial gamma > +/-0.21.
#'
#' @param data A dataframe with response data
#' @param dif.var A vector with a DIF variable
#' @param output Defaults to a HTML table, optional "quarto" and "dataframe"
#' @export
RIpartgamDIF <- function(data, dif.var, output = "table") {

  if(min(as.matrix(data), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")
  } else if(na.omit(data) %>% nrow() == 0) {
    stop("No complete cases in data.")
  }

  options(rgl.useNULL = TRUE) # temp MacOS fix for iarm dependency vcdExtra->rgl

  sink(nullfile()) # suppress output from the rows below
  ld <- iarm::partgam_DIF(as.data.frame(data), dif.var)
  sink() # disable suppress output

  test <- ld %>%
    bind_rows() %>%
    clean_names() %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    dplyr::filter(str_detect(sig,"\\*"))

  if (nrow(test) == 0) {
    options(rgl.useNULL = FALSE) # temp MacOS fix for iarm dependency vcdExtra->rgl
    return("No statistically significant DIF found.")
  }

  ld2 <- ld %>%
    bind_rows() %>%
    clean_names() %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    dplyr::filter(str_detect(sig,"\\*")) %>%
    arrange(desc(abs(gamma))) %>%
    dplyr::select(!c(pvalue,sig,var)) %>%
    relocate(padj_bh, .after = "upper")

  options(rgl.useNULL = FALSE) # temp MacOS fix for iarm dependency vcdExtra->rgl

  if (output == "table") {
    ld2 %>%
      mutate(gamma = cell_spec(gamma, color = ifelse(abs(gamma) > 0.21, "red", "black"))) %>%
      dplyr::rename(Item = item,
                    `Partial gamma` = gamma,
                    SE = se,
                    `Lower CI` = lower,
                    `Upper CI` = upper,
                    `Adjusted p-value (BH)` = padj_bh) %>%
      kbl_rise()

  } else if (output == "quarto") {
    ld2 %>%
      dplyr::rename(Item = item,
                    `Partial gamma` = gamma,
                    SE = se,
                    `Lower CI` = lower,
                    `Upper CI` = upper,
                    `Adjusted p-value (BH)` = padj_bh) %>%
      knitr::kable()
  } else if (output == "dataframe") {

    return(as.data.frame(ld2))
  }
}

#' Check response distribution prior to DIF analysis
#'
#' Outputs a patchwork plot with one `RItileplot()` for each level of the DIF
#' variable. Note that continuous DIF variables, such as age in years, will not
#' work. A limit has been set at 12 levels of DIF.
#'
#' Plot labels are automatically set based on the DIF variable being a factor
#' with labels. You can change the patchwork object title/etc as usual by adding
#' for instance `+ plot_annotation(title = "Whatever you like")`.
#'
#' @param data Dataframe with item responses
#' @param dif_var DIF variables, ideally a labelled factor
#' @export
#'
RIdifTileplot <- function(data, dif_var) {

  if (is.factor(dif_var) == FALSE) {
    if (n_distinct(dif_var) > 12) {
      stop("More than 12 DIF levels are not allowed")
    } else {
      dif_var <- as.factor(dif_var)
    }
  }

  difplots <- data %>%
    add_column(dif = {{ dif_var }}) %>%
    split(.$dif) %>%
    map(~ RItileplot(.x %>% dplyr::select(!dif)) + labs(title = .x$dif))

  plots <- patchwork::wrap_plots(difplots, axes = "collect", guides = NULL) +
    patchwork::plot_annotation(title = "Tileplots split by DIF variable")
  return(plots)
}
