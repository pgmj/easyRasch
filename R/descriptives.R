#' Creates a figure with item missing data descriptives for items
#'
#' Sample use: `RImissing(df, itemStart = "PSS")`
#'
#' If `itemStart` is missing, the whole dataframe will be used.
#'
#' @param data Dataframe/tibble to create table from
#' @param itemStart What your variable names start with, in quotes
#' @export
RImissing <- function(data, itemStart) {

  if (missing(itemStart)) {
    m <-
      data %>%
      t() %>%
      as.data.frame() %>%
      mutate(Missing = rowSums(is.na(.))) %>%
      dplyr::select(Missing) %>%
      arrange(desc(Missing)) %>%
      rownames_to_column(var = "Item") %>%
      mutate(Percentage = Missing / nrow(data) * 100) %>%
      mutate(Item = factor(Item, levels = rev(Item)))

    if (max(m$Missing) == 0) {
      return("No missing data.")
    }

    m %>%
      ggplot(aes(x = Item, y = Percentage)) +
      geom_col(fill = "#009ca6") +
      geom_text(aes(label = paste0(round(Percentage,1),"%")),
                hjust = 1.5, vjust = 0.5,
                color = "white"
      ) +
      coord_flip() +
      ggtitle("Missing data per item") +
      xlab("Items") +
      ylab("Percentage of responses missing") +
      theme_minimal()

  } else {

    m <-
      data %>%
      dplyr::select(starts_with({{ itemStart }})) %>%
      t() %>%
      as.data.frame() %>%
      mutate(Missing = rowSums(is.na(.))) %>%
      dplyr::select(Missing) %>%
      arrange(desc(Missing)) %>%
      rownames_to_column(var = "Item") %>%
      mutate(Percentage = Missing / nrow(data) * 100) %>%
      mutate(Item = factor(Item, levels = rev(Item)))

    if (max(m$Missing) == 0) {
      return("No missing data.")
    }

    m %>%
      ggplot(aes(x = Item, y = Percentage)) +
      geom_col(fill = "#009ca6") +
      geom_text(aes(label = paste0(round(Percentage,1),"%")),
                hjust = 1.5, vjust = 0.5,
                color = "white"
      ) +
      coord_flip() +
      ggtitle("Missing data per item") +
      xlab("Items") +
      ylab("Percentage of responses missing") +
      theme_minimal()
  }
}

#' Creates a figure with item missing data descriptives for participants
#'
#' The y axis is ordered by most missing responses.
#'
#' Sample use: `RImissingP(data)`
#'
#'
#' @param data Dataframe/tibble to create table from
#' @param output Optional dataframe with participants with missing data
#' @param N Show n participants with most missing data (default = 10)
#' @export
RImissingP <- function(data, output, N = 10) {
  order <- data %>%
    mutate(Missing = rowSums(is.na(.))) %>%
    dplyr::select(Missing) %>%
    rownames_to_column(var = "Participant") %>%
    na.omit() %>%
    dplyr::filter(Missing > 0) %>%
    arrange(desc(Missing)) %>%
    slice(0:N)

  if (nrow(order) < 1) {
    return("No missing data.")
  }
  data %>%
    mutate(Missing = rowSums(is.na(.))) %>%
    dplyr::select(Missing) %>%
    rownames_to_column(var = "Participant") %>%
    na.omit() %>%
    dplyr::filter(Missing > 0) %>%
    arrange(desc(Missing)) %>%
    head(N) %>%
    mutate(Participant = as.factor(Participant)) %>%
    mutate(Participant = fct_relevel(Participant, rev(order$Participant))) %>%

    ggplot(aes(x = Participant, y = Missing)) +
    geom_col(fill = "#009ca6") +
    geom_text(
      aes(label = paste0(round(Missing * 100 / ncol(data), 1), "%")),
      hjust = 1.1,
      vjust = 0.5,
      color = "white"
    ) +
    scale_y_continuous(
      breaks = seq(from = 0, to = max(order$Missing), by = 1),
      labels = scales::number_format(accuracy = 1),
      minor_breaks = NULL
    ) +
    coord_flip() +
    labs(
      title = "Missing data per participant",
      x = "Participant rownumber",
      y = "Number of responses missing",
      caption = paste0("Note. Total number of items is ", ncol(data), ".")
    ) +
    theme_minimal()
}

#' Show items based on itemlabels file
#'
#' Requires a dataframe with two columns, labeled "itemnr" and "item",
#' containing information on the item numbers/labels and item content/description.
#' This dataframe has to be labeled `itemlabels`.
#'
#' Default behavior is to only list items that are in the dataframe.
#' Any items eliminated during analysis process will not be included.
#'
#' If all items in the original dataset are to be shown, use option
#' "all.items = TRUE".
#'
#' @param dfin Dataframe with item data only
#' @param all.items Set to TRUE to list all items in 'itemlabels' df
#' @return A table with items used in dataframe
#' @export
RIlistitems <- function(dfin, all.items = FALSE, ...) {
  if (all.items == FALSE) {

    itemlabels %>%
      dplyr::filter(itemnr %in% names(dfin)) %>%
      kbl_rise(...)

  } else {
    itemlabels %>%
      kbl_rise(...)
  }
}

#' Show items based on itemlabels file, with coloring options
#'
#' Requires a dataframe with two columns, labeled "itemnr" and "item",
#' containing information on the item numbers (qN) and item content.
#' This dataframe has to be labeled "itemlabels".
#'
#' Input a vector of item rows, i.e c(1,3,5) to colorize items 1, 3 and 5.
#' Optionally choose which background color will be used. "Lightblue" is the
#' default. Text will be black, so choose a light color which gives good
#' contrast for readability.
#'
#' @param items vector of row numbers for items to colorize background
#' @param color color of background ("lightblue" is default)
#' @return A table with items used in dataframe
#' @export
RIcolorlistitems <- function(items, color) {
  if (missing(color)) {
    formattable(itemlabels,
                align = c("r", "l"), list(
                  formattable::area(row = items) ~ color_tile("lightblue", "lightpink")
                ),
                table.attr = 'class=\"table table-striped\" style="font-size: 15px; font-family: Lato"'
    )
  } else {
    formattable(itemlabels,
                align = c("r", "l"), list(
                  formattable::area(row = items) ~ color_tile(color, "lightpink")
                ),
                table.attr = 'class=\"table table-striped\" style="font-size: 15px; font-family: Lato"'
    )
  }
}

#' Create a table with the items used in a dataframe
#'
#' Depends on the `itemlabels` object, see package README.
#'
#' Intended for use with Quarto chunk option `column: margin`
#'
#' @param dfin Dataframe with item data only
#' @param fontsize Defaults to 11, optimize if desired
#' @export
RIlistItemsMargin <- function(dfin, fontsize = 11) {
  itemlabels %>%
    dplyr::filter(itemnr %in% names(dfin)) %>%
    formattable(align = c(
      "c",
      "l"
    ), list(itemnr = formatter("span", style = ~ formattable::style(
      color = "grey",
      font.weight = "bold"
    ))), table.attr = glue::glue("class=\"table table-striped\" style=\"font-size: {fontsize}px; font-family: Lato\""))
}

#' Create a Guttman-like tileplot/heatmap
#'
#' Sorts items (y axis) and persons (x axis) according to respective
#' total score and displays them with a color gradient based on responses
#'
#' @param dfin Dataframe with item data only
#' @export
RIheatmap <- function(dfin) {

  # extract vectors with person/item id arranged by order of total score
  person.order <- dfin %>%
    mutate(persontotal = rowSums(., na.rm = TRUE)) %>%
    rownames_to_column("PersonID") %>%
    dplyr::select(PersonID, persontotal) %>%
    arrange(persontotal) %>%
    pull(PersonID)
  item.order <- dfin %>%
    t() %>%
    as.data.frame() %>%
    mutate(itemtotal = rowSums(., na.rm = TRUE)) %>%
    rownames_to_column("ItemID") %>%
    dplyr::select(ItemID, itemtotal) %>%
    arrange(itemtotal) %>%
    pull(ItemID)

  # use order vectors to sort item responses and make tile plot
  dfin %>%
    rownames_to_column("PersonID") %>%
    mutate(PersonID = factor(PersonID, levels = person.order)) %>%
    pivot_longer(where(is.numeric)) %>%
    dplyr::rename(Item = name) %>%
    mutate(Item = factor(Item, levels = item.order)) %>%
    ggplot(aes(x = PersonID, y = Item, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "#fff7dd", high = "#009ca6") +
    # scale_x_discrete(guide = guide_axis(n.dodge = 2))
    theme(axis.text.x = element_text(angle = 90))
}

#' Create table for demographic variables
#'
#' Input should be a vector with a demographic variable such as gender or age,
#' and the desired label, enclosed in double quotes.
#'
#' Sample use: RIdemographics(dif.gender, "Gender", width = 40)
#'
#' @param dif.var A vector with a demographic variable
#' @param diflabel What the variable represents (sex/age/etc), in quotes
#' @param ... Options for table, see `kbl_rise()`
#' @export
RIdemographics <- function(dif.var, diflabel, ...) {
  dif.var %>%
    table() %>%
    as_tibble() %>%
    mutate("Percent" = (round((100 * n / sum(n)), 1))) %>%
    dplyr::rename(!!quo_name(diflabel) := ".") %>%
    kbl_rise(...)
}

#' Create tile plot for all items, also showing the count of
#' responses in each response category for each item
#'
#' @param data Dataframe with item data only
#' @param cutoff Conditional highlighting of text in cells with n below cutoff
#' @param highlight Defaults to TRUE. Set to FALSE to disable text highlighting
#' @param percent Set to TRUE to replace n with percentage of item responses
#' @export
RItileplot <- function(data, cutoff = 10, highlight = TRUE, percent = FALSE, text_color = "orange") {

  tileplot <-
    data %>%
    pivot_longer(everything()) %>%
    dplyr::count(name, value) %>%
    mutate(name = factor(name, levels = rev(names(data)))) %>%
    group_by(name) %>%
    mutate(percentage = round(n/sum(n)*100,1)) %>%
    ungroup() %>%

    ggplot(aes(x = value, y = name, fill = n)) +
    geom_tile() +
    scale_fill_viridis_c(expression(italic(n)), limits = c(0, NA)) +
    scale_x_continuous("Response category", expand = c(0, 0), breaks = 0:max(data, na.rm = T)) + # change breaks to fit number of response categories
    labs(y = "Items") +
    theme(axis.text.x = element_text(size = 8))

  if(highlight == TRUE & percent == FALSE) {
    tileplot +
      geom_text(aes(label = n,
                    color = ifelse(n < cutoff,"red",text_color))) +
      guides(color = "none") +
      scale_color_identity()

  } else if(highlight == FALSE & percent == FALSE) {

    tileplot +
      geom_text(aes(label = n), color = text_color)

  } else if(highlight == TRUE & percent == TRUE) {

    tileplot +
      geom_text(aes(label = paste0(percentage,"%"),
                    color = ifelse(n < cutoff,"red",text_color))) +
      guides(color = "none") +
      scale_color_identity()

  } else if(highlight == FALSE & percent == TRUE) {

    tileplot +
      geom_text(aes(label = paste0(percentage,"%")), color = text_color)

  }
}

#' Create a stacked bar graph to show response distribution
#'
#' @param dfin Dataframe with item data only
#' @param omit.na Remove respondents with missing data (or not)
#' @export
RIbarstack <- function(dfin, omit.na = T) {
  if (omit.na) {
    dfin %>%
      na.omit() %>%
      pivot_longer(everything()) %>%
      dplyr::count(name, value) %>%
      mutate(Item = factor(name, levels = rev(names(dfin))),
             value = factor(value)) %>%
      mutate(value = forcats::fct_rev(value)) %>%
      ggplot(aes(x = n, y = Item, fill = value)) +
      geom_col() +
      scale_fill_viridis_d(direction = -1) +
      labs(title = "Item responses",
           x = "Number of responses",
           fill = "Category")
  } else {
    dfin %>%
      pivot_longer(everything()) %>%
      dplyr::count(name, value) %>%
      mutate(Item = factor(name, levels = rev(names(dfin))),
             value = factor(value)) %>%
      mutate(value = forcats::fct_rev(value)) %>%
      ggplot(aes(x = n, y = Item, fill = value)) +
      geom_col() +
      scale_fill_viridis_d(direction = -1) +
      labs(title = "Item responses",
           x = "Number of responses",
           fill = "Category")
  }
}

#' Create a stacked diverging bar graph to show response distribution
#'
#' This function automatically removes respondents with missing data.
#'
#' @param dfin Dataframe with item data only
#' @export
RIbardiv <- function(dfin) {
  dfin %>%
    na.omit() %>%
    pivot_longer(everything()) %>%
    dplyr::rename(
      Item = name,
      Response = value
    ) %>%
    dplyr::count(Item, Response) %>%
    group_by(Item) %>%
    mutate(Percent = (100 * n / sum(n)) %>% round(digits = 1)) %>%
    pivot_wider(id_cols = Item, names_from = Response, values_from = Percent) %>%
    dplyr::relocate("0", .after = Item) %>%
    likert(
      horizontal = TRUE, aspect = 1.5,
      main = "Distribution of responses",
      auto.key = list(
        space = "right", columns = 1,
        reverse = FALSE, padding.text = 2
      )
    )
}

#' Create individual bar plots for all items.
#'
#' @param dfin Dataframe with item data only
#' @export
RIbarplot <- function(dfin) {
  for (i in 1:ncol(dfin)) {
    barplot(table(dfin[, i]),
            col = "#8dc8c7",
            ylab = "Number of responses",
            xlab = "Response category"
    )
    # set item description as subtitle
    mtext(text = itemlabels %>%
            dplyr::filter(itemnr %in% names(dfin))
          %>% .[i, 2],
          side = 3,
          line = 0.4)
    # add itemnr as title
    mtext(text = itemlabels %>%
            dplyr::filter(itemnr %in% names(dfin)) %>%
            .[i, 1],
          side = 3,
          line = 1.5,
          font = 2)
  }
}


#' Create table with summarized responses across all items.
#'
#' @param dfin Dataframe with item data only
#' @param pdf.out Set to TRUE to get PDF-compatible table (kableExtra)
#' @param fontsize Set font size for PDF-compatible table
#' @export
RIallresp <- function(dfin, pdf.out, fontsize = 15) {
  if (missing(pdf.out)) {
    dfin %>%
      pivot_longer(everything()) %>%
      dplyr::count(value) %>%
      mutate(percent = (100 * n / sum(n)) %>% round(digits = 1)) %>%
      dplyr::rename(
        "Response category" = "value",
        "Number of responses" = "n",
        "Percent" = "percent"
      ) %>%
      formattable(
        list(
          `Response category` = formatter("span", style = ~ formattable::style(font.weight = "bold"))
        ),
        table.attr =
          'class=\"table table-striped\" style="font-size: 15px;
                  font-family: Lato; width: 50%"'
      )
  } else {
    dfin %>%
      pivot_longer(everything()) %>%
      dplyr::count(value) %>%
      mutate(percent = (100 * n / sum(n)) %>% round(digits = 1)) %>%
      dplyr::rename(
        "Response category" = "value",
        "Number of responses" = "n",
        "Percent" = "percent"
      ) %>%
      kbl(booktabs = T, escape = F, table.attr = "data-quarto-disable-processing='true' style='width:40%;'") %>%
      # options for HTML output
      kable_styling(
        bootstrap_options = c("striped", "hover"),
        position = "left",
        full_width = F,
        font_size = fontsize,
        fixed_thead = T
      ) %>%
      column_spec(1, bold = T) %>%
      kable_classic(html_font = "Lato") %>%
      # latex_options are for PDF output
      kable_styling(latex_options = c("striped", "scale_down"))
  }
}

#' Floor/ceiling effects based on raw data (ordinal scores). Needs at least one
#' data point in each response category to produce correct footnote text.
#'
#' @param dfin Dataframe with item data only
#' @return A barplot with descriptives in footnote
#' @export
RIrawdist <- function(dfin) {
  df.erm <- PCM(dfin) # run PCM model
  # get info on thresholds
  item.estimates <- eRm::thresholds(df.erm)
  item_difficulty <- item.estimates[["threshtable"]][["1"]]
  item_difficulty <- as.data.frame(item_difficulty)

  # all items should have lowest category 0, making 0 the lowest total score
  rawMin <- 0

  # get the number of thresholds above 0, to calculate max total raw score
  rawMax <- item_difficulty %>%
    dplyr::select(starts_with("Threshold")) %>%
    pivot_longer(everything()) %>%
    na.omit() %>%
    dplyr::count() %>%
    pull()

  # what is the lowest score in the sample?
  rawMinX <- dfin %>%
    mutate(rowsums = rowSums(.,na.rm = T)) %>%
    dplyr::count(rowsums) %>%
    arrange(rowsums) %>%
    head(1) %>%
    pull(rowsums)

  # if lowest participant score is higher than 0, we have no floor effect
  if (rawMinX > 0) {
    rawMinN <- 0
  } else { # if lowest participant score is 0, how many participants have scored 0?
    rawMinN <- dfin %>%
      mutate(rowsums = rowSums(.,na.rm = T)) %>%
      dplyr::count(rowsums) %>%
      arrange(rowsums) %>%
      head(1) %>%
      pull(n)
  }

  # what is the highest score in the sample?
  rawMaxX <- dfin %>%
    mutate(rowsums = rowSums(.,na.rm = T)) %>%
    dplyr::count(rowsums) %>%
    arrange(desc(rowsums)) %>%
    head(1) %>%
    pull(rowsums)

  # if highest score is below max rawscore, we have no ceiling effect
  if (rawMaxX < rawMax) {
    rawMaxN <- 0
  } else {
    rawMaxN <- dfin %>%
      mutate(rowsums = rowSums(.,na.rm = T)) %>%
      dplyr::count(rowsums) %>%
      arrange(desc(rowsums)) %>%
      head(1) %>%
      pull(n)
  }
  # ceiling effect
  ceiling_eff<-round(rawMaxN/nrow(dfin)*100,2)
  # floor effect
  floor_eff<-round(rawMinN/nrow(dfin)*100,2)

  # create barplot to show sum score distribution
  dfin %>%
    mutate('Raw sum score' = rowSums(.,na.rm = T)) %>%
    pull() %>%
    table() %>%
    barplot(main = "Distribution of summed ordinal raw scores",
            ylab = "Number of participants",
            sub = paste0("Min score: ", floor_eff, "% , max score: ", ceiling_eff, "%."),
            xlim = c(0, rawMax),
            space = 0,
            col = "#009ca6")

}

#' Item response distribution figure
#'
#' A figure showing each item's response distribution and n + % of responses in each category.
#' If you use Quarto, you may need to increase the code chunk setting for `fig-height` to
#' something above 5 (default).
#'
#' Works best with the `itemlabels` object set up as described in the package README:
#' <https://pgmj.github.io/easyRasch/#using-the-package>
#'
#' @param data Dataframe/tibble with only item response data coded as integers
#' @param ncols How many columns to display, defaults to 1
#' @param labelwrap Number of characters to show on each row in item description
#' @param text_ypos Position on y axis for the text on each column with n responses
#' @param viridis_end If you need to adapt the coloring of text on columns (0.9 is default)
#' @param font Choose font family
#' @export

RIitemcols <- function(data, ncols = 1, labelwrap = 25, text_ypos = 6, viridis_end = 0.9, font = "sans") {

  # create temporary itemlabels object if none is found
  if (is.data.frame(itemlabels) != TRUE) {
    itemlabels <- data.frame(itemnr = names(data),
                             item = names(data))
  }

  itemlabels <- itemlabels %>%
    dplyr::filter(itemnr %in% names(data)) %>%
    mutate(it = paste0(itemnr," - ",item)) %>%
    mutate(labs = paste0("<b>",itemnr,"</b>"," - ",item))

  plot_data <- data %>%
    pivot_longer(everything()) %>%
    dplyr::count(name, value) %>%
    mutate(name = factor(name, levels = names(data)),
           value = factor(value)) %>%
    group_by(name) %>%
    mutate(percent = round(n/sum(n)*100,1)) %>%
    ungroup() %>%
    dplyr::rename(itemnr = name) %>%
    left_join(itemlabels, by = "itemnr") %>%  # this changes itemnr from factor to chr
    mutate(itemnr = factor(itemnr, levels = itemlabels$itemnr),
           item = factor(item, levels = itemlabels$item)) %>%
    mutate(it = paste0(itemnr," - ",item)) %>%
    mutate(it = factor(it, levels = itemlabels$it))
  #labels = itemlabels$labs))

  plot_data %>%
    ggplot(aes(x = value, y = percent)) +
    geom_col(aes(fill = value)) +
    geom_text(aes(label = paste0("n = ",n),
                  color = value),
              y = text_ypos) +
    facet_wrap(~it,
               ncol = ncols,
               strip.position = "left",
               labeller = labeller(it = label_wrap_gen(labelwrap))) +
    scale_fill_viridis_d(guide = "none") +
    scale_y_continuous(position = "right") +
    scale_color_viridis_d(guide = "none", direction = -1, option = "A", end = viridis_end) +
    labs(x = "Response category", y = "% of responses") +
    guides(color = "none") +
    theme_minimal() +
    theme(legend.position = "top",
          strip.text.y.left = element_text(angle = 0))

}
