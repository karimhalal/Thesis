#Visualization
library(ggplot2)
library(dplyr)
library(tidyr)
library(haven)
library(cmprsk)
#nacrs

# Density plots for one or more variables from a dataset.
# vars: character vector of column names; if NULL, uses all numeric columns.
# group: optional grouping column name (string) for overlaid density curves.
# alpha: transparency of filled density curves.
plot_density <- function(data, vars = NULL, stratify = NULL, stratify_labels = NULL,
                         stratify_lab = NULL, xlab = NULL, title = "Density plots") {
  if (is.null(vars)) {
    vars <- names(data)[sapply(data, is.numeric)]
  }

  stopifnot(all(vars %in% names(data)))

  if (!is.null(stratify)) {
    stopifnot(length(stratify) == 1L, stratify %in% names(data))
  }

  if (!is.null(stratify)) {
    col <- as.character(data[[stratify]])
    if (!is.null(stratify_labels)) {
      data[[stratify]] <- factor(col, levels = names(stratify_labels),
                                 labels = unname(stratify_labels))
    } else {
      data[[stratify]] <- haven::as_factor(data[[stratify]])
    }
  }

  long <- data %>%
    select(all_of(c(vars, stratify))) %>%
    tidyr::pivot_longer(cols = all_of(vars), names_to = "variable", values_to = "value") %>%
    filter(!is.na(value))

  p <- ggplot(long, aes(x = value))

  if (!is.null(stratify)) {
    p <- p +
      geom_density(aes(colour = .data[[stratify]]), fill = NA) +
      facet_wrap(~variable, scales = "free") +
      labs(title = title, x = xlab, y = "Density", colour = if (!is.null(stratify_lab)) stratify_lab else stratify)
  } else {
    p <- p +
      geom_density(colour = "steelblue4", fill = NA) +
      facet_wrap(~variable, scales = "free") +
      labs(title = title, x = xlab, y = "Density")
  }

  p + theme_minimal() +
    theme(strip.text = element_text(face = "bold"))
}


  
  nacrs_drugs %>%
  mutate(month = as.Date(format(regdate, "%Y-%m-01"))) %>%
  count(month) %>%
  ggplot(aes(x = month, y = n)) +
  geom_col(fill = "steelblue") +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dotted", colour = "red", linewidth = 0.8) +
  annotate("text", x = as.Date("2020-03-01"), y = Inf, label = "COVID-19 State of Emergency",
           angle = 90, hjust = 1.1, vjust = -0.5, size = 3, colour = "red") +
  scale_x_date(date_labels = "%B %Y", date_breaks = "3 months") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "Number of outcomes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plot cumulative incidence functions from a cmprsk::cuminc() object.
#
# type: "panels"  — separate facet per event, free y-axis (default)
#       "log"     — single panel for event_of_interest on log y-axis
#       "stacked" — stacked ribbon for all events
#
# event_labels: named character vector mapping event codes (as strings) to
#   display labels, e.g. c("1" = "NMS Death", "2" = "Other Death").
#
# group: optional column name used as the grouping variable in cuminc().
#   If supplied, curves are coloured by group level.
#
# y_limits: numeric vector of length 2 (e.g. c(0, 0.05)) to fix the y-axis
#   range. Applied per-panel in "panels" type, or to the single axis in
#   "log" and "stacked" types. NULL (default) lets ggplot choose.
#
# ci: logical, draw 95% pointwise confidence bands (default TRUE).
plot_cif <- function(
    data,
    ftime,
    fstatus,
    group              = NULL,
    event_of_interest  = "1",
    event_labels       = NULL,
    group_labels       = NULL,
    type               = c("panels", "log", "stacked"),
    y_limits           = NULL,
    ci                 = TRUE,
    xlab               = "Time",
    title              = NULL
) {
  type <- match.arg(type)

  grp_vec <- if (!is.null(group)) {
    col <- data[[group]]
    if (inherits(col, "haven_labelled")) haven::as_factor(col)
    else if (is.factor(col)) col
    else as.character(col)
  } else NULL
  cif_obj <- cmprsk::cuminc(ftime = haven::zap_labels(data[[ftime]]),
                             fstatus = haven::zap_labels(data[[fstatus]]),
                             group = grp_vec)

  elem_names <- names(cif_obj)[names(cif_obj) != "Tests"]

  cif_df <- do.call(rbind, lapply(elem_names, function(nm) {
    parts <- strsplit(nm, " ")[[1]]
    event <- tail(parts, 1)
    grp   <- if (length(parts) > 1) paste(head(parts, -1), collapse = " ") else "Overall"
    data.frame(
      time  = cif_obj[[nm]]$time,
      est   = cif_obj[[nm]]$est,
      se    = sqrt(cif_obj[[nm]]$var),
      event = event,
      group = grp,
      stringsAsFactors = FALSE
    )
  }))

  event_codes <- sort(unique(cif_df$event))
  if (is.null(event_labels)) {
    event_labels <- setNames(paste("Event", event_codes), event_codes)
  }
  cif_df$event_label <- dplyr::recode(cif_df$event, !!!event_labels)

  if (!is.null(group_labels)) {
    cif_df$group <- dplyr::recode(cif_df$group, !!!group_labels)
  }

  aes_group <- if (!is.null(group)) "group" else NULL

  y_scale_continuous <- function(log = FALSE) {
    if (log) {
      if (!is.null(y_limits))
        scale_y_log10(limits = y_limits, labels = scales::percent_format(accuracy = 0.01))
      else
        scale_y_log10(labels = scales::percent_format(accuracy = 0.01))
    } else {
      if (!is.null(y_limits))
        scale_y_continuous(limits = y_limits, labels = scales::percent_format(accuracy = 0.1))
      else
        scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))
    }
  }

  if (type == "log") {
    cif_df <- dplyr::filter(cif_df, event == event_of_interest)
    p <- ggplot(cif_df, aes(x = time, y = est,
                             colour = if (!is.null(aes_group)) .data[[aes_group]] else NULL,
                             group  = if (!is.null(aes_group)) .data[[aes_group]] else 1)) +
      geom_step() +
      { if (ci) geom_ribbon(aes(
          ymin  = pmax(est - 1.96 * se, 1e-6),
          ymax  = est + 1.96 * se,
          fill  = if (!is.null(aes_group)) .data[[aes_group]] else NULL
        ), alpha = 0.15, colour = NA) } +
      y_scale_continuous(log = TRUE) +
      labs(x = xlab, y = "CIF (log scale)",
           title = title %||% event_labels[[event_of_interest]],
           colour = group, fill = group)

  } else if (type == "stacked") {
    wide <- cif_df |>
      dplyr::select(time, est, event_label, group) |>
      tidyr::pivot_wider(names_from = event_label, values_from = est,
                         values_fn = mean) |>
      tidyr::fill(dplyr::all_of(unname(event_labels)), .direction = "down")

    event_cols <- intersect(unname(event_labels), colnames(wide))
    wide$bottom <- wide[[event_labels[[event_of_interest]]]]
    wide$top    <- rowSums(wide[, event_cols], na.rm = TRUE)

    p <- ggplot(wide, aes(x = time)) +
      geom_ribbon(aes(ymin = 0, ymax = bottom,
                      fill = event_labels[[event_of_interest]]), alpha = 0.7) +
      geom_ribbon(aes(ymin = bottom, ymax = top,
                      fill = setdiff(event_cols, event_labels[[event_of_interest]])[1]),
                  alpha = 0.7) +
      y_scale_continuous() +
      labs(x = xlab, y = "Cumulative Incidence", fill = NULL, title = title)

  } else {
    p <- ggplot(cif_df, aes(x = time, y = est,
                             colour = if (!is.null(aes_group)) .data[[aes_group]] else NULL,
                             group  = if (!is.null(aes_group)) .data[[aes_group]] else 1)) +
      geom_step() +
      { if (ci) geom_ribbon(aes(
          ymin = pmax(est - 1.96 * se, 0),
          ymax = est + 1.96 * se,
          fill = if (!is.null(aes_group)) .data[[aes_group]] else NULL
        ), alpha = 0.15, colour = NA) } +
      facet_wrap(~ event_label, scales = if (is.null(y_limits)) "free_y" else "fixed") +
      y_scale_continuous() +
      labs(x = xlab, y = "Cumulative Incidence", title = title,
           colour = group, fill = group)
  }

  p + theme_bw() +
    theme(strip.text = element_text(face = "bold"),
          legend.position = if (!is.null(group)) "bottom" else "none")
}


# 3-day de-duplication: for a sorted vector of dates per patient,
# drop any event that falls within 3 days of the previously kept event
dedup_3day <- function(dates) {
  keep <- logical(length(dates))
  keep[1] <- TRUE
  last_kept <- dates[1]
  for (i in seq_along(dates)[-1]) {
    if (!is.na(dates[i]) && as.numeric(dates[i] - last_kept) > 3) {
      keep[i] <- TRUE
      last_kept <- dates[i]
    }
  }
  keep
}

# Helper: dedup, aggregate by month, compute cumulative count
prep_cumulative <- function(data, date_col, label) {
  data %>%
    select(ikn, date = {{ date_col }}) %>%
    filter(!is.na(date)) %>%
    arrange(ikn, date) %>%
    group_by(ikn) %>%
    filter(dedup_3day(date)) %>%
    ungroup() %>%
    mutate(month = as.Date(format(date, "%Y-%m-01"))) %>%
    count(month) %>%
    mutate(cumulative = cumsum(n), source = label)
}

ed_cum    <- prep_cumulative(acrs_drugs,  regdate, "ED Visits")
hosp_cum  <- prep_cumulative(dad_drugs,   admdate, "Hospitalisations")
death_cum <- prep_cumulative(death_drugs, end_fup, "Deaths")

# Combined line: pool all three, deduplicate across datasets per patient
combined_cum <- bind_rows(
  acrs_drugs  %>% select(ikn, date = regdate),
  dad_drugs   %>% select(ikn, date = admdate),
  death_drugs %>% select(ikn, date = end_fup)
) %>%
  filter(!is.na(date)) %>%
  arrange(ikn, date) %>%
  group_by(ikn) %>%
  filter(dedup_3day(date)) %>%
  ungroup() %>%
  mutate(month = as.Date(format(date, "%Y-%m-01"))) %>%
  count(month) %>%
  mutate(cumulative = cumsum(n), source = "All Events")

plot_data <- bind_rows(ed_cum, hosp_cum, death_cum, combined_cum)

ggplot(plot_data, aes(x = month, y = cumulative, colour = source)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dotted", colour = "red", linewidth = 0.8) +
  annotate("text", x = as.Date("2020-03-01"), y = Inf, label = "COVID-19 State of Emergency",
           angle = 90, hjust = 1.1, vjust = -0.5, size = 3, colour = "red") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), labels = scales::comma) +
  scale_colour_manual(values = c(
    "ED Visits"        = "steelblue",
    "Hospitalisations" = "darkorange",
    "Deaths"           = "darkred",
    "All Events"       = "black"
  )) +
  labs(x = NULL, y = "Cumulative outcomes", colour = NULL) +
  theme_minimal() +
  theme(
    axis.text.x    = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )