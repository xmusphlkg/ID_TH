library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

dashboard_plot_theme <- function() {
  theme_minimal(base_family = "Segoe UI") +
    theme(
      plot.title = element_text(family = "Georgia", face = "bold", size = 15, margin = margin(b = 8)),
      plot.subtitle = element_text(size = 10.5, color = "#5A6472", margin = margin(b = 10)),
      plot.caption = element_text(size = 9, color = "#5A6472", margin = margin(t = 10)),
      axis.title = element_text(size = 10, face = "bold", color = "#33424F"),
      axis.text = element_text(size = 9.5, color = "#5A6472"),
      legend.position = "top",
      legend.justification = "left",
      legend.direction = "horizontal",
      legend.title = element_text(size = 9.5, face = "bold"),
      legend.text = element_text(size = 9.5),
      legend.spacing.x = grid::unit(10, "pt"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#D9D4CB", linewidth = 0.35),
      panel.grid.major.y = element_blank(),
      strip.text = element_text(face = "bold", size = 9.5, color = "#33424F"),
      strip.background = element_rect(fill = "#F3EEE6", color = NA),
      plot.margin = margin(8, 10, 8, 10)
    )
}

trajectory_plot <- function(disease, show_interval = FALSE) {
  disease_data <- outcome_lookup[[disease]]$outcome_data
  disease_metrics <- metrics %>% filter(Shortname == disease)

  plot_obj <- ggplot(disease_data, aes(x = date))

  if (isTRUE(show_interval)) {
    plot_obj <- plot_obj +
      geom_ribbon(aes(ymin = lower_95, ymax = upper_95), fill = "#D8E2EC", alpha = 0.65, na.rm = TRUE) +
      geom_ribbon(aes(ymin = lower_80, ymax = upper_80), fill = "#B8CADB", alpha = 0.8, na.rm = TRUE)
  }

  plot_obj +
    geom_line(aes(y = median, color = "Counterfactual median"), linewidth = 1.1) +
    geom_line(aes(y = value, color = "Observed cases"), linewidth = 1.0) +
    geom_vline(xintercept = as.Date("2020-01-01"), linetype = 3, color = "#5A6472") +
    geom_vline(
      data = disease_metrics %>% filter(!is.na(Date_Recovery)),
      aes(xintercept = Date_Recovery),
      linetype = 2,
      color = status_palette[["Recovered"]],
      linewidth = 0.8,
      inherit.aes = FALSE
    ) +
    geom_vline(
      data = disease_metrics %>% filter(!is.na(Date_Balance)),
      aes(xintercept = Date_Balance),
      linetype = 2,
      color = status_palette[["Debt Repaid"]],
      linewidth = 0.8,
      inherit.aes = FALSE
    ) +
    scale_color_manual(values = c("Observed cases" = "#C54A36", "Counterfactual median" = "#0B6E69")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    labs(
      title = disease,
      subtitle = "Observed monthly cases and counterfactual forecast, 2020-2025",
      x = NULL,
      y = "Cases",
      color = NULL,
      caption = if (isTRUE(show_interval)) {
        "Dashed vertical lines indicate January 2020, RP, and BP when available. Shaded bands show 80% and 95% forecast intervals."
      } else {
        "Dashed vertical lines indicate January 2020, RP, and BP when available. Forecast uncertainty bands are intentionally omitted for cleaner comparison."
      }
    ) +
    dashboard_plot_theme() +
    theme(panel.grid.major.y = element_line(color = "#E3DED5", linewidth = 0.35))
}

scatter_plot <- function(group_filter) {
  scatter_data <- metrics %>%
    filter(Status != "No Deficit")

  if (!identical(group_filter, "All")) {
    scatter_data <- scatter_data %>% filter(Group == group_filter)
  }

  ggplot(scatter_data, aes(x = Deficit_Percent, y = Rebound_Intensity, color = Group, shape = Status)) +
    geom_point(size = 3.2, alpha = 0.9) +
    geom_text(aes(label = Shortname), check_overlap = TRUE, nudge_y = 0.03, size = 3.2, show.legend = FALSE) +
    scale_color_manual(values = group_palette, drop = FALSE) +
    scale_shape_manual(values = c("Debt Repaid" = 16, "Recovered" = 17, "Suppressed" = 15)) +
    scale_x_continuous(labels = label_percent(accuracy = 1)) +
    scale_y_continuous(labels = label_number(accuracy = 0.1)) +
    labs(
      title = "Suppression depth vs rebound intensity",
      subtitle = "Diseases with comparable incidence recovery may still differ substantially in cumulative balance.",
      x = "Relative deficit at trough",
      y = "Rebound intensity",
      color = "Disease group",
      shape = "Recovery class"
    ) +
    dashboard_plot_theme() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      panel.grid.major.y = element_line(color = "#E3DED5", linewidth = 0.35)
    )
}

burden_plot <- function(metric_name) {
  total_burden %>%
    arrange(desc(.data[[metric_name]])) %>%
    slice_head(n = 12) %>%
    mutate(Shortname = factor(Shortname, levels = rev(Shortname))) %>%
    ggplot(aes(x = .data[[metric_name]], y = Shortname, fill = Group)) +
    geom_col(width = 0.72, color = "white") +
    scale_fill_manual(values = group_palette, na.value = "#A9B5C4") +
    scale_x_continuous(labels = label_number(big.mark = ",")) +
    labs(
      title = paste("Top diseases by", tolower(metric_name)),
      x = NULL,
      y = NULL,
      fill = "Disease group"
    ) +
    dashboard_plot_theme() +
    theme(
      axis.text.y = element_text(size = 9.5, color = "#41505C"),
      axis.text.x = element_text(size = 9.5, color = "#62707B"),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9.5),
      panel.grid.major.y = element_line(color = "#E3DED5", linewidth = 0.35),
      plot.margin = margin(8, 10, 8, 12)
    )
}

status_group_plot <- metrics %>%
  count(Group, Status, name = "n") %>%
  ggplot(aes(x = Group, y = n, fill = Status)) +
  geom_col(position = "stack", width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = status_palette) +
  labs(
    title = "Recovery classes across transmission groups",
    x = NULL,
    y = "Number of diseases",
    fill = NULL
  ) +
  dashboard_plot_theme() +
  theme(
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.major.y = element_line(color = "#E3DED5", linewidth = 0.35),
    plot.margin = margin(6, 6, 6, 6)
  )

seasonal_value_plot <- function(season_data, disease) {
  ggplot(season_data, aes(x = month, y = value, color = period, group = period)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.6) +
    scale_color_manual(values = c(
      "Observed pre-pandemic" = "#4C6A92",
      "Observed post-PHSM" = "#C54A36",
      "Counterfactual post-PHSM" = "#0B6E69"
    )) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    labs(
      title = paste0(disease, ": monthly mean cases"),
      subtitle = "Raw monthly values used to derive the normalized seasonal profiles",
      x = NULL,
      y = "Average monthly cases",
      color = NULL
    ) +
    dashboard_plot_theme() +
    theme(panel.grid.major.y = element_line(color = "#E3DED5", linewidth = 0.35))
}

all_time_series_plot <- function(ts_data, series_selection, facet_cols = 2) {
  plot_data <- ts_data %>%
    transmute(
      Shortname,
      date,
      `Observed cases` = Observed,
      `Counterfactual median` = Counterfactual
    ) %>%
    pivot_longer(cols = c(`Observed cases`, `Counterfactual median`), names_to = "series", values_to = "value") %>%
    filter(series %in% series_selection)

  ggplot(plot_data, aes(x = date, y = value, color = series)) +
    geom_line(linewidth = 0.8) +
    facet_wrap(vars(Shortname), scales = "free_y", ncol = facet_cols) +
    scale_color_manual(values = c("Observed cases" = "#C54A36", "Counterfactual median" = "#0B6E69")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    labs(
      title = "All selected monthly time series",
      subtitle = "Observed and counterfactual series can be compared across diseases over the full study period",
      x = NULL,
      y = "Cases",
      color = NULL
    ) +
    dashboard_plot_theme() +
    theme(
      panel.grid.major.y = element_line(color = "#E3DED5", linewidth = 0.35),
      panel.spacing = grid::unit(12, "pt")
    )
}

all_time_series_wide_table <- function(ts_data, series_selection) {
  ts_data %>%
    transmute(
      Date = format(date, "%Y-%m"),
      Shortname,
      `Observed cases` = Observed,
      `Counterfactual median` = Counterfactual
    ) %>%
    tidyr::pivot_longer(
      cols = c(`Observed cases`, `Counterfactual median`),
      names_to = "Series",
      values_to = "Value"
    ) %>%
    dplyr::filter(Series %in% series_selection) %>%
    dplyr::mutate(Column = paste(Shortname, Series, sep = " | ")) %>%
    dplyr::select(Date, Column, Value) %>%
    tidyr::pivot_wider(names_from = Column, values_from = Value) %>%
    dplyr::arrange(Date)
}