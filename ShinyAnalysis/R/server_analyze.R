library(shiny)
library(bslib)
library(DT)
library(htmltools)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(forecast)
library(readr)
library(scales)
library(zoo)

#####################################
## Server — Analyze tab
## Handles: CSV upload, validation, counterfactual fitting (ETS/SARIMA),
## RP/BP classification, seasonal profile, result download
#####################################

analyze_server <- function(input, output, session) {

  # ---- Reactive state -------------------------------------------------
  user_data   <- reactiveVal(NULL)   # parsed CSV tibble
  run_result  <- reactiveVal(NULL)   # list: metrics + forecast data + seasonal
  data_origin <- reactiveVal("No dataset loaded")

  activate_dataset <- function(df, label) {
    if (is.null(df) || nrow(df) == 0) {
      showNotification("No valid monthly records were available after parsing.", type = "error", duration = 8)
      return(invisible(FALSE))
    }

    if (nrow(df) < 12) {
      showNotification("Too few rows — need at least 12 months of data.", type = "error", duration = 8)
      return(invisible(FALSE))
    }

    user_data(df)
    run_result(NULL)
    data_origin(label)

    diseases <- sort(unique(df$disease))
    updateSelectInput(session, "analyze_disease", choices = diseases, selected = diseases[1])

    showNotification(
      paste0(
        "Loaded ", label, ": ", nrow(df), " rows, ",
        length(diseases), " disease(s), ",
        format(min(df$date), "%Y-%m"), " – ", format(max(df$date), "%Y-%m")
      ),
      type = "message",
      duration = 6
    )

    invisible(TRUE)
  }

  # ---- 1. File upload & parse ----------------------------------------
  observeEvent(input$user_file, {
    req(input$user_file)
    tryCatch({
      df <- readr::read_csv(input$user_file$datapath,
                            col_types = readr::cols(.default = readr::col_guess()),
                            show_col_types = FALSE)

      # Normalise column names to lowercase
      names(df) <- tolower(trimws(names(df)))

      # Accept "date", "month", "time" as the date column
      date_col <- intersect(names(df), c("date", "month", "time", "yearmonth"))[1]
      case_col <- intersect(names(df), c("cases", "count", "n", "incidence"))[1]
      dis_col  <- intersect(names(df), c("disease", "pathogen", "condition", "name"))[1]

      if (is.na(date_col) || is.na(case_col)) {
        showNotification(
          "CSV must contain a date column (date/month/time) and a cases column (cases/count/n).",
          type = "error", duration = 8
        )
        return()
      }

      if (!is.na(dis_col)) {
        df <- df |> dplyr::rename(disease = !!dis_col)
      } else {
        df <- df |> dplyr::mutate(disease = "Disease")
      }

      df <- df |>
        dplyr::rename(date = !!date_col, cases = !!case_col) |>
        dplyr::mutate(
          date  = parse_surveillance_date(date),
          disease = trimws(as.character(disease)),
          cases = suppressWarnings(as.numeric(cases))
        ) |>
        dplyr::filter(!is.na(date), !is.na(cases), cases >= 0, nzchar(disease)) |>
        dplyr::group_by(disease, date) |>
        dplyr::summarise(cases = sum(cases, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(disease, date)

      if (nrow(df) == 0) {
        showNotification("No valid rows remained after parsing the CSV. Check dates and case counts.", type = "error", duration = 8)
        return()
      }

      activate_dataset(df, "uploaded dataset")
    }, error = function(e) {
      showNotification(paste("Upload error:", conditionMessage(e)), type = "error", duration = 8)
    })
  })

  observeEvent(input$analyze_load_example, {
    activate_dataset(example_upload_data, "Thailand example dataset")
  })

  # ---- 2. Data preview table -----------------------------------------
  output$analyze_data_table <- DT::renderDT({
    req(user_data())
    user_data() |>
      dplyr::mutate(date = as.character(date)) |>
      DT::datatable(
        options = list(pageLength = 8, scrollX = TRUE, dom = "frtip"),
        rownames = FALSE
      )
  })

  # ---- 3. Run analysis -----------------------------------------------
  observeEvent(input$analyze_run, {
    req(user_data(), nchar(input$analyze_disease) > 0)

    withProgress(message = "Fitting counterfactual model…", value = 0, {

      df_disease <- user_data() |>
        dplyr::filter(disease == input$analyze_disease) |>
        dplyr::arrange(date)

      setProgress(0.1)

      # ---- 3a. Validate training length
      cutoff <- as.Date(paste0(input$analyze_cutoff_year, "-01-01"))
      df_train <- df_disease |> dplyr::filter(date < cutoff)
      df_test  <- df_disease |> dplyr::filter(date >= cutoff)

      if (nrow(df_train) < 24) {
        showNotification(
          paste0("Only ", nrow(df_train), " training months before disruption year ",
                 input$analyze_cutoff_year, ". Need at least 24. Adjust disruption year."),
          type = "warning", duration = 8
        )
        return()
      }
      if (nrow(df_test) < 3) {
        showNotification("Too few post-disruption months for analysis. Check disruption year.", type = "warning")
        return()
      }

      # ---- 3b. Build time series
      add_val <- if (isTRUE(input$analyze_sqrt_transform)) 0.01 else 0

      ts_values <- if (add_val > 0) sqrt(df_train$cases + add_val) else df_train$cases
      ts_train <- ts(ts_values,
                     start     = c(year(min(df_train$date)), month(min(df_train$date))),
                     frequency = 12)

      h <- nrow(df_test)
      setProgress(0.25)

      # ---- 3c. Fit and forecast
      fit_model <- function(method) {
        tryCatch({
          mod <- switch(method,
            ETS    = forecast::ets(ts_train, ic = "aicc"),
            SARIMA = forecast::auto.arima(ts_train, seasonal = TRUE, ic = "aicc")
          )
          fc <- forecast::forecast(mod, h = h, level = c(80, 95))

          # Bootstrap 5000 paths for intervals in the refreshed primary workflow.
          n_paths <- 5000
          sim_mat <- replicate(n_paths,
            as.numeric(forecast::simulate(mod, nsim = h, future = TRUE, bootstrap = TRUE)))

          get_q <- function(p) apply(sim_mat, 1, quantile, probs = p, na.rm = TRUE)

          back <- if (add_val > 0) {
            function(x) pmax((x ^ 2) - add_val, 0)
          } else {
            identity
          }

          list(
            method   = method,
            mean     = back(as.numeric(fc$mean)),
            median   = back(get_q(0.50)),
            lower_80 = back(get_q(0.10)),
            upper_80 = back(get_q(0.90)),
            lower_95 = back(get_q(0.025)),
            upper_95 = back(get_q(0.975))
          )
        }, error = function(e) NULL)
      }

      models_to_run <- if (input$analyze_model == "Both") c("ETS", "SARIMA") else input$analyze_model
      fc_list <- lapply(models_to_run, fit_model)
      names(fc_list) <- models_to_run
      fc_list <- Filter(Negate(is.null), fc_list)

      if (length(fc_list) == 0) {
        showNotification("Model fitting failed. Try a different model or check your data.", type = "error")
        return()
      }

      setProgress(0.65)

      # ---- 3d. Build combined outcome tibble (primary: first model)
      primary_fc <- fc_list[[1]]

      outcome_df <- dplyr::bind_rows(
        df_train |> dplyr::transmute(
          date, observed = cases,
          median = NA_real_, lower_95 = NA_real_, upper_95 = NA_real_,
          lower_80 = NA_real_, upper_80 = NA_real_,
          phase = "Training"
        ),
        dplyr::tibble(
          date     = df_test$date,
          observed = df_test$cases,
          median   = primary_fc$median,
          lower_95 = primary_fc$lower_95,
          upper_95 = primary_fc$upper_95,
          lower_80 = primary_fc$lower_80,
          upper_80 = primary_fc$upper_80,
          phase    = "Post-disruption"
        )
      ) |> dplyr::arrange(date)

      # ---- 3e. Compute RP / BP metrics
      rp_thresh    <- input$analyze_recovery_threshold / 100
      rp_persist   <- as.integer(input$analyze_persistence)
      post_df      <- outcome_df |> dplyr::filter(phase == "Post-disruption", !is.na(median))

      D_t          <- post_df$observed - post_df$median
      C_t          <- cumsum(D_t)
      first_deficit_idx <- which(C_t < 0)[1]
      trough_idx   <- if (min(C_t) < 0) which.min(C_t) else NA_integer_

      start_deficit_date <- if (!is.na(first_deficit_idx)) post_df$date[first_deficit_idx] else as.Date(NA)
      trough_date        <- if (!is.na(trough_idx))        post_df$date[trough_idx]        else as.Date(NA)
      max_deficit        <- if (!is.na(trough_idx))        C_t[trough_idx]                 else NA_real_
      cum_expected       <- if (!is.na(trough_idx)) sum(post_df$median[1:trough_idx]) else NA_real_
      relative_deficit   <- if (!is.na(max_deficit) && !is.na(cum_expected) && cum_expected > 0)
                              max_deficit / cum_expected else NA_real_

      # RP
      recovery_date <- as.Date(NA)
      if (!is.na(start_deficit_date) && !is.na(max_deficit) && max_deficit < 0) {
        df_search     <- post_df |> dplyr::filter(date >= start_deficit_date)
        D_search      <- df_search$observed - df_search$median
        C_search      <- cumsum(D_search)
        recovered_vec <- (df_search$observed >= rp_thresh * df_search$median)
        paying_back   <- c(FALSE, diff(C_search) >= 0)
        both_vec      <- recovered_vec & paying_back
        robust <- zoo::rollapply(both_vec, width = rp_persist, FUN = all,
                                 fill = FALSE, align = "left")
        rp_idx <- which(robust)[1]
        if (!is.na(rp_idx)) recovery_date <- df_search$date[rp_idx]
      }

      # BP
      balance_date <- as.Date(NA)
      if (!is.na(trough_date)) {
        df_post_trough <- post_df |> dplyr::filter(date > trough_date)
        if (nrow(df_post_trough) > 0) {
          D_bt <- df_post_trough$observed - df_post_trough$median
          C_bt <- cumsum(D_bt) + max_deficit  # continue from trough
          bp_idx <- which(C_bt >= 0)[1]
          if (!is.na(bp_idx)) balance_date <- df_post_trough$date[bp_idx]
        }
      }

      # Rebound intensity
      rebound_intensity <- if (!is.na(trough_date)) {
        post_df |>
          dplyr::filter(date >= trough_date) |>
          dplyr::summarise(ri = max(observed / (median + 1), na.rm = TRUE)) |>
          dplyr::pull(ri)
      } else NA_real_

      status <- dplyr::case_when(
        !is.na(balance_date)  ~ "Debt Repaid",
        !is.na(recovery_date) ~ "Recovered",
        !is.na(start_deficit_date) ~ "Suppressed",
        TRUE ~ "No Deficit"
      )

      rp_months <- if (!is.na(start_deficit_date) && !is.na(recovery_date))
        as.integer(lubridate::interval(start_deficit_date, recovery_date) %/% months(1))
      else NA_integer_

      bp_months <- if (!is.na(start_deficit_date) && !is.na(balance_date))
        as.integer(lubridate::interval(start_deficit_date, balance_date) %/% months(1))
      else NA_integer_

      setProgress(0.80)

      # ---- 3f. Seasonal profile
      seasonal_df <- dplyr::bind_rows(
        df_disease |>
          dplyr::filter(year(date) <= input$analyze_cutoff_year - 1) |>
          dplyr::mutate(month_num = month(date), period = "Pre-disruption observed") |>
          dplyr::group_by(period, month_num) |>
          dplyr::summarise(value = mean(cases, na.rm = TRUE), .groups = "drop"),
        df_disease |>
          dplyr::filter(year(date) >= input$analyze_cutoff_year + 2) |>
          dplyr::mutate(month_num = month(date), period = "Post-disruption observed") |>
          dplyr::group_by(period, month_num) |>
          dplyr::summarise(value = mean(cases, na.rm = TRUE), .groups = "drop"),
        dplyr::tibble(
          date     = df_test$date,
          median   = primary_fc$median,
          month_num = month(df_test$date)
        ) |>
          dplyr::filter(year(date) >= input$analyze_cutoff_year + 2) |>
          dplyr::group_by(month_num) |>
          dplyr::summarise(value = mean(median, na.rm = TRUE), .groups = "drop") |>
          dplyr::mutate(period = "Post-disruption counterfactual")
      ) |>
        dplyr::group_by(period) |>
        dplyr::mutate(
          norm_value = (value - min(value)) / pmax(max(value) - min(value), 1e-6),
          month_lab  = month.abb[month_num],
          period     = factor(period, levels = c(
            "Pre-disruption observed",
            "Post-disruption observed",
            "Post-disruption counterfactual"
          ))
        ) |>
        dplyr::ungroup()

      # ---- Compile result
      run_result(list(
        disease         = input$analyze_disease,
        outcome_df      = outcome_df,
        fc_list         = fc_list,
        seasonal_df     = seasonal_df,
        status          = status,
        start_deficit   = start_deficit_date,
        trough_date     = trough_date,
        recovery_date   = recovery_date,
        balance_date    = balance_date,
        rp_months       = rp_months,
        bp_months       = bp_months,
        max_deficit     = max_deficit,
        relative_deficit = relative_deficit,
        rebound_intensity = rebound_intensity,
        n_train         = nrow(df_train),
        n_test          = nrow(df_test),
        model_used      = paste(models_to_run, collapse = " + "),
        cutoff          = cutoff,
        rp_thresh       = rp_thresh,
        rp_persist      = rp_persist,
        C_t             = C_t,
        post_dates      = post_df$date
      ))

      setProgress(1.0)
    })
  })

  # ---- 4. KPI boxes ---------------------------------------------------
  status_palette <- c(
    "Debt Repaid" = "#0D5D56",
    "Recovered"   = "#D89A2B",
    "Suppressed"  = "#BE4C3A",
    "No Deficit"  = "#4C6A92"
  )

  output$analyze_status_box <- renderUI({
    r <- run_result()
    if (is.null(r)) return(NULL)
    col <- status_palette[[r$status]]
    value_box(
      class   = "kpi-box",
      title   = "Recovery status",
      value   = r$status,
      tags$p(paste0("Model: ", r$model_used)),
      theme   = value_box_theme(bg = col, fg = "white")
    )
  })

  output$analyze_rp_box <- renderUI({
    r <- run_result()
    if (is.null(r)) return(NULL)
    rp_label <- if (!is.na(r$rp_months)) paste0(r$rp_months, " months") else "Not recovered"
    value_box(
      class   = "kpi-box kpi-amber",
      title   = "Recovery Period (RP)",
      value   = rp_label,
      tags$p(if (!is.na(r$recovery_date)) format(r$recovery_date, "%Y-%m") else "—")
    )
  })

  output$analyze_bp_box <- renderUI({
    r <- run_result()
    if (is.null(r)) return(NULL)
    bp_label <- if (!is.na(r$bp_months)) paste0(r$bp_months, " months") else "Not balanced"
    value_box(
      class   = "kpi-box kpi-emerald",
      title   = "Balance Period (BP)",
      value   = bp_label,
      tags$p(if (!is.na(r$balance_date)) format(r$balance_date, "%Y-%m") else "—")
    )
  })

  output$analyze_training_box <- renderUI({
    r <- run_result()
    if (is.null(r)) return(NULL)
    value_box(
      class = "kpi-box kpi-slate",
      title = "Training months",
      value = r$n_train,
      tags$p(paste0(r$n_test, " post-disruption months analysed"))
    )
  })

  # ---- 5. Trajectory plot --------------------------------------------
  output$analyze_plot_area <- renderUI({
    r <- run_result()
    if (is.null(r)) {
      return(tags$div(
        class = "text-center mt-5",
        style = "color: #62707B; padding: 3rem;",
        icon("chart-line", style = "font-size: 2.5rem; opacity: 0.3;"),
        tags$p(class = "mt-2", "Upload a CSV or load the bundled Thailand example to see the counterfactual trajectory.")
      ))
    }
    plotOutput("analyze_trajectory_plot", height = "480px")
  })

  output$analyze_trajectory_plot <- renderPlot({
    r <- run_result()
    req(r)

    df  <- r$outcome_df
    col_observed      <- "#C54A36"
    col_counterfactual <- "#0B6E69"

    p <- ggplot(df, aes(x = date)) +
      # Training shading
      annotate("rect",
               xmin = min(df$date[df$phase == "Training"]),
               xmax = max(df$date[df$phase == "Training"]),
               ymin = -Inf, ymax = Inf,
               fill = "#F5EFE6", alpha = 0.5) +
      # Deficit shading
      geom_ribbon(
        data = df |> dplyr::filter(phase == "Post-disruption", !is.na(median)),
        aes(ymin = pmin(observed, median), ymax = pmax(observed, median),
            fill = (observed < median)),
        alpha = 0.25, show.legend = FALSE
      ) +
      scale_fill_manual(values = c(`TRUE` = "#C54A36", `FALSE` = "#0B6E69")) +
      # 95% PI
      geom_ribbon(
        data = df |> dplyr::filter(!is.na(lower_95)),
        aes(ymin = lower_95, ymax = upper_95),
        fill = "#B8D4D0", alpha = 0.45
      ) +
      # 80% PI
      geom_ribbon(
        data = df |> dplyr::filter(!is.na(lower_80)),
        aes(ymin = lower_80, ymax = upper_80),
        fill = "#8FC4BF", alpha = 0.55
      ) +
      # Counterfactual line (post-disruption only)
      geom_line(
        data = df |> dplyr::filter(!is.na(median)),
        aes(y = median, colour = "Counterfactual"),
        linewidth = 1.25
      ) +
      # Observed line
      geom_line(aes(y = observed, colour = "Observed"), linewidth = 1.0) +
      # Disruption marker
      geom_vline(xintercept = r$cutoff, linetype = "dashed", colour = "#62707B", linewidth = 0.8) +
      # RP marker
      { if (!is.na(r$recovery_date))
          geom_vline(xintercept = r$recovery_date, linetype = "dashed",
                     colour = "#D89A2B", linewidth = 0.9)
        else geom_blank() } +
      # BP marker
      { if (!is.na(r$balance_date))
          geom_vline(xintercept = r$balance_date, linetype = "dashed",
                     colour = "#0D5D56", linewidth = 0.9)
        else geom_blank() } +
      # Trough marker
      { if (!is.na(r$trough_date))
          annotate("point", x = r$trough_date,
                   y = r$outcome_df$observed[r$outcome_df$date == r$trough_date][1],
                   colour = "#BE4C3A", size = 4, shape = 19)
        else geom_blank() } +
      scale_colour_manual(
        values = c("Observed" = col_observed, "Counterfactual" = col_counterfactual),
        name   = NULL
      ) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
      labs(
        title    = paste0(r$disease, " — Counterfactual trajectory"),
        subtitle = paste0(
          "Model: ", r$model_used,
          "  ·  Disruption: ", format(r$cutoff, "%Y-%m"),
          "  ·  Status: ", r$status,
          if (!is.na(r$rp_months)) paste0("  ·  RP = ", r$rp_months, " m") else "",
          if (!is.na(r$bp_months)) paste0("  ·  BP = ", r$bp_months, " m") else ""
        ),
        x        = NULL,
        y        = "Monthly cases",
        caption  = "Shaded bands: 80% and 95% predictive intervals. Dashed vertical lines: disruption onset, RP (amber), BP (teal). ● = trough."
      ) +
      theme_minimal(base_family = "Segoe UI") +
      theme(
        plot.title           = element_text(face = "bold", size = 13, hjust = 0),
        plot.subtitle        = element_text(size = 10, colour = "#5A6472"),
        plot.caption         = element_text(size = 9, colour = "#5A6472"),
        axis.title           = element_text(size = 10, face = "bold", colour = "#33424F"),
        axis.text            = element_text(size = 9.5, colour = "#5A6472"),
        legend.position      = "top",
        legend.justification = "left",
        panel.grid.minor     = element_blank(),
        panel.grid.major.y   = element_line(colour = "#E3DED5", linewidth = 0.35)
      )

    # If "Both" models requested, overlay second counterfactual
    if (length(r$fc_list) == 2) {
      fc2   <- r$fc_list[[2]]
      df_fc2 <- dplyr::tibble(
        date     = r$outcome_df$date[r$outcome_df$phase == "Post-disruption"],
        median2  = fc2$median
      )
      p <- p +
        geom_line(data = df_fc2, aes(x = date, y = median2),
                  colour = "#C96A43", linetype = "longdash", linewidth = 1.0, inherit.aes = FALSE) +
        annotate("text", x = max(df_fc2$date), y = tail(fc2$median, 1),
                 label = names(r$fc_list)[2], colour = "#C96A43", hjust = 1.1, size = 3.5)
    }

    p
  }, res = 110)

  # ---- 6. Metrics card ------------------------------------------------
  output$analyze_metrics_card <- renderUI({
    r <- run_result()
    if (is.null(r)) {
      return(tags$p(class = "section-copy compact text-center",
                    "Results will appear after running the analysis."))
    }

    fmt_date <- function(d) if (is.na(d)) "—" else format(d, "%Y-%m")
    fmt_num  <- function(x, digits = 1) if (is.na(x)) "—" else formatC(x, format = "f", digits = digits)

    tagList(
      tags$dl(
        class = "definition-list",
        tags$dt("Status"),
        tags$dd(
          tags$span(
            class = paste0("status-badge ", switch(r$status,
              "Debt Repaid" = "badge-success",
              "Recovered"   = "badge-warning",
              "Suppressed"  = "badge-danger",
              "badge-info"
            )),
            r$status
          )
        ),
        tags$dt("Disruption onset"),
        tags$dd(fmt_date(r$start_deficit)),
        tags$dt("Trough"),
        tags$dd(fmt_date(r$trough_date)),
        tags$dt("Recovery Point (RP)"),
        tags$dd(if (!is.na(r$rp_months)) paste0(fmt_date(r$recovery_date), " (", r$rp_months, " m)") else "Not recovered"),
        tags$dt("Balance Point (BP)"),
        tags$dd(if (!is.na(r$bp_months)) paste0(fmt_date(r$balance_date), " (", r$bp_months, " m)") else "Not balanced"),
        tags$dt("Absolute deficit"),
        tags$dd(if (!is.na(r$max_deficit)) scales::comma(abs(round(r$max_deficit))) else "—"),
        tags$dt("Relative deficit"),
        tags$dd(if (!is.na(r$relative_deficit)) scales::percent(abs(r$relative_deficit), accuracy = 0.1) else "—"),
        tags$dt("Rebound intensity"),
        tags$dd(fmt_num(r$rebound_intensity, 2)),
        tags$dt("Model"),
        tags$dd(r$model_used),
        tags$dt("Training months"),
        tags$dd(r$n_train),
        tags$dt("Post-disruption months"),
        tags$dd(r$n_test),
        tags$dt("RP threshold"),
        tags$dd(scales::percent(r$rp_thresh, accuracy = 1)),
        tags$dt("RP persistence"),
        tags$dd(paste0(r$rp_persist, " months"))
      )
    )
  })

  # ---- 7. Seasonal plot -----------------------------------------------
  output$analyze_seasonal_plot <- renderPlot({
    r <- run_result()
    if (is.null(r) || is.null(r$seasonal_df) || nrow(r$seasonal_df) == 0) return(NULL)

    period_colors <- c(
      "Pre-disruption observed"         = "#7E6148FF",
      "Post-disruption observed"        = "#E64B35FF",
      "Post-disruption counterfactual"  = "#91D1C2FF"
    )

    # Close radar polygon
    close_df <- r$seasonal_df |>
      dplyr::group_by(period) |>
      dplyr::group_modify(~ {
        m1 <- dplyr::filter(.x, month_num == 1)
        m13 <- dplyr::mutate(m1, month_num = 13)
        dplyr::bind_rows(.x, m13)
      }) |>
      dplyr::ungroup()

    rainy_ribbon <- dplyr::tibble(month_num = c(5, 10), ymin = 0, ymax = 1)

    ggplot(close_df, aes(x = month_num, y = norm_value, colour = period, fill = period)) +
      # Rainy season background
      annotate("rect", xmin = 5, xmax = 10, ymin = 0, ymax = 1,
               fill = "#B3D7F0", alpha = 0.20) +
      geom_area(position = "identity", alpha = 0.12, linewidth = 0.6) +
      coord_polar(start = 0) +
      scale_x_continuous(breaks = 1:12, labels = month.abb, limits = c(1, 13)) +
      scale_y_continuous(labels = NULL, breaks = NULL) +
      scale_colour_manual(values = period_colors, name = "Period") +
      scale_fill_manual(values   = period_colors, name = "Period") +
      labs(
        title    = paste0(r$disease, " — Seasonal profile"),
        subtitle = "Normalized monthly mean (0–1 scaling within each period)"
      ) +
      theme_minimal(base_family = "Segoe UI") +
      theme(
        axis.title           = element_blank(),
        legend.position      = "bottom",
        panel.grid.major.x   = element_line(colour = "grey80", linetype = "dotted"),
        panel.grid.major.y   = element_line(colour = "grey90", linetype = "dashed"),
        plot.title           = element_text(face = "bold", size = 11, hjust = 0),
        plot.subtitle        = element_text(size = 9, colour = "#5A6472")
      )
  }, res = 110)

  # ---- 8. Diagnostics -------------------------------------------------
  output$analyze_diagnostics <- renderUI({
    r <- run_result()
    df <- user_data()

    notes <- list()

    if (!is.null(df)) {
      n_diseases <- length(unique(df$disease))
      date_range <- paste(format(min(df$date), "%Y-%m"), "–", format(max(df$date), "%Y-%m"))
      notes <- c(notes, list(
        tags$li(paste0("Current source: ", data_origin())),
        tags$li(paste0("Rows loaded: ", nrow(df), " · ", n_diseases, " disease(s) · ", date_range))
      ))
      # Check for zero-only months
      n_zero <- sum(df$cases == 0, na.rm = TRUE)
      if (n_zero > 0) {
        notes <- c(notes, list(tags$li(
          tags$span(class = "status-badge badge-warning", "Note"),
          paste0(" ", n_zero, " zero-case months detected. Square-root transform + 0.01 offset is recommended.")
        )))
      }
      # Check for large gaps
      if (!is.null(r)) {
        if (r$n_train < 36) {
          notes <- c(notes, list(tags$li(
            tags$span(class = "status-badge badge-warning", "Warning"),
            paste0(" Only ", r$n_train, " training months — counterfactual may be unreliable. Recommend ≥ 36.")
          )))
        } else if (r$n_train < 60) {
          notes <- c(notes, list(tags$li(
            tags$span(class = "status-badge badge-info", "Info"),
            paste0(" ", r$n_train, " training months. Results adequate but ≥ 60 recommended for seasonal models.")
          )))
        } else {
          notes <- c(notes, list(tags$li(
            tags$span(class = "status-badge badge-success", "OK"),
            paste0(" ", r$n_train, " training months — sufficient for stable counterfactual.")
          )))
        }
        notes <- c(notes, list(
          tags$li(paste0("Counterfactual model(s): ", r$model_used)),
          tags$li(paste0("RP threshold: ", scales::percent(r$rp_thresh, accuracy = 1),
                          " with ", r$rp_persist, "-month persistence window")),
          tags$li(paste0("Disruption onset configured: ", format(r$cutoff, "%Y-%m")))
        ))
        if (!is.na(r$start_deficit)) {
          notes <- c(notes, list(
            tags$li(paste0("Actual disruption detected: ", format(r$start_deficit, "%Y-%m")))
          ))
        }
      }
    } else {
      notes <- c(notes, list(
        tags$li(tags$span(class = "status-badge badge-info", "Waiting"), " Upload a CSV or load the bundled Thailand example to begin.")
      ))
    }

    tagList(
      tags$p(class = "mini-kicker", "Analysis log"),
      tags$ul(class = "insight-list", notes)
    )
  })

  # ---- 9. Download ----------------------------------------------------
  output$analyze_download <- downloadHandler(
    filename = function() {
      r <- run_result()
      paste0("recovery_", if (!is.null(r)) gsub(" ", "_", r$disease) else "results",
             "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      r <- run_result()
      df <- user_data()
      if (is.null(r) || is.null(df)) {
        write.csv(data.frame(message = "No analysis results yet."), file, row.names = FALSE)
        return()
      }
      result_df <- r$outcome_df |>
        dplyr::mutate(disease = r$disease) |>
        dplyr::select(disease, date, phase, observed, median, lower_80, upper_80, lower_95, upper_95)
      write.csv(result_df, file, row.names = FALSE)
    }
  )

  output$analyze_download_example <- downloadHandler(
    filename = function() {
      paste0("thailand_example_upload_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(example_upload_data, file, row.names = FALSE)
    }
  )

  output$analyze_download_template <- downloadHandler(
    filename = function() {
      paste0("surveillance_template_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      template_df <- data.frame(
        date = c("2019-01", "2019-02", "2019-03", "2020-01", "2020-02", "2020-03"),
        disease = c("Example disease", "Example disease", "Example disease", "Example disease", "Example disease", "Example disease"),
        cases = c(120, 145, 131, 72, 64, 70),
        stringsAsFactors = FALSE
      )

      write.csv(template_df, file, row.names = FALSE)
    }
  )
}
