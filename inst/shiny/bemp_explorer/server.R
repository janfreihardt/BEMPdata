server <- function(input, output, session) {

  # ── Tab 1: Codebook Browser ────────────────────────────────────────────────

  # Reset filters
  observeEvent(input$cb_reset, {
    updateTextInput(session, "cb_search", value = "")
    updateSelectizeInput(session, "cb_waves",  selected = wo$wave)
    updateSelectizeInput(session, "cb_blocks", selected = character(0))
  })

  cb_filtered <- reactive({
    df <- cb

    # Wave filter
    if (length(input$cb_waves) > 0)
      df <- df[df$wave %in% tolower(input$cb_waves), ]

    # Block filter
    if (length(input$cb_blocks) > 0)
      df <- df[!is.na(df$block) & df$block %in% input$cb_blocks, ]

    # Keyword search across name, label, question
    q <- trimws(input$cb_search)
    if (nzchar(q)) {
      hits <- Reduce(`|`, lapply(
        c("variable_name", "variable_label", "question"),
        function(col) grepl(q, df[[col]], ignore.case = TRUE, perl = TRUE)
      ))
      df <- df[hits, ]
    }

    # Sort by chronological wave order (N before M within each round)
    df[order(match(df$wave, wave_sort_order)), ]
  })

  output$cb_export <- downloadHandler(
    filename = function() paste0("bemp_codebook_subset_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(cb_filtered(), file, row.names = FALSE)
  )

  output$cb_n_results <- renderUI({
    n <- nrow(cb_filtered())
    tags$p(class = "text-muted small",
           sprintf("%s variable%s found", format(n, big.mark = ","),
                   if (n == 1) "" else "s"))
  })

  output$cb_table <- renderDT({
    df <- cb_filtered()
    q  <- trimws(input$cb_search)
    wave_disp <- setNames(wo$wave, tolower(wo$wave))
    df$wave <- wave_disp[df$wave]

    use_pagination <- nrow(df) > 1000
    dt_options <- if (use_pagination) {
      list(
        paging     = TRUE,
        pageLength = 25,
        scrollX    = TRUE,
        dom        = "rtip"
      )
    } else {
      list(
        paging         = FALSE,
        scrollY        = "500px",
        scrollCollapse = TRUE,
        scrollX        = TRUE,
        dom            = "rti"
      )
    }

    display <- df |>
      select(wave, variable_name, variable_label, item_text, block, question, question_type)

    # Apply search-term highlighting to text columns (escape others)
    display$wave           <- htmltools::htmlEscape(display$wave)
    display$variable_name  <- hl(display$variable_name, q)
    display$variable_label <- hl(display$variable_label, q)
    display$item_text      <- hl(display$item_text, q)
    display$block          <- htmltools::htmlEscape(as.character(display$block))
    display$question       <- hl(display$question, q)
    display$question_type  <- htmltools::htmlEscape(as.character(display$question_type))

    datatable(
      display,
      selection  = "single",
      rownames   = FALSE,
      filter     = "top",
      style      = "bootstrap5",
      escape     = FALSE,
      colnames   = c("Wave", "Variable", "Label", "Item Text", "Block", "Question", "Type"),
      options    = dt_options
    )
  }, server = TRUE)

  # Copy button above the table (always visible, no scroll issues)
  output$cb_copy_ui <- renderUI({
    sel <- input$cb_table_rows_selected
    if (is.null(sel) || length(sel) == 0) return(NULL)
    var_name <- cb_filtered()$variable_name[[sel]]
    div(class = "d-flex align-items-center gap-2",
      tags$code(var_name),
      tags$button(
        class   = "btn btn-outline-secondary btn-sm",
        style   = "font-size:0.75rem;padding:1px 8px;",
        onclick = sprintf("copyToClip('%s',this)",
                          gsub("'", "\\'", var_name, fixed = TRUE)),
        "Copy"
      )
    )
  })

  # Detail panel when a row is selected
  output$cb_detail <- renderUI({
    sel <- input$cb_table_rows_selected
    if (is.null(sel) || length(sel) == 0) return(NULL)

    row      <- cb_filtered()[sel, ]
    wave_id  <- row$wave
    var_name <- row$variable_name
    vl       <- get_value_labels(wave_id, var_name)

    tagList(
      hr(),
      tags$table(class = "table table-sm table-borderless",
        tags$tbody(
          tags$tr(tags$th("Wave"),          tags$td(row$wave)),
          tags$tr(tags$th("Block"),         tags$td(row$block)),
          tags$tr(tags$th("Question type"), tags$td(row$question_type)),
          tags$tr(tags$th("Question"),      tags$td(row$question_text)),
          if (!is.na(row$question_text_bn) && nzchar(row$question_text_bn))
            tags$tr(tags$th("Question (BN)"), tags$td(row$question_text_bn)),
          tags$tr(tags$th("Appears in"),
                  tags$td(tags$small(class = "text-muted", row$appears_in_waves)))
        )
      ),
      if (!is.null(vl) && nrow(vl) > 0) {
        tagList(
          tags$strong("Value labels"),
          tags$table(class = "table table-sm table-striped w-auto",
            tags$thead(tags$tr(tags$th("Code"), tags$th("Label"))),
            tags$tbody(
              lapply(seq_len(nrow(vl)), function(i)
                tags$tr(tags$td(vl$code[i]), tags$td(vl$label[i]))
              )
            )
          )
        )
      },

      # Cross-wave wording comparison
      {
        appears <- row$appears_in_waves
        if (!is.na(appears) && nzchar(appears)) {
          # appears_in_waves format: "w1_q7 (n = 2012), w2_q67 (n = 1543), ..."
          cross_vars <- trimws(sub(" \\(.*", "", strsplit(appears, ",\\s*")[[1]]))
          if (length(cross_vars) > 1) {
            # Build rows in appears_in_waves order (one lookup per variable)
            same_var <- dplyr::bind_rows(lapply(cross_vars, function(v) {
              r <- cb[cb$variable_name == v,
                      c("wave", "variable_name", "variable_label", "question_text"),
                      drop = FALSE]
              if (nrow(r) > 0) r[1L, ] else NULL
            }))
            # Lookup for correct-case wave labels (w6_M not w6_m)
            wave_disp <- setNames(wo$wave, tolower(wo$wave))
            tagList(
              hr(),
              tags$strong("Cross-wave wording"),
              tags$table(class = "table table-sm table-striped mt-2",
                tags$thead(tags$tr(tags$th("Wave"), tags$th("Variable"),
                                   tags$th("Label"), tags$th("Question"))),
                tags$tbody(
                  lapply(seq_len(nrow(same_var)), function(i) {
                    wv <- same_var[["wave"]][i]
                    tags$tr(
                      tags$td(tags$code(wave_disp[wv])),
                      tags$td(tags$code(same_var[["variable_name"]][i])),
                      tags$td(same_var[["variable_label"]][i]),
                      tags$td(tags$small(same_var[["question_text"]][i]))
                    )
                  })
                )
              )
            )
          }
        }
      }
    )
  })


  # ── Tab 2: Variable Inspector ──────────────────────────────────────────────

  # Pre-select current wave when compare mode is enabled
  observeEvent(input$vi_wave_compare, {
    if (isTRUE(input$vi_wave_compare)) {
      updateSelectizeInput(session, "vi_waves_compare", selected = input$vi_wave)
    }
  }, ignoreInit = TRUE)

  # Load wave data reactively (cached on disk by get_wave())
  vi_data    <- reactiveVal(NULL)
  vi_url_var <- reactiveVal(NULL)  # variable to pre-select from bookmark restore, cleared on wave change

  observeEvent(input$vi_wave, vi_url_var(NULL), ignoreInit = TRUE)

  observeEvent(input$vi_wave, {
    vi_data(NULL)
    withProgress(message = paste("Loading", input$vi_wave, "..."), value = 0.5, {
      df <- tryCatch(get_wave(input$vi_wave), error = function(e) NULL)
    })
    vi_data(df)

    # Populate Respondent Type choices from the resp_type column in this wave
    rt_var <- get_resp_type_var(input$vi_wave)
    if (!is.null(rt_var) && !is.null(df) && rt_var %in% names(df)) {
      rt_codes <- sort(unique(df[[rt_var]]))
      rt_codes <- rt_codes[!is.na(rt_codes)]
      vl <- get_value_labels(input$vi_wave, rt_var)
      if (!is.null(vl) && nrow(vl) > 0) {
        rt_labels <- vl$label[match(rt_codes, vl$code)]
        rt_labels[is.na(rt_labels)] <- as.character(rt_codes[is.na(rt_labels)])
      } else {
        rt_labels <- as.character(rt_codes)
      }
      rt_choices <- setNames(as.character(rt_codes), rt_labels)
      updateSelectInput(session, "vi_respondent_type",
                        choices  = c("All" = "All", rt_choices),
                        selected = "All")
    } else {
      updateSelectInput(session, "vi_respondent_type",
                        choices  = "All",
                        selected = "All")
    }

    # Clear variable table selection when wave changes
    dataTableProxy("vi_var_table") |> selectRows(NULL)
  })

  # Filtered data: subset by respondent type when selected
  vi_filtered_data <- reactive({
    df <- vi_data()
    req(df)
    if (is.null(input$vi_respondent_type) || input$vi_respondent_type == "All") {
      return(df)
    }
    rt_var <- get_resp_type_var(input$vi_wave)
    if (is.null(rt_var) || !rt_var %in% names(df)) return(df)
    rt_code <- suppressWarnings(as.integer(input$vi_respondent_type))
    df[!is.na(df[[rt_var]]) & df[[rt_var]] == rt_code, ]
  })

  # Resolve per-wave variable names for the selected variable via appears_in_waves
  vi_compare_vars <- reactive({
    req(vi_var(), input$vi_waves_compare, length(input$vi_waves_compare) > 0)
    row <- get_cb_row(input$vi_wave, vi_var())
    if (nrow(row) == 0) return(NULL)

    appears <- row$appears_in_waves[1]
    if (is.na(appears) || !nzchar(appears)) {
      # Fallback: only use primary wave if it is selected
      primary <- tolower(input$vi_wave)
      if (primary %in% tolower(input$vi_waves_compare))
        return(setNames(list(vi_var()), primary))
      return(NULL)
    }

    # Parse "w1_q7 (n = 2012), w2_q67 (n = 1543), ..."
    parts    <- trimws(strsplit(appears, ",\\s*")[[1]])
    varnames <- sub("\\s*\\(.*\\)$", "", parts)

    # Look up each variable's wave in the codebook
    result <- list()
    for (vname in varnames) {
      cb_row <- cb[cb$variable_name == vname, , drop = FALSE]
      if (nrow(cb_row) == 0) next
      wave_id <- cb_row$wave[1]
      if (wave_id %in% tolower(input$vi_waves_compare))
        result[[wave_id]] <- vname
    }
    if (length(result) == 0) NULL else result
  })

  # Load and row-bind data for all selected comparison waves
  vi_compare_data <- reactive({
    req(isTRUE(input$vi_wave_compare))
    vars_map <- vi_compare_vars()
    req(vars_map, length(vars_map) >= 1)

    # Sort waves chronologically
    ordered_waves <- intersect(wave_sort_order, names(vars_map))

    result_list <- list()
    withProgress(message = "Loading waves for comparison...", value = 0, {
      for (i in seq_along(ordered_waves)) {
        wave_id  <- ordered_waves[i]
        var_name <- vars_map[[wave_id]]
        incProgress(1 / length(ordered_waves), detail = wave_id)
        df <- tryCatch(get_wave(wave_id), error = function(e) NULL)
        if (is.null(df) || !var_name %in% names(df)) next
        result_list[[wave_id]] <- data.frame(
          value = df[[var_name]],
          wave  = wave_id,
          stringsAsFactors = FALSE
        )
      }
    })

    if (length(result_list) == 0) return(NULL)
    do.call(rbind, result_list)
  })

  # Variable table for current wave
  vi_var_tbl <- reactive({
    req(input$vi_wave)
    df_data <- vi_data()
    wave_cb <- cb[cb$wave == tolower(input$vi_wave),
                  c("question", "variable_name", "variable_label", "item_text", "block", "question_type"),
                  drop = FALSE]
    if (!is.null(df_data)) {
      wave_cb <- wave_cb[wave_cb$variable_name %in% names(df_data), ]
    }
    rownames(wave_cb) <- NULL  # ensure sequential 1-N indices for DT row selection
    as.data.frame(wave_cb)     # drop tibble class so [sel, "col"] returns a scalar
  })

  output$vi_var_table <- renderDT({
    tbl     <- vi_var_tbl()
    pending <- isolate(vi_url_var())
    sel     <- if (!is.null(pending)) {
      idx <- which(tbl$variable_name == pending)
      if (length(idx) > 0) idx[1] else NULL
    } else NULL
    datatable(
      tbl,
      selection = list(mode = "single", selected = sel),
      rownames  = FALSE,
      filter    = "top",
      style     = "bootstrap5",
      colnames  = c("Question", "Variable", "Label", "Item Text", "Block", "Type"),
      options   = list(
        paging         = FALSE,
        scrollY        = "400px",
        scrollCollapse = TRUE,
        scrollX        = TRUE,
        dom            = "frti"
      )
    )
  }, server = TRUE)

  # Selected variable name (derived from table row click)
  vi_var <- reactive({
    sel <- input$vi_var_table_rows_selected
    if (is.null(sel) || length(sel) == 0) return(NULL)
    vi_var_tbl()$variable_name[[sel]]
  })

  output$vi_group_ui <- renderUI({
    if (isTRUE(input$vi_wave_compare)) return(NULL)
    req(input$vi_wave)
    rt_var <- get_resp_type_var(input$vi_wave)
    if (is.null(rt_var)) return(NULL)
    tagList(
      hr(),
      checkboxInput("vi_group_by_resp_type", "Group by Respondent Type", value = FALSE),
      uiOutput("vi_rt_filter_ui")
    )
  })

  output$vi_rt_filter_ui <- renderUI({
    if (!isTRUE(input$vi_group_by_resp_type)) return(NULL)
    div(style = "margin-left: 18px; margin-top: 2px;",
      checkboxGroupInput(
        "vi_resp_types", NULL,
        choices  = unname(RESP_TYPE_LABELS),
        selected = unname(RESP_TYPE_LABELS)
      )
    )
  })

  output$vi_title <- renderUI({
    req(vi_var())
    row <- get_cb_row(input$vi_wave, vi_var())
    lab <- if (nrow(row) > 0) row$variable_label[1] else vi_var()
    if (isTRUE(input$vi_wave_compare)) {
      n_waves <- length(input$vi_waves_compare)
      h4(lab, tags$small(class = "text-muted ms-2",
                         paste0(vi_var(), " — ", n_waves, " waves")))
    } else {
      h4(lab, tags$small(class = "text-muted ms-2", vi_var()))
    }
  })

  vi_plot_obj <- reactive({
    req(vi_var())

    # ── Wave comparison plot ────────────────────────────────────────────────────
    if (isTRUE(input$vi_wave_compare)) {
      plot_df <- vi_compare_data()
      req(plot_df)

      vl <- get_value_labels(input$vi_wave, vi_var())

      # Clean: exclude NA, -55, -66
      plot_df <- plot_df[!is.na(plot_df$value) &
                           plot_df$value != -55 &
                           plot_df$value != -66, ]
      if (nrow(plot_df) == 0) return(NULL)

      # Wave display labels and chronological ordering
      wave_disp   <- setNames(wo$wave, tolower(wo$wave))
      wave_ids    <- intersect(wave_sort_order, unique(plot_df$wave))
      wave_labels <- wave_disp[wave_ids]
      plot_df$wave_label <- factor(wave_disp[plot_df$wave], levels = wave_labels)

      # Subtitle: n per wave
      wave_ns  <- tapply(seq_len(nrow(plot_df)), plot_df$wave_label, length)
      subtitle <- paste0("n: ", paste(names(wave_ns), format(wave_ns, big.mark = ","),
                                      sep = " = ", collapse = " | "))

      num_clean <- suppressWarnings(as.numeric(plot_df$value))
      use_bar   <- (!is.null(vl) && nrow(vl) > 0) ||
                   (!any(is.na(num_clean)) && length(unique(num_clean)) <= 20)

      style <- if (is.null(input$vi_compare_style)) "Facets" else input$vi_compare_style

      if (use_bar) {
        tbl <- as.data.frame(table(code       = plot_df$value,
                                    wave_label = plot_df$wave_label),
                             stringsAsFactors = FALSE)
        tbl$code_num <- suppressWarnings(as.integer(tbl$code))

        # % within wave
        group_totals <- tapply(tbl$Freq, tbl$wave_label, sum)
        tbl$pct      <- round(100 * tbl$Freq / group_totals[tbl$wave_label], 1)

        # Response category labels
        if (!is.null(vl) && nrow(vl) > 0) {
          tbl <- merge(tbl, vl, by.x = "code_num", by.y = "code", all.x = TRUE)
          tbl$label[is.na(tbl$label)] <- tbl$code[is.na(tbl$label)]
          tbl$DisplayLabel <- paste0(tbl$code, ": ", substr(tbl$label, 1, 25))
        } else {
          tbl$DisplayLabel <- tbl$code
        }

        # Custom sort order
        tbl$order_key <- ifelse(tbl$code_num == 0, 0,
                                ifelse(tbl$code_num > 0, tbl$code_num,
                                       1000 - tbl$code_num))
        tbl <- tbl[order(tbl$order_key, na.last = TRUE), ]
        tbl$DisplayLabel <- factor(tbl$DisplayLabel, levels = unique(tbl$DisplayLabel))
        tbl$wave_label   <- factor(tbl$wave_label,   levels = wave_labels)

        if (style == "Facets") {
          return(
            ggplot(tbl, aes(x = DisplayLabel, y = pct)) +
              geom_col(fill = "steelblue", alpha = 0.8, width = 0.7) +
              geom_text(aes(label = paste0(pct, "%")), vjust = -0.4, size = 2.5) +
              facet_wrap(~ wave_label) +
              labs(subtitle = subtitle, x = "", y = "% within wave") +
              scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
              bemp_theme()
          )
        } else {
          return(
            ggplot(tbl, aes(x = DisplayLabel, y = pct, fill = wave_label)) +
              geom_col(position = position_dodge(0.8), alpha = 0.85, width = 0.75) +
              geom_text(aes(label = paste0(pct, "%")),
                        position = position_dodge(0.8), vjust = -0.4, size = 2.5) +
              labs(subtitle = subtitle, x = "", y = "% within wave", fill = NULL) +
              scale_fill_brewer(palette = "Set1") +
              scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
              bemp_theme() +
              theme(legend.position = "bottom")
          )
        }

      } else {
        # Numeric
        plot_df$num_value <- suppressWarnings(as.numeric(plot_df$value))
        plot_df <- plot_df[!is.na(plot_df$num_value), ]
        if (nrow(plot_df) == 0) return(NULL)

        if (style == "Overlay") {
          return(
            ggplot(plot_df, aes(x = num_value, colour = wave_label, fill = wave_label)) +
              geom_density(alpha = 0.2) +
              labs(subtitle = subtitle, x = "Value", y = "Density",
                   colour = NULL, fill = NULL) +
              scale_colour_brewer(palette = "Set1") +
              scale_fill_brewer(palette = "Set1") +
              bemp_theme() +
              theme(legend.position = "bottom")
          )
        } else {
          return(
            ggplot(plot_df, aes(x = num_value)) +
              geom_histogram(fill = "steelblue", alpha = 0.8, color = "white", bins = 30) +
              facet_wrap(~ wave_label) +
              labs(subtitle = subtitle, x = "Value", y = "Frequency") +
              scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
              bemp_theme()
          )
        }
      }
    }

    # ── Grouped bar plot by respondent type ────────────────────────────────────
    rt_var   <- get_resp_type_var(input$vi_wave)
    do_group <- isTRUE(input$vi_group_by_resp_type) && !is.null(rt_var)

    if (do_group) {
      df_full <- vi_data()
      req(df_full)
      if (!vi_var() %in% names(df_full) || !rt_var %in% names(df_full)) return(NULL)

      col_data <- df_full[[vi_var()]]
      rt_data  <- df_full[[rt_var]]
      vl       <- get_value_labels(input$vi_wave, vi_var())
      rt_vl    <- get_value_labels(input$vi_wave, rt_var)

      # Clean: exclude NA, -55, -66 from response variable; exclude NA from rt
      keep      <- !is.na(col_data) & col_data != -55 & col_data != -66 & !is.na(rt_data)
      col_clean <- col_data[keep]
      rt_clean  <- rt_data[keep]

      # Filter to selected respondent types
      if (!is.null(input$vi_resp_types) && length(input$vi_resp_types) > 0) {
        keep_rt <- RESP_TYPE_LABELS[as.character(rt_clean)] %in% input$vi_resp_types
        keep_rt[is.na(keep_rt)] <- FALSE
        col_clean <- col_clean[keep_rt]
        rt_clean  <- rt_clean[keep_rt]
      }

      # Respondent type labels (fixed labels for cross-wave consistency)
      rt_codes <- sort(unique(rt_clean))
      rt_map   <- RESP_TYPE_LABELS[as.character(rt_codes)]
      rt_map[is.na(rt_map)] <- as.character(rt_codes[is.na(rt_map)])

      # Subtitle: n per group
      group_ns    <- tapply(rt_clean, rt_clean, length)
      subtitle_parts <- paste0(rt_map[names(group_ns)], " = ", group_ns)
      subtitle    <- paste0("n: ", paste(subtitle_parts, collapse = ", "))

      num_clean <- suppressWarnings(as.numeric(col_clean))
      use_bar   <- (!is.null(vl) && nrow(vl) > 0) ||
                   (is.numeric(col_clean) && length(unique(col_clean)) <= 20) ||
                   (!any(is.na(num_clean)) && length(unique(num_clean)) <= 20)

      if (use_bar) {
        tbl <- as.data.frame(table(code = col_clean, rt = rt_clean),
                             stringsAsFactors = FALSE)
        tbl$code_num <- suppressWarnings(as.integer(tbl$code))

        # % within respondent type group
        group_totals  <- tapply(tbl$Freq, tbl$rt, sum)
        tbl$pct       <- round(100 * tbl$Freq / group_totals[tbl$rt], 1)

        # Response category display labels
        if (!is.null(vl) && nrow(vl) > 0) {
          tbl <- merge(tbl, vl, by.x = "code_num", by.y = "code", all.x = TRUE)
          tbl$label[is.na(tbl$label)] <- tbl$code[is.na(tbl$label)]
          tbl$DisplayLabel <- paste0(tbl$code, ": ", substr(tbl$label, 1, 30))
        } else {
          tbl$DisplayLabel <- tbl$code
        }

        # Custom sort order
        tbl$order_key <- ifelse(tbl$code_num == 0, 0,
                                ifelse(tbl$code_num > 0, tbl$code_num,
                                       1000 - tbl$code_num))
        tbl <- tbl[order(tbl$order_key, na.last = TRUE), ]
        tbl$DisplayLabel <- factor(tbl$DisplayLabel, levels = unique(tbl$DisplayLabel))
        tbl$rt_label     <- rt_map[tbl$rt]
        tbl$rt_label     <- factor(tbl$rt_label, levels = unname(rt_map))

        return(
          ggplot(tbl, aes(x = DisplayLabel, y = pct, fill = rt_label)) +
            geom_col(position = position_dodge(0.8), alpha = 0.85, width = 0.75) +
            geom_text(aes(label = paste0(pct, "%")),
                      position = position_dodge(0.8),
                      vjust = -0.4, size = 2.8) +
            labs(subtitle = subtitle, x = "", y = "% within group", fill = NULL) +
            scale_fill_brewer(palette = "Set2") +
            scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
            bemp_theme() +
            theme(legend.position = "bottom")
        )

      } else {
        # Numeric: density curves per group
        num_data <- suppressWarnings(as.numeric(col_clean))
        valid    <- !is.na(num_data)
        plot_df  <- data.frame(x = num_data[valid],
                               rt_label = rt_map[as.character(rt_clean[valid])])
        plot_df$rt_label <- factor(plot_df$rt_label, levels = unname(rt_map))

        return(
          ggplot(plot_df, aes(x = x, colour = rt_label, fill = rt_label)) +
            geom_density(alpha = 0.25) +
            labs(subtitle = subtitle, x = "Value", y = "Density",
                 colour = NULL, fill = NULL) +
            scale_colour_brewer(palette = "Set2") +
            scale_fill_brewer(palette = "Set2") +
            bemp_theme() +
            theme(legend.position = "bottom")
        )
      }
    }

    # ── Standard (single) plot ─────────────────────────────────────────────────
    df <- vi_filtered_data()
    req(df)
    if (!vi_var() %in% names(df)) return(NULL)

    col_data <- df[[vi_var()]]
    vl       <- get_value_labels(input$vi_wave, vi_var())

    # Counts for subtitle
    na_count      <- sum(is.na(col_data))
    miss55_count  <- sum(col_data == -55, na.rm = TRUE)
    miss66_count  <- sum(col_data == -66, na.rm = TRUE)
    total_count   <- length(col_data)

    make_subtitle <- function(n_valid) {
      parts <- paste0("NA = ", na_count)
      if (miss55_count > 0) parts <- c(parts, paste0("-55 = ", miss55_count))
      if (miss66_count > 0) parts <- c(parts, paste0("-66 = ", miss66_count))
      parts <- c(parts, paste0("Total = ", total_count))
      paste0("n = ", n_valid, " (", paste(parts, collapse = ", "), ")")
    }

    # Clean data: exclude NA, -55, -66
    col_clean <- col_data[!is.na(col_data) & col_data != -55 & col_data != -66]

    # Decide plot type: use bar chart if value labels exist OR few unique numeric values
    num_clean  <- suppressWarnings(as.numeric(col_clean))
    use_bar    <- (!is.null(vl) && nrow(vl) > 0) ||
                  (is.numeric(col_clean) && length(unique(col_clean)) <= 20) ||
                  (!any(is.na(num_clean)) && length(unique(num_clean)) <= 20)

    if (use_bar) {
      tbl <- as.data.frame(table(col_clean, useNA = "no"), stringsAsFactors = FALSE)
      names(tbl) <- c("code_chr", "n")
      tbl$pct      <- round(100 * tbl$n / sum(tbl$n), 1)
      tbl$code_num <- suppressWarnings(as.integer(tbl$code_chr))

      # Attach value labels
      if (!is.null(vl) && nrow(vl) > 0) {
        tbl <- merge(tbl, vl, by.x = "code_num", by.y = "code", all.x = TRUE)
        tbl$label[is.na(tbl$label)] <- tbl$code_chr[is.na(tbl$label)]
        tbl$DisplayLabel <- paste0(tbl$code_chr, ": ", substr(tbl$label, 1, 30))
      } else {
        tbl$DisplayLabel <- tbl$code_chr
      }

      # Custom sort: 0 first, positives ascending, negatives descending
      tbl$order_key <- ifelse(tbl$code_num == 0, 0,
                              ifelse(tbl$code_num > 0, tbl$code_num,
                                     1000 - tbl$code_num))
      tbl <- tbl[order(tbl$order_key, na.last = TRUE), ]
      tbl$DisplayLabel <- factor(tbl$DisplayLabel, levels = tbl$DisplayLabel)

      subtitle <- make_subtitle(sum(tbl$n))

      # Binary (only 0 and 1)
      if (setequal(na.omit(tbl$code_num), c(0L, 1L)) || setequal(na.omit(tbl$code_num), 0L) || setequal(na.omit(tbl$code_num), 1L)) {
        if (all(na.omit(tbl$code_num) %in% c(0L, 1L)) && nrow(tbl) <= 2) {
          tbl$DisplayLabel <- ifelse(tbl$code_num == 0L, "0: Not Selected", "1: Selected")
          tbl$DisplayLabel <- factor(tbl$DisplayLabel, levels = tbl$DisplayLabel)
          fill_colors <- ifelse(tbl$code_num == 0L, "coral3", "darkgreen")

          return(
            ggplot(tbl, aes(x = DisplayLabel, y = n)) +
              geom_col(fill = fill_colors, alpha = 0.8, width = 0.7) +
              geom_text(aes(label = paste0(n, "\n(", pct, "%)")), vjust = -0.5, size = 3) +
              labs(subtitle = subtitle, x = "", y = "Frequency") +
              scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
              bemp_theme()
          )
        }
      }

      ggplot(tbl, aes(x = DisplayLabel, y = n)) +
        geom_col(fill = "steelblue", alpha = 0.8, width = 0.7) +
        geom_text(aes(label = paste0(n, "\n(", pct, "%)")), vjust = -0.5, size = 3) +
        labs(subtitle = subtitle, x = "", y = "Frequency") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
        bemp_theme()

    } else {
      # Numeric histogram
      num_data <- num_clean[!is.na(num_clean)]
      if (length(num_data) == 0) return(NULL)

      subtitle <- make_subtitle(length(num_data))

      ggplot(data.frame(x = num_data), aes(x = x)) +
        geom_histogram(fill = "steelblue", alpha = 0.8, color = "white", bins = 30) +
        labs(subtitle = subtitle, x = "Value", y = "Frequency") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        bemp_theme()
    }
  })

  # Dynamic plot output: taller for faceted compare plots
  output$vi_plot_output <- renderUI({
    in_compare <- isTRUE(input$vi_wave_compare)
    is_facets  <- is.null(input$vi_compare_style) || input$vi_compare_style == "Facets"
    n_waves    <- if (in_compare) length(input$vi_waves_compare) else 1
    height_px  <- if (in_compare && is_facets && n_waves > 2) "560px" else "380px"
    plotOutput("vi_plot", height = height_px)
  })

  output$vi_plot <- renderPlot({ vi_plot_obj() })

  output$vi_dl_btn <- renderUI({
    req(vi_plot_obj())
    in_compare <- isTRUE(input$vi_wave_compare)
    fname_base <- if (in_compare)
      paste0("bemp_compare_", vi_var())
    else
      paste0("bemp_", input$vi_wave, "_", vi_var())
    downloadButton("vi_plot_dl", "Save plot (.png)",
                   class = "btn-outline-secondary btn-sm mt-1")
  })

  output$vi_plot_dl <- downloadHandler(
    filename = function() {
      if (isTRUE(input$vi_wave_compare))
        paste0("bemp_compare_", vi_var(), ".png")
      else
        paste0("bemp_", input$vi_wave, "_", vi_var(), ".png")
    },
    content = function(file) {
      in_compare <- isTRUE(input$vi_wave_compare)
      is_facets  <- is.null(input$vi_compare_style) || input$vi_compare_style == "Facets"
      n_waves    <- if (in_compare) length(input$vi_waves_compare) else 1
      h <- if (in_compare && is_facets && n_waves > 2) 7 else 5
      ggsave(file, vi_plot_obj(), width = 8, height = h, dpi = 150)
    }
  )

  output$vi_summary <- renderUI({
    if (isTRUE(input$vi_wave_compare)) return(NULL)
    req(vi_var())
    df <- vi_filtered_data()
    req(df)
    if (!vi_var() %in% names(df)) return(NULL)

    col_data  <- df[[vi_var()]]
    n_total   <- length(col_data)
    n_na      <- sum(is.na(col_data))
    n_55      <- sum(col_data == -55, na.rm = TRUE)
    n_66      <- sum(col_data == -66, na.rm = TRUE)
    n_valid   <- n_total - n_na - n_55 - n_66
    wave_lc   <- tolower(input$vi_wave)
    show_55   <- wave_lc %in% waves_with_55
    show_66   <- wave_lc %in% waves_with_66

    miss_pct <- function(n) round(100 * n / n_total, 1)

    miss_td <- function(n) {
      pct <- miss_pct(n)
      tags$td(
        sprintf("%s (%.1f%%)", format(n, big.mark = ","), pct),
        div(class = "progress mt-1", style = "height: 5px;",
          div(class = "progress-bar bg-warning", role = "progressbar",
              style = paste0("width:", min(pct, 100), "%;"))
        )
      )
    }

    stats_rows <- list(
      tags$tr(tags$th("N (total)"), tags$td(format(n_total, big.mark = ","))),
      tags$tr(tags$th("n (valid)"), miss_td(n_valid))
    )

    stats_rows <- c(stats_rows, list(
      tags$tr(tags$th("Missing (NA)"), miss_td(n_na))
    ))

    if (show_55) {
      stats_rows <- c(stats_rows, list(
        tags$tr(tags$th("Missing (panel wave dropout)"), miss_td(n_55))
      ))
    }

    if (show_66) {
      stats_rows <- c(stats_rows, list(
        tags$tr(tags$th("Missing (due to migrant status)"), miss_td(n_66))
      ))
    }

    vl       <- get_value_labels(input$vi_wave, vi_var())
    num_data <- suppressWarnings(as.numeric(col_data))
    if (is.null(vl) || nrow(vl) == 0) {
      num_data <- num_data[which(num_data != -55 & num_data != -66)]
      num_data <- num_data[!is.na(num_data)]
      if (length(num_data) > 0) {
        stats_rows <- c(stats_rows, list(
          tags$tr(tags$th("Mean"),   tags$td(round(mean(num_data),   2))),
          tags$tr(tags$th("Median"), tags$td(round(median(num_data), 2))),
          tags$tr(tags$th("SD"),     tags$td(round(sd(num_data),     2))),
          tags$tr(tags$th("Min"),    tags$td(min(num_data))),
          tags$tr(tags$th("Max"),    tags$td(max(num_data)))
        ))
      }
    }

    tagList(
      tags$table(class = "table table-sm table-borderless w-auto",
                 tags$tbody(stats_rows))
    )
  })


  # ── Tab 4: Correlation Explorer ───────────────────────────────────────────

  ce_data <- reactiveVal(NULL)

  observeEvent(input$ce_wave, {
    ce_data(NULL)
    withProgress(message = paste("Loading", input$ce_wave, "..."), value = 0.5, {
      df <- tryCatch(get_wave(input$ce_wave), error = function(e) NULL)
    })
    ce_data(df)
    if (is.null(df)) return()

    # Only offer variables that are numeric in the data and in the codebook
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    wave_cb  <- cb[cb$wave == tolower(input$ce_wave) &
                     cb$variable_name %in% num_cols, ]
    wave_cb  <- wave_cb[order(wave_cb$variable_name), ]
    choices <- setNames(
      wave_cb$variable_name,
      make_var_choice_label(wave_cb$variable_name, wave_cb$variable_label,
                            wave_cb$item_text, wave_cb$question)
    )
    updateSelectizeInput(session, "ce_var_x", choices = choices,
                         selected = character(0))
    updateSelectizeInput(session, "ce_var_y", choices = choices,
                         selected = character(0))
  })

  output$ce_main <- renderUI({
    df <- ce_data()
    if (is.null(df)) {
      return(p(class = "text-muted mt-3",
               "Wave data is loading — or select a wave to begin."))
    }
    vx <- input$ce_var_x
    vy <- input$ce_var_y
    if (is.null(vx) || !nzchar(vx) || is.null(vy) || !nzchar(vy)) {
      return(p(class = "text-muted mt-3",
               "Select X and Y variables from the sidebar."))
    }
    tagList(
      plotOutput("ce_plot", height = "500px"),
      uiOutput("ce_dl_btn")
    )
  })

  ce_plot_obj <- reactive({
    req(input$ce_var_x, input$ce_var_y, nzchar(input$ce_var_x), nzchar(input$ce_var_y))
    df <- ce_data()
    req(df, input$ce_var_x %in% names(df), input$ce_var_y %in% names(df))

    # Replace negative special missing codes with NA
    x <- as.numeric(df[[input$ce_var_x]])
    y <- as.numeric(df[[input$ce_var_y]])
    x[!is.na(x) & x < 0] <- NA
    y[!is.na(y) & y < 0] <- NA

    plot_df <- data.frame(x = x, y = y)
    plot_df  <- plot_df[complete.cases(plot_df), ]
    validate(need(nrow(plot_df) >= 5,
                  "Not enough valid observations to compute a correlation."))

    ct    <- cor.test(plot_df$x, plot_df$y, method = "pearson")
    r     <- round(ct$estimate, 3)
    p_str <- if (ct$p.value < 0.001) "p < 0.001" else
               paste0("p = ", round(ct$p.value, 3))
    n     <- nrow(plot_df)

    lbl <- function(var) {
      row <- cb[cb$wave == tolower(input$ce_wave) & cb$variable_name == var, ]
      if (nrow(row) == 0) return(var)
      label <- row$variable_label[1]
      it    <- row$item_text[1]
      if (!is.na(it) && nzchar(it) && grepl("[0-9]$", strip_wave_prefix(label))) {
        paste0(label, "-", it)
      } else {
        label
      }
    }

    # Use slight jitter to reveal overplotting in ordinal/integer variables
    n_ux <- length(unique(plot_df$x))
    n_uy <- length(unique(plot_df$y))
    jw <- if (n_ux <= 20) 0.15 else 0
    jh <- if (n_uy <= 20) 0.15 else 0

    ggplot(plot_df, aes(x = x, y = y)) +
      geom_point(alpha = 0.25, color = BEMP_BLUE, size = 1.5,
                 position = position_jitter(width = jw, height = jh, seed = 1)) +
      geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                  color = "#d6604d", fill = "#f4a582", linewidth = 0.8) +
      labs(
        title    = paste0(input$ce_wave, ":  ", input$ce_var_x, "  \u2014  ", input$ce_var_y),
        subtitle = paste0("r = ", r, "   (", p_str, ",  n = ",
                          format(n, big.mark = ","), ")"),
        x = paste0(input$ce_var_x, "\n", lbl(input$ce_var_x)),
        y = paste0(input$ce_var_y, "\n", lbl(input$ce_var_y))
      ) +
      bemp_theme() +
      theme(axis.text.x  = element_text(angle = 0, hjust = 0.5),
            plot.subtitle = element_text(color = BEMP_BLUE, face = "bold", size = 11))
  })

  output$ce_plot <- renderPlot({
    req(ce_plot_obj())
    ce_plot_obj()
  }, res = 96)

  output$ce_dl_btn <- renderUI({
    req(input$ce_var_x, input$ce_var_y, nzchar(input$ce_var_x), nzchar(input$ce_var_y))
    tagList(
      downloadButton("ce_download",     "Export plot",
                     class = "btn-outline-primary btn-sm mt-2"),
      downloadButton("ce_download_csv", "Export CSV",
                     class = "btn-outline-secondary btn-sm mt-2 ms-1")
    )
  })

  output$ce_download <- downloadHandler(
    filename = function() {
      paste0("bemp_", input$ce_wave, "_", input$ce_var_x, "_vs_", input$ce_var_y, ".png")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = ce_plot_obj(),
                      width = 7, height = 5, dpi = 150, bg = "white")
    }
  )

  output$ce_download_csv <- downloadHandler(
    filename = function() {
      paste0("bemp_", input$ce_wave, "_", input$ce_var_x, "_", input$ce_var_y, ".csv")
    },
    content = function(file) {
      df     <- ce_data()
      rc_var <- get_resp_code_var(input$ce_wave)
      keep   <- unique(c(if (!is.null(rc_var) && rc_var %in% names(df)) rc_var,
                         input$ce_var_x, input$ce_var_y))
      write.csv(df[, intersect(keep, names(df)), drop = FALSE],
                file, row.names = FALSE)
    }
  )

  # ── Tab 4: Panel Dynamics ─────────────────────────────────────────────────

  updateSelectizeInput(session, "pd_var",
                       choices  = pd_choices,
                       selected = character(0),
                       server   = TRUE)

  pd_results <- reactiveVal(NULL)
  pd_raw     <- reactiveVal(NULL)

  observeEvent(input$pd_load, {
    req(input$pd_var, nzchar(input$pd_var))
    pd_results(NULL)
    pd_raw(NULL)

    # Parse appears_in_waves → named vector  wave_id → var_name
    wave_vars <- parse_appears(input$pd_var)
    if (length(wave_vars) == 0) return()

    # Filter to requested wave types and sort chronologically
    keep <- names(wave_vars) %in% wave_sort_order &
      tolower(wo$type[match(names(wave_vars), tolower(wo$wave))]) %in%
        input$pd_wave_type
    wave_vars <- wave_vars[keep]
    wave_vars <- wave_vars[order(match(names(wave_vars), wave_sort_order))]
    if (length(wave_vars) == 0) {
      showNotification("No waves match the selected wave-type filter.",
                       type = "warning")
      return()
    }

    items <- withProgress(message = "Loading waves...", value = 0, {
      lapply(seq_along(wave_vars), function(i) {
        wv  <- names(wave_vars)[i]
        var <- wave_vars[[i]]
        incProgress(1 / length(wave_vars), detail = wv)

        # get_wave() needs original-case wave ID (e.g. "w6_N", not "w6_n")
        wv_orig <- wo$wave[match(wv, tolower(wo$wave))]
        if (is.na(wv_orig)) return(list(summary = NULL, raw = NULL))
        df <- tryCatch(get_wave(wv_orig), error = function(e) NULL)
        if (is.null(df) || !var %in% names(df)) return(list(summary = NULL, raw = NULL))

        x <- as.numeric(df[[var]])
        x[!is.na(x) & x < 0] <- NA

        # Raw respondent-level data for CSV export
        rc_var <- get_resp_code_var(wv)
        rc     <- if (!is.null(rc_var) && rc_var %in% names(df)) df[[rc_var]] else rep(NA_character_, nrow(df))
        raw_df <- setNames(data.frame(rc, x, stringsAsFactors = FALSE),
                           c("resp_code", paste0(var, "_", wv)))

        summary_df <- if (isTRUE(input$pd_subgroup)) {
          rt_var <- get_resp_type_var(wv)
          if (!is.null(rt_var) && rt_var %in% names(df)) {
            vl <- get_value_labels(wv, rt_var)
            grp_raw <- df[[rt_var]]
            grp_raw[!is.na(grp_raw) & grp_raw < 0] <- NA

            # Collapse by numeric code; use fixed labels to avoid cross-wave label drift
            valid_codes <- sort(unique(grp_raw[!is.na(grp_raw)]))
            do.call(rbind, lapply(valid_codes, function(g) {
              xi <- x[!is.na(grp_raw) & grp_raw == g]
              xi <- xi[!is.na(xi)]
              if (length(xi) < 5) return(NULL)
              lbl <- RESP_TYPE_LABELS[as.character(g)]
              if (is.na(lbl)) lbl <- as.character(g)
              data.frame(wave = wv, group = lbl, n = length(xi),
                         mean  = mean(xi),
                         ci_lo = mean(xi) - qt(0.975, length(xi) - 1) * sd(xi) / sqrt(length(xi)),
                         ci_hi = mean(xi) + qt(0.975, length(xi) - 1) * sd(xi) / sqrt(length(xi)),
                         stringsAsFactors = FALSE)
            }))
          } else NULL
        } else {
          xi <- x[!is.na(x)]
          if (length(xi) < 5) return(list(summary = NULL, raw = raw_df))
          data.frame(wave = wv, group = NA_character_, n = length(xi),
                     mean  = mean(xi),
                     ci_lo = mean(xi) - qt(0.975, length(xi) - 1) * sd(xi) / sqrt(length(xi)),
                     ci_hi = mean(xi) + qt(0.975, length(xi) - 1) * sd(xi) / sqrt(length(xi)),
                     stringsAsFactors = FALSE)
        }
        list(summary = summary_df, raw = raw_df)
      })
    })

    result <- do.call(rbind, Filter(Negate(is.null), lapply(items, `[[`, "summary")))
    if (is.null(result) || nrow(result) == 0) {
      showNotification("No valid data found for the selected variable and filters.",
                       type = "warning")
      return()
    }
    result$wave_f <- factor(result$wave, levels = wave_sort_order)
    pd_results(result)

    raw_list <- Filter(Negate(is.null), lapply(items, `[[`, "raw"))
    pd_raw(if (length(raw_list) > 0)
      Reduce(function(a, b) merge(a, b, by = "resp_code", all = TRUE), raw_list)
    else NULL)
  })

  output$pd_status <- renderUI({
    df <- pd_results()
    if (is.null(df)) return(NULL)
    n_waves <- length(unique(df$wave))
    p(class = "text-muted small",
      sprintf("Loaded: %d wave%s, %s total observations",
              n_waves, if (n_waves == 1) "" else "s",
              format(sum(df$n), big.mark = ",")))
  })

  output$pd_resp_filter_ui <- renderUI({
    if (!isTRUE(input$pd_subgroup)) return(NULL)
    div(style = "margin-left: 18px; margin-top: 2px;",
      checkboxGroupInput(
        "pd_resp_types", NULL,
        choices  = unname(RESP_TYPE_LABELS),
        selected = unname(RESP_TYPE_LABELS)
      )
    )
  })

  output$pd_plot_ui <- renderUI({
    if (is.null(pd_results())) {
      return(p(class = "text-muted mt-3",
               "Select a variable and click ", tags$b("Load / Update"), "."))
    }
    plotOutput("pd_plot", height = "420px")
  })

  # Build the ggplot object as a reactive so it can be both rendered and exported
  pd_plot_obj <- reactive({
    df <- pd_results()
    req(df, nrow(df) > 0)

    subgroup <- isTRUE(input$pd_subgroup) && !all(is.na(df$group))
    if (subgroup && !is.null(input$pd_resp_types) && length(input$pd_resp_types) > 0) {
      df <- df[df$group %in% input$pd_resp_types, ]
    }
    if (subgroup) {
      df$group <- factor(df$group, levels = unname(RESP_TYPE_LABELS))
    }

    # Title: "Question title (clean_label[, item text])" or just clean_label
    rep_row <- tracked_vars[tracked_vars$appears_in_waves == input$pd_var, ]
    plot_title <- ""
    if (nrow(rep_row) > 0) {
      clean_label <- strip_wave_prefix(rep_row$variable_label[1])
      item_text_val <- if ("item_text" %in% names(rep_row)) rep_row$item_text[1] else NA_character_
      has_item <- grepl("[0-9]$", clean_label) &&
                  !is.na(item_text_val) && nzchar(item_text_val)
      inner_label <- if (has_item) paste0(clean_label, ", ", item_text_val) else clean_label
      q_col  <- if ("question" %in% names(rep_row)) rep_row$question[1] else NA_character_
      q_title <- if (!is.na(q_col) && nzchar(q_col)) q_col else clean_label
      plot_title <- if (!is.na(q_title) && nzchar(q_title) && q_title != clean_label) {
        paste0(q_title, " (", inner_label, ")")
      } else {
        inner_label
      }
    }

    # Y-axis: for variables with a fixed response scale, use value labels as tick marks
    y_limits      <- NULL
    y_axis_labels <- NULL
    if (nrow(rep_row) > 0) {
      first_wv_vars <- parse_appears(rep_row$appears_in_waves[1])
      if (length(first_wv_vars) > 0) {
        fwv  <- names(first_wv_vars)[1]
        fvar <- first_wv_vars[[1]]
        vl   <- get_value_labels(fwv, fvar)
        if (!is.null(vl) && nrow(vl) > 0) {
          pos_vl <- vl[vl$code > 0, ]
          pos_vl <- pos_vl[order(pos_vl$code), ]
          if (nrow(pos_vl) >= 2) {
            y_limits <- c(min(pos_vl$code), max(pos_vl$code))
            ax_lbl <- paste0(pos_vl$code, ": ", pos_vl$label)
            ax_lbl <- ifelse(nchar(ax_lbl) > 22,
                             paste0(substr(ax_lbl, 1, 20), "\u2026"), ax_lbl)
            y_axis_labels <- setNames(ax_lbl, as.character(pos_vl$code))
          }
        }
      }
    }

    plt <- ggplot(df, aes(x = wave_f, y = mean,
                          group  = if (subgroup) group else 1,
                          color  = if (subgroup) group else NULL,
                          fill   = if (subgroup) group else NULL))

    if (subgroup) {
      plt <- plt +
        geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, color = NA) +
        geom_line(linewidth = 0.8) +
        geom_point(size = 2.5) +
        scale_color_brewer(palette = "Set1", name = "Respondent type") +
        scale_fill_brewer(palette  = "Set1", guide = "none")
    } else {
      plt <- plt +
        geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
                    fill = BEMP_BLUE, alpha = 0.15, color = NA) +
        geom_line(color = BEMP_BLUE, linewidth = 0.8) +
        geom_point(color = BEMP_BLUE, size = 2.5)
    }

    plt <- plt +
      geom_text(aes(y = ci_lo, label = paste0("n=", format(n, big.mark = ","))),
                vjust = 1.6, size = 2.8, color = "grey50",
                position = if (subgroup) position_dodge(0.3) else "identity") +
      labs(
        title    = plot_title,
        subtitle = "Mean \u00b1 95% CI  \u2022  negative special missing codes excluded",
        x = NULL, y = "Mean"
      ) +
      scale_x_discrete(labels = function(x) wave_label_map[x]) +
      bemp_theme() +
      theme(axis.text.x     = element_text(angle = 45, hjust = 1),
            legend.position = if (subgroup) "bottom" else "none")

    if (!is.null(y_limits)) {
      plt <- plt + coord_cartesian(ylim = y_limits)
      if (!is.null(y_axis_labels)) {
        plt <- plt +
          scale_y_continuous(breaks = as.integer(names(y_axis_labels)),
                             labels = unname(y_axis_labels)) +
          theme(axis.text.y = element_text(angle = -35, hjust = 1, vjust = 0.5),
                axis.title.y = element_blank())
      }
    }
    plt
  })

  output$pd_plot <- renderPlot({
    req(pd_plot_obj())
    pd_plot_obj()
  }, res = 96)

  output$pd_dl_btn <- renderUI({
    if (is.null(pd_results())) return(NULL)
    tagList(
      downloadButton("pd_download",     "Export plot",
                     class = "btn-outline-primary btn-sm mt-2"),
      downloadButton("pd_download_csv", "Export CSV",
                     class = "btn-outline-secondary btn-sm mt-2 ms-1")
    )
  })

  output$pd_download <- downloadHandler(
    filename = function() {
      rep_row <- tracked_vars[tracked_vars$appears_in_waves == input$pd_var, ]
      lbl <- if (nrow(rep_row) > 0) strip_wave_prefix(rep_row$variable_label[1]) else "panel_dynamics"
      lbl <- gsub("[^A-Za-z0-9_]", "_", lbl)
      paste0("bemp_pd_", lbl, ".png")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = pd_plot_obj(),
                      width = 8, height = 5, dpi = 150, bg = "white")
    }
  )

  output$pd_download_csv <- downloadHandler(
    filename = function() {
      rep_row <- tracked_vars[tracked_vars$appears_in_waves == input$pd_var, ]
      lbl <- if (nrow(rep_row) > 0) strip_wave_prefix(rep_row$variable_label[1]) else "panel_dynamics"
      lbl <- gsub("[^A-Za-z0-9_]", "_", lbl)
      paste0("bemp_pd_", lbl, ".csv")
    },
    content = function(file) {
      write.csv(pd_raw(), file, row.names = FALSE)
    }
  )

  output$pd_table_ui <- renderUI({
    if (is.null(pd_results())) return(NULL)
    DTOutput("pd_table")
  })

  output$pd_table <- renderDT({
    df <- pd_results()
    req(df)
    subgroup <- isTRUE(input$pd_subgroup) && !all(is.na(df$group))
    if (subgroup && !is.null(input$pd_resp_types) && length(input$pd_resp_types) > 0) {
      df <- df[df$group %in% input$pd_resp_types, ]
    }
    display <- data.frame(
      Wave  = wave_label_map[as.character(df$wave_f)],
      N     = df$n,
      Mean  = round(df$mean, 3),
      CI_95 = paste0("[", round(df$ci_lo, 3), ", ", round(df$ci_hi, 3), "]"),
      stringsAsFactors = FALSE
    )
    if (subgroup) display <- cbind(Group = df$group, display)
    datatable(display, rownames = FALSE, style = "bootstrap5",
              options = list(dom = "t", pageLength = nrow(display),
                             order = list()))
  })

  # ── Tab 5: Download Assistant ─────────────────────────────────────────────

  # Dynamic per-wave variable selectors
  output$mw_var_selectors <- renderUI({
    waves <- input$mw_waves
    if (is.null(waves) || length(waves) == 0) return(NULL)
    lapply(waves, function(wv) {
      wave_cb <- cb[cb$wave == tolower(wv), ]
      rc_var  <- get_resp_code_var(wv)
      # Exclude resp_code itself when merging (always included as merge key)
      if (!is.null(rc_var)) wave_cb <- wave_cb[wave_cb$variable_name != rc_var, ]
      choices <- setNames(wave_cb$variable_name,
                          make_var_choice_label(wave_cb$variable_name,
                                               wave_cb$variable_label,
                                               wave_cb$item_text,
                                               wave_cb$question))
      tagList(
        hr(),
        selectizeInput(
          paste0("mw_vars_", tolower(wv)),
          paste0(wave_label_map[tolower(wv)], " variables (empty = all)"),
          choices  = choices,
          selected = NULL,
          multiple = TRUE,
          options  = list(placeholder = "All variables",
                          maxOptions  = 2000,
                          searchField = c("label", "value"))
        )
      )
    })
  })

  mw_data             <- reactiveVal(NULL)
  mw_any_vars_selected <- reactiveVal(FALSE)

  observeEvent(input$mw_merge, {
    waves <- input$mw_waves
    req(waves, length(waves) >= 1)
    mw_data(NULL)

    if (length(waves) == 1) {
      # Single-wave download — no merge needed, all wave types supported
      withProgress(message = paste("Loading", waves, "..."), value = 0.5, {
        wv_orig <- wo$wave[match(tolower(waves), tolower(wo$wave))]
        df <- tryCatch(get_wave(wv_orig), error = function(e) {
          showNotification("Failed to load wave data. Check your internet connection.",
                           type = "error")
          NULL
        })
      })
      if (is.null(df)) return()
      sel_vars <- input[[paste0("mw_vars_", tolower(waves))]]
      if (length(sel_vars) > 0) {
        keep <- intersect(sel_vars, names(df))
        if (length(keep) > 0) df <- df[, keep, drop = FALSE]
      }
      mw_any_vars_selected(length(sel_vars) > 0)
      mw_data(df)
      return()
    }

    # Multi-wave merge
    wave_dfs <- withProgress(message = "Loading and merging waves...", value = 0, {
      lapply(seq_along(waves), function(i) {
        wv      <- waves[i]
        wv_orig <- wo$wave[match(tolower(wv), tolower(wo$wave))]
        incProgress(1 / length(waves), detail = wv_orig)

        rc_var <- get_resp_code_var(wv)
        if (is.na(wv_orig) || is.null(rc_var)) return(NULL)

        df <- tryCatch(get_wave(wv_orig), error = function(e) NULL)
        if (is.null(df)) return(NULL)

        # Selected variables for this wave (NULL = all)
        sel_vars <- input[[paste0("mw_vars_", tolower(wv))]]
        if (length(sel_vars) > 0) {
          keep <- intersect(sel_vars, names(df))
          df   <- df[, c(rc_var, keep), drop = FALSE]
        }

        # Rename resp_code to shared key "resp_code"
        names(df)[names(df) == rc_var] <- "resp_code"
        df
      })
    })

    wave_dfs <- Filter(Negate(is.null), wave_dfs)
    if (length(wave_dfs) < 1) {
      showNotification("Could not load data for any wave.", type = "warning")
      return()
    }

    merged <- Reduce(function(x, y) merge(x, y, by = "resp_code", all = TRUE), wave_dfs)
    any_sel <- any(vapply(waves, function(wv) {
      length(input[[paste0("mw_vars_", tolower(wv))]]) > 0
    }, logical(1)))
    mw_any_vars_selected(any_sel)
    mw_data(merged)
  })

  output$mw_info <- renderUI({
    df <- mw_data()
    if (is.null(df)) {
      return(p(class = "text-muted mt-3",
               "Select wave(s) and click ", tags$b("Load / Merge"), "."))
    }
    n_waves <- length(input$mw_waves)
    # For multi-wave merges resp_code is the join key; subtract it from the variable count
    n_vars <- ncol(df) - if (n_waves > 1L && "resp_code" %in% names(df)) 1L else 0L
    waves_label <- if (n_waves > 1L) "Waves merged" else "Wave loaded"
    tagList(
      div(class = "d-flex gap-4 flex-wrap mt-2",
        div(tags$span(class = "text-muted small", "Rows"),
            tags$b(format(nrow(df), big.mark = ","))),
        div(tags$span(class = "text-muted small", "Variables"),
            tags$b(format(n_vars, big.mark = ","))),
        div(tags$span(class = "text-muted small", waves_label),
            tags$b(n_waves))
      )
    )
  })

  output$mw_preview <- renderDT({
    df <- mw_data()
    req(df)

    if (mw_any_vars_selected()) {
      # Variables selected: show all rows in a scrollable table
      datatable(df, rownames = FALSE, style = "bootstrap5",
                options = list(scrollX = TRUE, scrollY = "400px",
                               scrollCollapse = TRUE, paging = FALSE, dom = "rti"))
    } else {
      # Full wave: limit preview to first 100 rows
      preview_df  <- head(df[, seq_len(min(ncol(df), 20L)), drop = FALSE], 100)
      row_clipped <- nrow(df) > 100
      col_clipped <- ncol(df) > 20
      cap <- if (row_clipped || col_clipped) {
        msg <- sprintf(
          "Showing first %s of %s rows and first %s of %s columns. Select specific variables above to preview all.",
          nrow(preview_df), format(nrow(df), big.mark = ","),
          ncol(preview_df), format(ncol(df), big.mark = ",")
        )
        tags$caption(
          style = "caption-side: top; text-align: left; color: #6c757d; font-size: 0.85em; padding-bottom: 4px;",
          msg
        )
      } else NULL
      datatable(preview_df, rownames = FALSE, style = "bootstrap5",
                caption = cap,
                options = list(scrollX = TRUE, scrollY = "400px",
                               scrollCollapse = TRUE, paging = FALSE, dom = "rti"))
    }
  })

  output$mw_download_ui <- renderUI({
    tagList(
      if (!is.null(mw_data()))
        tagList(
          downloadButton("mw_download", "Download CSV", class = "btn-success w-100"),
          br(), br()
        ),
      downloadButton("mw_codebook", "Download codebook (CSV)",
                     class = "btn-outline-secondary btn-sm w-100")
    )
  })

  output$mw_download <- downloadHandler(
    filename = function() {
      waves_tag <- paste(tolower(input$mw_waves), collapse = "_")
      prefix    <- if (length(input$mw_waves) > 1L) "bemp_merged_" else "bemp_"
      paste0(prefix, waves_tag, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(mw_data(), file, row.names = FALSE)
    }
  )

  output$mw_codebook <- downloadHandler(
    filename = function() {
      waves_tag <- paste(tolower(input$mw_waves), collapse = "_")
      prefix    <- if (length(input$mw_waves) > 1L) "bemp_merged_" else "bemp_"
      paste0(prefix, waves_tag, "_codebook_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- mw_data()
      merged_vars <- if (!is.null(df)) setdiff(names(df), "resp_code") else character(0)
      wave_cb <- cb[tolower(cb$wave) %in% tolower(input$mw_waves), ]
      if (length(merged_vars) > 0) {
        wave_cb <- wave_cb[wave_cb$variable_name %in% merged_vars, ]
      }
      write.csv(wave_cb, file, row.names = FALSE)
    }
  )

  # ── Bookmarking ───────────────────────────────────────────────────────────
  # Exclude action-button counters from bookmark state
  setBookmarkExclude(c("mw_merge", "pd_load", "cb_reset"))

  # When bookmarking: save the VI variable by name (row index is fragile)
  onBookmark(function(state) {
    vv <- vi_var()
    if (!is.null(vv)) state$values$vi_var <- vv
  })

  # When restoring from a bookmark: pre-select the VI variable in the DT
  onRestore(function(state) {
    if (!is.null(state$values$vi_var))
      vi_url_var(state$values$vi_var)
  })

  # ── Tab 6: About ──────────────────────────────────────────────────────────

  output$about_waves <- renderDT({
    datatable(
      wo,
      rownames = FALSE,
      style    = "bootstrap5",
      colnames = c("Wave", "Round", "Type", "Questionnaire"),
      options  = list(pageLength = 20, dom = "t")
    )
  })
}
