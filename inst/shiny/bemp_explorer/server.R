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

    df
  })

  output$cb_n_results <- renderUI({
    n <- nrow(cb_filtered())
    tags$p(class = "text-muted small",
           sprintf("%s variable%s found", format(n, big.mark = ","),
                   if (n == 1) "" else "s"))
  })

  output$cb_table <- renderDT({
    cb_filtered() |>
      select(wave, variable_name, variable_label, block, question, question_type) |>
      datatable(
        selection  = "single",
        rownames   = FALSE,
        filter     = "none",
        style      = "bootstrap5",
        colnames   = c("Wave", "Variable", "Label", "Block", "Question", "Type"),
        options    = list(
          pageLength = 15,
          scrollX    = TRUE,
          dom        = "tip"
        )
      )
  }, server = TRUE)

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
      h5(paste0(var_name, "  —  ", row$variable_label)),
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
      }
    )
  })


  # ── Tab 2: Variable Inspector ──────────────────────────────────────────────

  # Load wave data reactively (cached on disk by get_wave())
  vi_data <- reactiveVal(NULL)

  observeEvent(input$vi_wave, {
    vi_data(NULL)
    withProgress(message = paste("Loading", input$vi_wave, "..."), value = 0.5, {
      df <- tryCatch(get_wave(input$vi_wave), error = function(e) NULL)
    })
    vi_data(df)

    # Populate variable selector from codebook for this wave
    wave_cb <- cb[cb$wave == tolower(input$vi_wave), ]
    # Keep only variables that actually exist in the data
    if (!is.null(df)) {
      wave_cb <- wave_cb[wave_cb$variable_name %in% names(df), ]
    }
    choices <- setNames(
      wave_cb$variable_name,
      paste0(wave_cb$variable_name, ": ", wave_cb$variable_label)
    )
    updateSelectizeInput(session, "vi_var",
                         choices  = choices,
                         selected = character(0),
                         server   = TRUE)
  })

  output$vi_meta <- renderUI({
    req(input$vi_var, input$vi_wave)
    row <- get_cb_row(input$vi_wave, input$vi_var)
    if (nrow(row) == 0) return(NULL)
    tagList(
      hr(),
      tags$small(
        tags$b("Block: "), row$block, tags$br(),
        tags$b("Type: "),  row$question_type
      )
    )
  })

  output$vi_title <- renderUI({
    req(input$vi_var)
    row <- get_cb_row(input$vi_wave, input$vi_var)
    lab <- if (nrow(row) > 0) row$variable_label[1] else input$vi_var
    h4(lab, tags$small(class = "text-muted ms-2", input$vi_var))
  })

  output$vi_plot <- renderPlot({
    req(input$vi_var)
    df <- vi_data()
    req(df)
    if (!input$vi_var %in% names(df)) return(NULL)

    col_data <- df[[input$vi_var]]
    vl       <- get_value_labels(input$vi_wave, input$vi_var)

    if (!is.null(vl) && nrow(vl) > 0) {
      # Categorical: bar chart with value labels
      tbl <- as.data.frame(table(col_data, useNA = "ifany"),
                            stringsAsFactors = FALSE)
      names(tbl) <- c("code_chr", "n")
      tbl$code_num <- suppressWarnings(as.integer(tbl$code_chr))
      tbl <- merge(tbl, vl, by.x = "code_num", by.y = "code", all.x = TRUE)
      tbl$label[is.na(tbl$label)] <- tbl$code_chr[is.na(tbl$label)]
      tbl <- tbl[order(tbl$code_num, na.last = TRUE), ]
      tbl$label <- factor(tbl$label, levels = tbl$label)

      ggplot(tbl, aes(x = label, y = n)) +
        geom_col(fill = BEMP_BLUE, width = 0.7) +
        geom_text(aes(label = n), vjust = -0.4, size = 3.5, color = "grey30") +
        labs(x = NULL, y = "Count") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        bemp_theme()

    } else {
      # Numeric: histogram
      num_data <- suppressWarnings(as.numeric(col_data))
      num_data <- num_data[!is.na(num_data)]
      if (length(num_data) == 0) return(NULL)

      ggplot(data.frame(x = num_data), aes(x = x)) +
        geom_histogram(fill = BEMP_BLUE, color = "white", bins = 30) +
        labs(x = input$vi_var, y = "Count") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        bemp_theme()
    }
  })

  output$vi_summary <- renderUI({
    req(input$vi_var)
    df <- vi_data()
    req(df)
    if (!input$vi_var %in% names(df)) return(NULL)

    col_data <- df[[input$vi_var]]
    n_total  <- length(col_data)
    n_miss   <- sum(is.na(col_data))
    n_valid  <- n_total - n_miss
    pct_miss <- round(100 * n_miss / n_total, 1)

    vl <- get_value_labels(input$vi_wave, input$vi_var)

    stats_rows <- list(
      tags$tr(tags$th("N (valid)"),  tags$td(format(n_valid,  big.mark = ","))),
      tags$tr(tags$th("Missing"),    tags$td(sprintf("%s  (%.1f%%)",
                                                     format(n_miss, big.mark = ","),
                                                     pct_miss)))
    )

    num_data <- suppressWarnings(as.numeric(col_data))
    if (is.null(vl) || nrow(vl) == 0) {
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


  # ── Tab 3: Download Assistant ──────────────────────────────────────────────

  # Populate variable selector when wave changes
  observeEvent(input$dl_wave, {
    wave_cb <- cb[cb$wave == tolower(input$dl_wave), ]
    choices <- setNames(
      wave_cb$variable_name,
      paste0(wave_cb$variable_name, ": ", wave_cb$variable_label)
    )
    updateSelectizeInput(session, "dl_vars",
                         choices  = choices,
                         selected = character(0),
                         server   = TRUE)
  })

  dl_data <- reactiveVal(NULL)

  observeEvent(input$dl_load, {
    dl_data(NULL)
    withProgress(message = paste("Loading", input$dl_wave, "..."), value = 0.5, {
      df <- tryCatch(get_wave(input$dl_wave), error = function(e) {
        showNotification("Failed to load wave data. Check your internet connection.",
                         type = "error")
        NULL
      })
    })
    dl_data(df)
  })

  dl_subset <- reactive({
    df <- dl_data()
    req(df)
    vars <- input$dl_vars
    if (length(vars) > 0) {
      vars <- vars[vars %in% names(df)]
      if (length(vars) > 0) df <- df[, vars, drop = FALSE]
    }
    df
  })

  output$dl_info <- renderUI({
    df <- dl_data()
    if (is.null(df)) {
      return(p(class = "text-muted",
               "Select a wave and click ", tags$b("Load preview"), "."))
    }
    sub_df <- dl_subset()
    tagList(
      p(class = "text-muted small",
        sprintf("Showing %s rows × %s columns  (full wave: %s × %s)",
                format(nrow(sub_df), big.mark = ","),
                ncol(sub_df),
                format(nrow(df), big.mark = ","),
                ncol(df)))
    )
  })

  output$dl_preview <- renderDT({
    req(dl_data())
    datatable(
      head(dl_subset(), 100),
      rownames = FALSE,
      style    = "bootstrap5",
      options  = list(scrollX = TRUE, pageLength = 10, dom = "tip")
    )
  })

  output$dl_download_ui <- renderUI({
    req(dl_data())
    downloadButton("dl_file", "Download CSV", class = "btn-success w-100")
  })

  output$dl_file <- downloadHandler(
    filename = function() {
      vars_tag <- if (length(input$dl_vars) > 0) "_subset" else ""
      paste0("bemp_", input$dl_wave, vars_tag, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(dl_subset(), file, row.names = FALSE)
    }
  )


  # ── Tab 4: About ──────────────────────────────────────────────────────────

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
