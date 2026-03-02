ui <- page_navbar(
  title = "BEMP Data Explorer",
  theme = bs_theme(
    bootswatch  = "flatly",
    primary     = "#2166ac",
    font_scale  = 0.95
  ),
  fillable = FALSE,

  # ── Tab 1: Codebook Browser ──────────────────────────────────────────────
  nav_panel(
    title = "Codebook Browser",
    icon  = icon("book"),
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        textInput("cb_search", "Search", placeholder = "e.g. income, flood, migrat..."),
        selectizeInput(
          "cb_waves", "Waves",
          choices  = wave_choices,
          selected = wo$wave,
          multiple = TRUE,
          options  = list(placeholder = "All waves")
        ),
        selectizeInput(
          "cb_blocks", "Thematic block",
          choices  = all_blocks,
          selected = NULL,
          multiple = TRUE,
          options  = list(placeholder = "All blocks")
        ),
        hr(),
        downloadButton("cb_export", "Export to CSV",
                       class = "btn-outline-primary btn-sm w-100 mb-2"),
        actionButton("cb_reset", "Reset filters", class = "btn-outline-secondary btn-sm w-100")
      ),

      # Main area: results table + detail panel
      uiOutput("cb_n_results"),
      br(),
      DTOutput("cb_table"),
      br(),
      uiOutput("cb_detail")
    )
  ),

  # ── Tab 2: Variable Inspector ────────────────────────────────────────────
  nav_panel(
    title = "Variable Inspector",
    icon  = icon("chart-bar"),
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        selectInput(
          "vi_wave", "Wave",
          choices  = wave_choices,
          selected = "w1"
        ),
        selectizeInput(
          "vi_var", "Variable",
          choices = NULL,
          options = list(
            placeholder    = "Select a wave first...",
            maxOptions     = 500,
            searchField    = c("label", "value")
          )
        ),
        hr(),
        uiOutput("vi_meta")
      ),

      uiOutput("vi_title"),
      plotOutput("vi_plot", height = "380px"),
      uiOutput("vi_dl_btn"),
      br(),
      uiOutput("vi_summary")
    )
  ),

  # ── Tab 3: Download Assistant ────────────────────────────────────────────
  nav_panel(
    title = "Download Assistant",
    icon  = icon("download"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        selectInput(
          "dl_wave", "Wave",
          choices  = wave_choices,
          selected = "w1"
        ),
        selectizeInput(
          "dl_vars", "Variables  (leave empty for all)",
          choices  = NULL,
          multiple = TRUE,
          options  = list(
            placeholder = "All variables",
            maxOptions  = 2000,
            searchField = c("label", "value")
          )
        ),
        hr(),
        p(class = "text-muted small",
          "First download may take a few seconds while wave data is fetched from Zenodo."),
        actionButton("dl_load", "Load preview", class = "btn-primary w-100"),
        br(), br(),
        uiOutput("dl_download_ui")
      ),

      uiOutput("dl_info"),
      br(),
      DTOutput("dl_preview")
    )
  ),

  # ── Tab 4: About ─────────────────────────────────────────────────────────
  nav_panel(
    title = "About",
    icon  = icon("info-circle"),
    fluidRow(
      column(8, offset = 2,
        br(),
        h3("Bangladesh Environmental Mobility Panel (BEMP)"),
        p("The BEMP is a household panel survey conducted along the Jamuna River
          in Bangladesh tracing the impacts of riverbank erosion and flooding on
          (im)mobility, socio-economic outcomes, and political attitudes.
          The dataset follows ", strong("1,691 households"), " with a total of ",
          strong("2,170 panel respondents"), " from 2021 to 2024 across four
          annual in-person survey waves and ten bi-monthly phone survey waves,
          yielding ", strong("24,279 completed surveys"), " across 20 datasets."),
        hr(),
        h5("Data"),
        p(a("Zenodo deposit", href = "https://doi.org/10.5281/zenodo.18229498",
            target = "_blank"),
          " — CC BY 4.0"),
        h5("R package"),
        p(a("github.com/janfreihardt/BEMPdata",
            href = "https://github.com/janfreihardt/BEMPdata", target = "_blank"),
          " — ", a("DOI: 10.5281/zenodo.18775710",
                   href = "https://doi.org/10.5281/zenodo.18775710",
                   target = "_blank")),
        h5("Wave overview"),
        br(),
        DTOutput("about_waves")
      )
    )
  ),

  nav_spacer(),
  nav_item(
    a(icon("github"), href = "https://github.com/janfreihardt/BEMPdata",
      target = "_blank", class = "nav-link")
  )
)
