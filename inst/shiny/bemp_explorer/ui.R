ui <- page_navbar(
  id    = "main_navbar",
  title = "BEMP Data Explorer",
  theme = bs_theme(
    bootswatch  = "flatly",
    primary     = "#2166ac",
    font_scale  = 0.95
  ),
  fillable = FALSE,
  header = tags$head(
    tags$style(HTML(
      ".selectize-input.has-items { max-height: none !important; overflow-y: visible !important; }
       #cb_detail { scroll-margin-top: 220px; }
       .shiny-bookmark-btn { background: none !important; border: none !important;
         padding: 0.4rem 0.6rem !important; color: inherit !important; opacity: 0.7; }
       .shiny-bookmark-btn:hover { opacity: 1; }"
    )),
    tags$script(HTML("
      /* Clipboard copy with textarea fallback for non-HTTPS contexts */
      function copyToClip(text, btn) {
        function flash() {
          var orig = btn.innerHTML;
          btn.innerHTML = 'Copied!';
          setTimeout(function() { btn.innerHTML = orig; }, 1500);
        }
        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(text).then(flash).catch(function() {
            fallbackCopy(text, flash);
          });
        } else {
          fallbackCopy(text, flash);
        }
      }
      function fallbackCopy(text, cb) {
        var el = document.createElement('textarea');
        el.value = text;
        el.style.cssText = 'position:fixed;opacity:0;top:0;left:0';
        document.body.appendChild(el);
        el.focus(); el.select();
        try { document.execCommand('copy'); if (cb) cb(); } catch(e) {}
        document.body.removeChild(el);
      }
      /* Scroll to detail panel only after Shiny has actually rendered content
         (MutationObserver fires when renderUI populates #cb_detail) */
      document.addEventListener('DOMContentLoaded', function() {
        var det = document.getElementById('cb_detail');
        if (!det) return;
        new MutationObserver(function() {
          if (det.children.length > 0) {
            det.scrollIntoView({ behavior: 'smooth', block: 'start' });
          }
        }).observe(det, { childList: true });
      });
    "))
  ),

  # ── Tab 1: Variable Inspector ────────────────────────────────────────────
  nav_panel(
    title = "Variable Inspector",
    icon  = icon("chart-bar"),
    layout_sidebar(
      sidebar = sidebar(
        open  = "desktop",
        width = 280,
        checkboxInput("vi_wave_compare", "Compare waves", value = FALSE),
        hr(),
        # ── Single-wave controls ──────────────────────────────────────────
        conditionalPanel(
          condition = "!input.vi_wave_compare",
          selectInput(
            "vi_respondent_type", "Respondent Type",
            choices  = "All",
            selected = "All"
          ),
          selectInput(
            "vi_wave", "Wave",
            choices   = wave_choices,
            selected  = "w1",
            selectize = FALSE,
            size      = 20
          ),
          uiOutput("vi_group_ui")
        ),
        # ── Compare-wave controls ─────────────────────────────────────────
        conditionalPanel(
          condition = "input.vi_wave_compare",
          selectizeInput(
            "vi_waves_compare", "Waves",
            choices  = wave_choices,
            selected = NULL,
            multiple = TRUE,
            options  = list(placeholder = "Select waves to compare...")
          ),
          radioButtons(
            "vi_compare_style", "Display",
            choices = c("Facets", "Overlay"),
            selected = "Facets",
            inline  = TRUE
          )
        )
      ),

      DTOutput("vi_var_table"),
      br(),
      uiOutput("vi_title"),
      uiOutput("vi_plot_output"),
      uiOutput("vi_dl_btn"),
      br(),
      uiOutput("vi_summary")
    )
  ),

  # ── Tab 2: Codebook Browser ──────────────────────────────────────────────
  nav_panel(
    title = "Codebook Browser",
    icon  = icon("book"),
    layout_sidebar(
      sidebar = sidebar(
        open  = "desktop",
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
        downloadButton("cb_export", "Export to CSV",
                       class = "btn-outline-primary btn-sm w-100 mb-2 mt-3"),
        actionButton("cb_reset", "Reset filters", class = "btn-outline-secondary btn-sm w-100")
      ),

      # Main area: results table + detail panel
      div(class = "d-flex align-items-center gap-3 flex-wrap",
        uiOutput("cb_n_results"),
        uiOutput("cb_copy_ui")
      ),
      br(),
      DTOutput("cb_table"),
      br(),
      uiOutput("cb_detail")
    )
  ),

  # ── Tab 3: Correlation Explorer ──────────────────────────────────────────
  nav_panel(
    title = "Correlation Explorer",
    icon  = icon("circle-dot"),
    layout_sidebar(
      sidebar = sidebar(
        open  = "desktop",
        width = 300,
        selectInput(
          "ce_wave", "Wave",
          choices  = wave_choices,
          selected = "w1"
        ),
        selectizeInput(
          "ce_var_x", "X variable",
          choices = NULL,
          options = list(placeholder = "Select variable...",
                         maxOptions = 2000, searchField = c("label", "value"))
        ),
        selectizeInput(
          "ce_var_y", "Y variable",
          choices = NULL,
          options = list(placeholder = "Select variable...",
                         maxOptions = 2000, searchField = c("label", "value"))
        ),
        hr(),
        p(class = "text-muted small",
          "Negative special missing codes (e.g. \u22129, \u221255, \u221266) are excluded automatically.")
      ),
      uiOutput("ce_main")
    )
  ),

  # ── Tab 4: Panel Dynamics ────────────────────────────────────────────────
  nav_panel(
    title = "Panel Dynamics",
    icon  = icon("chart-line"),
    layout_sidebar(
      sidebar = sidebar(
        open  = "desktop",
        width = 320,
        selectizeInput(
          "pd_var", "Variable (tracked across waves)",
          choices  = NULL,
          options  = list(placeholder = "Search by label or variable name...",
                          maxOptions  = 5000,
                          searchField = "label")
        ),
        hr(),
        checkboxGroupInput(
          "pd_wave_type", "Include wave types",
          choices  = c("in-person", "phone"),
          selected = c("in-person", "phone")
        ),
        checkboxInput("pd_subgroup", "Break out by respondent type",
                      value = FALSE),
        uiOutput("pd_resp_filter_ui"),
        hr(),
        actionButton("pd_load", "Load / Update",
                     class = "btn-primary w-100"),
        br(), br(),
        uiOutput("pd_status")
      ),
      uiOutput("pd_plot_ui"),
      uiOutput("pd_dl_btn"),
      br(),
      uiOutput("pd_table_ui")
    )
  ),

  # ── Tab 5: Download Assistant ────────────────────────────────────────────
  nav_panel(
    title = "Download Assistant",
    icon  = icon("download"),
    layout_sidebar(
      sidebar = sidebar(
        open  = "desktop",
        width = 320,
        selectizeInput(
          "mw_waves", "Waves",
          choices  = wave_choices,
          selected = NULL,
          multiple = TRUE,
          options  = list(placeholder = "Select one or more waves...")
        ),
        p(class = "text-muted small mt-1",
          "Select one wave to download it as-is. Select multiple waves to merge them ",
          "on the respondent code (panel ID). Village-profile waves are skipped when merging."),
        uiOutput("mw_var_selectors"),
        hr(),
        actionButton("mw_merge", "Load / Merge", class = "btn-primary w-100"),
        br(), br(),
        uiOutput("mw_download_ui")
      ),
      uiOutput("mw_info"),
      br(),
      DTOutput("mw_preview")
    )
  ),

  # ── Tab 6: About ─────────────────────────────────────────────────────────
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
  nav_item(bookmarkButton(
    label = NULL,
    icon  = icon("bookmark"),
    title = "Bookmark / share current state"
  )),
  nav_item(input_dark_mode(id = "dark_mode")),
  nav_item(
    a(icon("github"), href = "https://github.com/janfreihardt/BEMPdata",
      target = "_blank", class = "nav-link")
  )
)
