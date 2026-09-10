if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Package 'shiny' is required to run this application.", call. = FALSE)
}

has_dt <- requireNamespace("DT", quietly = TRUE)
has_bslib <- requireNamespace("bslib", quietly = TRUE)

# `%||%` only became part of base R in 4.4.0 and the package supports 4.1.0.
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

resolve_rehydra_namespace <- function() {
  ns <- tryCatch(asNamespace("rehydra"), error = function(e) NULL)
  if (!is.null(ns)) {
    return(ns)
  }

  # Development fallback: run app directly from source tree.
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    return(NULL)
  }

  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  roots <- unique(c(
    wd,
    normalizePath(file.path(wd, ".."), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(wd, "..", ".."), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(wd, "..", "..", ".."), winslash = "/", mustWork = FALSE)
  ))

  is_rehydra_root <- function(path) {
    desc <- file.path(path, "DESCRIPTION")
    if (!file.exists(desc)) {
      return(FALSE)
    }

    dcf <- tryCatch(read.dcf(desc), error = function(e) NULL)
    if (is.null(dcf) || nrow(dcf) == 0 || !"Package" %in% colnames(dcf)) {
      return(FALSE)
    }

    isTRUE(unname(trimws(dcf[1, "Package"])) == "rehydra")
  }

  for (root in roots) {
    if (!is_rehydra_root(root)) {
      next
    }

    loaded <- tryCatch({
      pkgload::load_all(path = root, helpers = FALSE, quiet = TRUE)
      TRUE
    }, error = function(e) FALSE)

    if (!loaded) {
      next
    }

    ns <- tryCatch(asNamespace("rehydra"), error = function(e) NULL)
    if (!is.null(ns)) {
      return(ns)
    }
  }

  NULL
}

rehydra_ns <- resolve_rehydra_namespace()
if (is.null(rehydra_ns)) {
  stop(
    paste(
      "Package 'rehydra' is required to run this application.",
      "Install it with install.packages('rehydra') or run devtools::load_all('.') before opening app.R."
    ),
    call. = FALSE
  )
}

rehydra_call <- function(fun, ...) {
  get(fun, envir = rehydra_ns, inherits = FALSE)(...)
}

# Is the column mapping describing long-format input?
#
# Written defensively because it runs before the user has touched the mapping
# selectors: with `input$value_col` still NULL, `input$value_col %in% names(dat)`
# is `logical(0)`, and `TRUE && logical(0)` is NA, which turns the `if` that
# consumes it into "missing value where TRUE/FALSE needed".
.is_long_mapping <- function(variable_col, value_col, available) {
  if (is.null(variable_col) || is.null(value_col)) {
    return(FALSE)
  }
  if (length(variable_col) != 1L || length(value_col) != 1L) {
    return(FALSE)
  }
  if (identical(variable_col, "__wide__") || identical(value_col, "__wide__")) {
    return(FALSE)
  }
  isTRUE(variable_col %in% available) && isTRUE(value_col %in% available)
}

default_theme <- if (has_bslib) {
  bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1f6f5d",
    secondary = "#5a6678"
  )
} else {
  NULL
}

app_css <- "
.rehydra-header {
  background: linear-gradient(120deg, #e8f3ee 0%, #f6f9fb 100%);
  border: 1px solid #dbe7e2;
  border-radius: 14px;
  margin-bottom: 14px;
  padding: 18px 20px;
}
.rehydra-title {
  margin: 0;
  font-weight: 700;
  color: #20434a;
}
.rehydra-subtitle {
  margin: 6px 0 0;
  color: #4f5f67;
  font-size: 14px;
}
.rehydra-panel {
  background: #ffffff;
  border: 1px solid #e2e8f0;
  border-radius: 12px;
  padding: 14px;
  margin-bottom: 12px;
  box-shadow: 0 2px 10px rgba(22, 39, 60, 0.05);
}
.rehydra-panel h4 {
  margin-top: 0;
  color: #294a57;
}
.rehydra-section-label {
  margin-top: 8px;
  margin-bottom: 8px;
  font-size: 13px;
  font-weight: 700;
  letter-spacing: 0.2px;
  color: #3c5466;
  text-transform: uppercase;
}
.rehydra-status {
  margin-bottom: 10px;
  border-radius: 10px;
  padding: 10px 12px;
}
.rehydra-status.ready {
  background: #f5f7fa;
  border: 1px solid #d9e2ec;
  color: #425466;
}
.rehydra-status.ok {
  background: #ecf8f3;
  border: 1px solid #c9ebdc;
  color: #205846;
}
.rehydra-status.error {
  background: #fff2f2;
  border: 1px solid #f3c7c7;
  color: #8b2f2f;
}
.rehydra-mono {
  font-family: Consolas, 'Segoe UI Mono', 'Courier New', monospace;
  font-size: 12px;
}

/* Flexbox layout instead of fixed Bootstrap columns: when the sidebar is
   hidden, Shiny sets display:none on its conditionalPanel and the main area
   reflows to the full width on its own, with no JavaScript involved. */
.rehydra-layout {
  display: flex;
  align-items: flex-start;
  gap: 14px;
}
.rehydra-sidebar {
  flex: 0 0 330px;
  max-width: 330px;
}
.rehydra-main {
  flex: 1 1 auto;
  min-width: 0;
}
@media (max-width: 900px) {
  .rehydra-layout { flex-direction: column; }
  .rehydra-sidebar { flex: 1 1 auto; max-width: 100%; }
}

.rehydra-toolbar {
  display: flex;
  align-items: center;
  gap: 10px;
  margin-bottom: 10px;
}

/* Compact summary tiles for the numbers worth seeing without opening a table. */
.rehydra-boxes {
  display: flex;
  flex-wrap: wrap;
  gap: 10px;
  margin-bottom: 12px;
}
.rehydra-box {
  flex: 1 1 130px;
  background: #ffffff;
  border: 1px solid #e2e8f0;
  border-radius: 12px;
  padding: 10px 12px;
  box-shadow: 0 2px 10px rgba(22, 39, 60, 0.05);
}
.rehydra-box .value {
  font-size: 22px;
  font-weight: 700;
  color: #1f6f5d;
  line-height: 1.1;
}
.rehydra-box .label {
  font-size: 11px;
  text-transform: uppercase;
  letter-spacing: 0.3px;
  color: #64748b;
  margin-top: 2px;
}
.rehydra-box.warn .value { color: #b45309; }

/* Nested tab strips: make the inner pills visually secondary to the sections. */
.rehydra-main .nav-pills > li > a,
.rehydra-main .nav-pills .nav-link {
  font-size: 13px;
  padding: 4px 10px;
}
.rehydra-main .tab-content { padding-top: 10px; }
"

ui <- shiny::fluidPage(
  theme = default_theme,
  shiny::tags$head(shiny::tags$style(shiny::HTML(app_css))),
  shiny::div(
    class = "rehydra-header",
    shiny::h2("rehydra", class = "rehydra-title"),
    shiny::p(
      "Professional workflow for drought-rewatering segmentation, resilience indices, and memory analysis.",
      class = "rehydra-subtitle"
    )
  ),
  shiny::div(
    class = "rehydra-toolbar",
    shiny::checkboxInput("show_controls", "Show controls", value = TRUE, width = "160px")
  ),
  shiny::div(
    class = "rehydra-layout",
    shiny::conditionalPanel(
      condition = "input.show_controls",
      class = "rehydra-sidebar",
      shiny::div(
        class = "rehydra-panel",
        shiny::h4("Data and Mapping"),
        shiny::fileInput("file_csv", "Upload CSV file", accept = c(".csv")),
        shiny::checkboxInput("use_example", "Use bundled example data", value = TRUE),
        shiny::actionButton("run_demo", "Load bundled demo", class = "btn-default"),
        shiny::helpText("The bundled example includes two full drought-rewatering cycles."),
        shiny::tags$div(class = "rehydra-section-label", "Column mapping"),
        shiny::selectInput("time_col", "Time column", choices = character()),
        shiny::selectInput("genotype_col", "Genotype column", choices = character()),
        shiny::selectInput("treatment_col", "Treatment column", choices = character()),
        shiny::selectInput("replicate_col", "Replicate column", choices = character()),
        shiny::selectInput("variable_col", "Variable column (long data)", choices = character()),
        shiny::selectInput("value_col", "Value column (long data)", choices = character()),
        shiny::selectizeInput("physiological_vars", "Physiological variables", choices = NULL, multiple = TRUE),
        shiny::selectInput(
          "response_direction",
          "Response direction",
          choices = c("higher_is_better", "lower_is_better", "more_negative_is_worse", "auto"),
          selected = "auto"
        ),
        shiny::selectInput(
          "transform_mode",
          "Transformation",
          choices = c("none", "absolute", "invert", "stress_score", "relative_to_control"),
          selected = "none"
        )
      ),
      shiny::div(
        class = "rehydra-panel",
        shiny::h4("Segmentation"),
        shiny::radioButtons(
          "seg_method",
          "Segmentation method",
          choices = c("Automatic" = "auto", "Manual" = "manual"),
          selected = "auto",
          inline = TRUE
        ),
        shiny::checkboxInput("smoothing", "Use smoothing for automatic segmentation", value = TRUE),
        shiny::sliderInput("smooth_span", "Smoothing span", min = 0.10, max = 0.80, value = 0.25, step = 0.05),
        shiny::numericInput("min_drop", "Minimum relative drop", value = 0.15, min = 0, max = 1, step = 0.01),
        shiny::numericInput("min_recovery", "Minimum relative recovery", value = 0.10, min = 0, max = 1, step = 0.01),
        shiny::checkboxInput("set_cycles", "Set expected number of cycles", value = FALSE),
        shiny::conditionalPanel(
          condition = "input.set_cycles === true",
          shiny::numericInput("n_cycles", "Expected cycles", value = 2, min = 1, max = 10, step = 1)
        ),
        shiny::conditionalPanel(
          condition = "input.seg_method == 'manual'",
          shiny::tags$div(class = "rehydra-section-label", "Manual windows"),
          shiny::sliderInput("c1_drought", "Cycle 1 drought window", min = 0, max = 40, value = c(5, 10), step = 1),
          shiny::sliderInput("c1_rewater", "Cycle 1 rewatering window", min = 0, max = 40, value = c(10, 15), step = 1),
          shiny::sliderInput("c2_drought", "Cycle 2 drought window", min = 0, max = 40, value = c(23, 28), step = 1),
          shiny::sliderInput("c2_rewater", "Cycle 2 rewatering window", min = 0, max = 40, value = c(28, 34), step = 1)
        ),
        shiny::actionButton("run_analysis", "Run analysis", class = "btn-primary")
      )
    ),
    shiny::div(
      class = "rehydra-main",
      shiny::uiOutput("run_status"),
      shiny::uiOutput("summary_boxes"),
      shiny::tabsetPanel(
        id = "main_tabs",
        shiny::tabPanel(
          "Data",
          shiny::tabsetPanel(
            id = "data_tabs",
            type = "pills",
            shiny::tabPanel(
              "Data",
              shiny::div(class = "rehydra-panel", shiny::h4("Data preview"), if (has_dt) DT::DTOutput("data_preview") else shiny::tableOutput("data_preview")),
              shiny::div(class = "rehydra-panel", shiny::h4("Data quality checks"), if (has_dt) DT::DTOutput("checks_table") else shiny::tableOutput("checks_table"))
            ),
            shiny::tabPanel(
              "Segmentation",
              shiny::div(class = "rehydra-panel", shiny::plotOutput("segmentation_plot", height = "430px")),
              shiny::div(class = "rehydra-panel", if (has_dt) DT::DTOutput("segmentation_table") else shiny::tableOutput("segmentation_table"))
            )
          )
        ),
        shiny::tabPanel(
          "Dynamics",
          shiny::tabsetPanel(
            id = "dynamics_tabs",
            type = "pills",
            shiny::tabPanel(
              "Trajectory",
              shiny::div(class = "rehydra-panel", shiny::plotOutput("trajectory_plot", height = "520px"))
            ),
            shiny::tabPanel(
              "Deficit curve (AUC)",
              shiny::div(
                class = "rehydra-panel",
                shiny::h4("Full time series with the deficit area highlighted"),
                shiny::p(
                  class = "rehydra-subtitle",
                  paste(
                    "The shaded band is the integral of max(0, baseline - response):",
                    "the area under the deficit curve. The cross marks the trough and",
                    "the open circle the time to half recovery (t50)."
                  )
                ),
                shiny::fluidRow(
                  shiny::column(4, shiny::selectInput("auc_variable", "Variable", choices = character())),
                  shiny::column(4, shiny::selectInput("auc_treatment", "Treatment", choices = character())),
                  shiny::column(4, shiny::selectInput("auc_replicate", "Replicate", choices = character()))
                ),
                shiny::plotOutput("deficit_plot", height = "480px")
              ),
              shiny::div(
                class = "rehydra-panel",
                shiny::h4("Curve-shape metrics"),
                shiny::plotOutput("curve_shape_plot", height = "430px")
              ),
              shiny::div(
                class = "rehydra-panel",
                shiny::h4("AUC, t50, rate constant and latency"),
                if (has_dt) DT::DTOutput("curve_shape_table") else shiny::tableOutput("curve_shape_table")
              )
            )
          )
        ),
        shiny::tabPanel(
          "Indices",
          shiny::tabsetPanel(
            id = "indices_tabs",
            type = "pills",
            shiny::tabPanel(
              "Damage and Recovery",
              shiny::div(class = "rehydra-panel", shiny::plotOutput("damage_plot", height = "430px")),
              shiny::div(class = "rehydra-panel", shiny::h4("Damage metrics"), if (has_dt) DT::DTOutput("damage_table") else shiny::tableOutput("damage_table")),
              shiny::div(class = "rehydra-panel", shiny::h4("Recovery metrics"), if (has_dt) DT::DTOutput("recovery_table") else shiny::tableOutput("recovery_table"))
            ),
            shiny::tabPanel(
              "Extended indices",
              shiny::div(
                class = "rehydra-panel",
                shiny::h4("Period and reduction components"),
                shiny::plotOutput("extended_plot", height = "440px")
              ),
              shiny::div(
                class = "rehydra-panel",
                shiny::h4("Recovery period and total reduction"),
                shiny::p(
                  class = "rehydra-subtitle",
                  paste(
                    "recovery_censored = TRUE means the series ended before the response",
                    "returned to the recovery threshold, so recovery_period is not",
                    "estimable and is reported as NA rather than truncated."
                  )
                ),
                if (has_dt) DT::DTOutput("period_table") else shiny::tableOutput("period_table")
              ),
              shiny::div(
                class = "rehydra-panel",
                shiny::h4("Mean reduction and mean recovery rate"),
                if (has_dt) DT::DTOutput("reduction_table") else shiny::tableOutput("reduction_table")
              )
            ),
            shiny::tabPanel(
              "Resilience",
              shiny::div(class = "rehydra-panel", shiny::plotOutput("resilience_plot", height = "430px")),
              shiny::div(class = "rehydra-panel", if (has_dt) DT::DTOutput("resilience_table") else shiny::tableOutput("resilience_table"))
            ),
            shiny::tabPanel(
              "Stability",
              shiny::div(class = "rehydra-panel", shiny::plotOutput("stability_plot", height = "430px")),
              shiny::div(class = "rehydra-panel", if (has_dt) DT::DTOutput("stability_table") else shiny::tableOutput("stability_table"))
            )
          )
        ),
        shiny::tabPanel(
          "Memory",
          shiny::tabsetPanel(
            id = "memory_tabs",
            type = "pills",
            shiny::tabPanel(
              "Memory",
              shiny::div(class = "rehydra-panel", shiny::plotOutput("memory_plot", height = "430px")),
              shiny::div(class = "rehydra-panel", if (has_dt) DT::DTOutput("memory_table") else shiny::tableOutput("memory_table"))
            ),
            shiny::tabPanel(
              "Multi-cycle memory",
              shiny::div(
                class = "rehydra-panel",
                shiny::h4("Index trajectory across cycles"),
                shiny::p(
                  class = "rehydra-subtitle",
                  paste(
                    "The slope of the fitted line is the average change in the index per",
                    "additional drought cycle. Read it together with monotonic_fraction:",
                    "a significant slope with a fraction near 0.5 is driven by the",
                    "endpoints, not by a consistent progression."
                  )
                ),
                shiny::selectInput(
                  "trend_family",
                  "Index family",
                  choices = c(
                    "Resilience (Rt, Rc, Rs)" = "resilience",
                    "Curve shape (AUC, t50, k)" = "shape"
                  ),
                  selected = "resilience"
                ),
                shiny::plotOutput("memory_trend_plot", height = "460px")
              ),
              shiny::div(
                class = "rehydra-panel",
                shiny::h4("Memory trend (slope per cycle)"),
                if (has_dt) DT::DTOutput("memory_trend_table") else shiny::tableOutput("memory_trend_table")
              ),
              shiny::div(
                class = "rehydra-panel",
                shiny::h4("Consecutive-cycle contrasts"),
                if (has_dt) DT::DTOutput("memory_pairs_table") else shiny::tableOutput("memory_pairs_table")
              )
            )
          )
        ),
        shiny::tabPanel(
          "Statistics",
          shiny::tabsetPanel(
            id = "statistics_tabs",
            type = "pills",
            shiny::tabPanel(
              "Designed analysis",
              shiny::div(
                class = "rehydra-panel",
                shiny::h4("Analysis of variance on the indices"),
                shiny::fluidRow(
                  shiny::column(4, shiny::selectInput("anova_response", "Response index", choices = character())),
                  shiny::column(4, shiny::selectInput(
                    "anova_source", "Index table",
                    choices = c(
                      "Resilience" = "resilience",
                      "Curve shape" = "curve_shape",
                      "Recovery period" = "recovery_period",
                      "Mean reduction" = "mean_reduction"
                    ),
                    selected = "resilience"
                  )),
                  shiny::column(4, shiny::checkboxInput("anova_interaction", "Include interaction", value = TRUE))
                ),
                shiny::helpText(
                  paste(
                    "One model is fitted per variable and per cycle. Cycles are analysed",
                    "separately because repeated cycles on the same plants are not",
                    "independent; the change between cycles belongs to the multi-cycle",
                    "memory tab."
                  )
                )
              ),
              shiny::div(
                class = "rehydra-panel",
                shiny::h4("ANOVA table"),
                if (has_dt) DT::DTOutput("anova_table") else shiny::tableOutput("anova_table")
              ),
              shiny::div(
                class = "rehydra-panel",
                shiny::h4("Marginal means"),
                if (has_dt) DT::DTOutput("anova_means") else shiny::tableOutput("anova_means")
              ),
              shiny::div(
                class = "rehydra-panel",
                shiny::h4("Pairwise comparisons (Tukey)"),
                if (has_dt) DT::DTOutput("anova_comparisons") else shiny::tableOutput("anova_comparisons")
              ),
              shiny::div(
                class = "rehydra-panel",
                shiny::h4("Model diagnostics"),
                if (has_dt) DT::DTOutput("anova_diagnostics") else shiny::tableOutput("anova_diagnostics")
              )
            ),
            shiny::tabPanel(
              "Genotype comparison",
              shiny::div(class = "rehydra-panel", if (has_dt) DT::DTOutput("comparison_table") else shiny::tableOutput("comparison_table"))
            )
          )
        ),
        shiny::tabPanel(
          "Export",
          shiny::div(
            class = "rehydra-panel",
            shiny::p("Download merged tables, trajectory plot, and the automated HTML report."),
            shiny::downloadButton("download_csv", "Download CSV results"),
            shiny::downloadButton("download_plot", "Download PNG plot"),
            shiny::downloadButton("download_report", "Download HTML report")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  force_demo <- shiny::reactiveVal(FALSE)
  run_token <- shiny::reactiveVal(0L)
  last_error <- shiny::reactiveVal(NULL)

  # Index tables carry double-precision columns that are unreadable at full
  # width; rounding to a fixed number of significant digits keeps them scannable
  # without hiding order-of-magnitude differences (signif, not round, so a rate
  # constant of 0.00042 does not collapse to 0).
  round_display <- function(x, digits = 4) {
    if (!is.data.frame(x) || nrow(x) == 0) {
      return(x)
    }
    num <- vapply(x, is.numeric, logical(1))
    # Integer-valued columns (cycle, n, counts) are left alone.
    num <- num & !vapply(x, function(col) {
      is.integer(col) || (is.numeric(col) && all(col == round(col), na.rm = TRUE))
    }, logical(1))
    x[num] <- lapply(x[num], function(col) signif(col, digits))
    x
  }

  # Flags a reader must not miss: a censored recovery is not a fast one, and a
  # cycle with no detected stress is not a resilient one.
  flag_cols <- c("recovery_censored", "stress_detected", "overcompensation")

  render_table <- function(expr, digits = 4) {
    # The table expression is captured unevaluated and re-evaluated inside each
    # render. Referring to the `expr` argument directly from within the render
    # block would instead force a promise defined outside it, and every re-render
    # would restart that promise - the source of a stream of
    # "restarting interrupted promise evaluation" warnings.
    expr_quoted <- substitute(expr)
    expr_env <- parent.frame()
    evaluate <- function() eval(expr_quoted, expr_env)

    if (has_dt) {
      DT::renderDT(
        {
          tbl <- round_display(evaluate(), digits)
          present <- intersect(flag_cols, names(tbl))
          dt <- DT::datatable(
            tbl,
            options = list(pageLength = 8, scrollX = TRUE),
            rownames = FALSE
          )
          for (fc in present) {
            dt <- DT::formatStyle(
              dt, fc,
              backgroundColor = DT::styleEqual(
                c(TRUE, FALSE),
                if (fc == "stress_detected") c("#ecf8f3", "#fff7ed")
                else c("#fff7ed", "#ecf8f3")
              )
            )
          }
          dt
        },
        server = TRUE
      )
    } else {
      shiny::renderTable(round_display(evaluate(), digits))
    }
  }

  choose_default <- function(choices, preferred, fallback = NULL) {
    for (nm in preferred) {
      if (nm %in% choices) {
        return(nm)
      }
    }
    if (!is.null(fallback) && fallback %in% choices) {
      return(fallback)
    }
    if (length(choices) > 0) {
      return(choices[[1]])
    }
    NULL
  }

  resolve_example_path <- function() {
    pkg_path <- system.file("extdata", "rehydra_example.csv", package = "rehydra")
    if (nzchar(pkg_path) && file.exists(pkg_path)) {
      return(pkg_path)
    }

    candidates <- c(
      file.path(getwd(), "inst", "extdata", "rehydra_example.csv"),
      file.path(getwd(), "..", "extdata", "rehydra_example.csv"),
      file.path("inst", "extdata", "rehydra_example.csv"),
      file.path("..", "extdata", "rehydra_example.csv")
    )

    for (candidate in candidates) {
      full <- normalizePath(candidate, winslash = "/", mustWork = FALSE)
      if (file.exists(full)) {
        return(full)
      }
    }

    ""
  }

  raw_data <- shiny::reactive({
    if (isTRUE(force_demo()) || isTRUE(input$use_example)) {
      path <- resolve_example_path()
      if (!nzchar(path) || !file.exists(path)) {
        return(NULL)
      }
      return(readr::read_csv(path, show_col_types = FALSE))
    }

    shiny::req(input$file_csv)
    readr::read_csv(input$file_csv$datapath, show_col_types = FALSE)
  })

  shiny::observeEvent(input$run_analysis, {
    force_demo(FALSE)
    run_token(run_token() + 1L)
  })

  shiny::observeEvent(input$run_demo, {
    force_demo(TRUE)
    shiny::updateCheckboxInput(session, "use_example", value = TRUE)
    shiny::updateSelectInput(session, "response_direction", selected = "auto")
    shiny::updateSelectInput(session, "transform_mode", selected = "none")
    shiny::updateRadioButtons(session, "seg_method", selected = "auto")
    shiny::updateCheckboxInput(session, "smoothing", value = TRUE)
    shiny::updateSliderInput(session, "smooth_span", value = 0.25)
    shiny::updateNumericInput(session, "min_drop", value = 0.15)
    shiny::updateNumericInput(session, "min_recovery", value = 0.10)
    run_token(run_token() + 1L)
  })

  shiny::observe({
    dat <- raw_data()
    if (is.null(dat) || nrow(dat) == 0) {
      return()
    }

    cols <- names(dat)
    long_choices <- c("<wide_data>" = "__wide__", cols)
    rep_choices <- c("<auto_replicate>" = "__auto__", cols)

    shiny::updateSelectInput(
      session,
      "time_col",
      choices = cols,
      selected = choose_default(cols, c("time_days", "time"))
    )
    shiny::updateSelectInput(
      session,
      "genotype_col",
      choices = cols,
      selected = choose_default(cols, c("genotype"))
    )
    shiny::updateSelectInput(
      session,
      "treatment_col",
      choices = cols,
      selected = choose_default(cols, c("treatment"))
    )
    shiny::updateSelectInput(
      session,
      "replicate_col",
      choices = rep_choices,
      selected = choose_default(cols, c("replicate"), fallback = "__auto__")
    )
    shiny::updateSelectInput(
      session,
      "variable_col",
      choices = long_choices,
      selected = if ("variable" %in% cols) "variable" else "__wide__"
    )
    shiny::updateSelectInput(
      session,
      "value_col",
      choices = long_choices,
      selected = if ("value" %in% cols) "value" else "__wide__"
    )

    if (all(c("variable", "value") %in% cols)) {
      vars <- sort(unique(as.character(dat$variable)))
      shiny::updateSelectizeInput(session, "physiological_vars", choices = vars, selected = vars)
    } else {
      id_cols <- c("plant_id", "genotype", "time_days", "time", "treatment", "replicate", "cycle", "phase", "block", "plot_id", "pot_id")
      metric_candidates <- setdiff(cols, id_cols)
      metric_candidates <- metric_candidates[vapply(dat[metric_candidates], is.numeric, logical(1))]
      shiny::updateSelectizeInput(
        session,
        "physiological_vars",
        choices = metric_candidates,
        selected = metric_candidates
      )
    }

    t_col <- choose_default(cols, c("time_days", "time"), fallback = cols[[1]])
    tt <- suppressWarnings(as.numeric(dat[[t_col]]))
    tt <- tt[is.finite(tt)]
    if (length(tt) > 0) {
      lo <- floor(min(tt))
      hi <- ceiling(max(tt))
      pad <- max(1, floor((hi - lo) * 0.08))
      shiny::updateSliderInput(session, "c1_drought", min = lo, max = hi, value = c(lo + pad, lo + 2 * pad))
      shiny::updateSliderInput(session, "c1_rewater", min = lo, max = hi, value = c(lo + 2 * pad, lo + 3 * pad))
      shiny::updateSliderInput(session, "c2_drought", min = lo, max = hi, value = c(lo + 5 * pad, lo + 6 * pad))
      shiny::updateSliderInput(session, "c2_rewater", min = lo, max = hi, value = c(lo + 6 * pad, lo + 7 * pad))
    }
  })

  output$data_preview <- render_table({
    shiny::req(raw_data())
    head(raw_data(), 25)
  })

  selected_data <- shiny::reactive({
    dat <- raw_data()
    shiny::req(dat)

    is_long <- .is_long_mapping(input$variable_col, input$value_col, names(dat))

    if (is_long) {
      if (length(input$physiological_vars) > 0) {
        dat <- dat[as.character(dat[[input$variable_col]]) %in% input$physiological_vars, , drop = FALSE]
      }
      return(dat)
    }

    keep <- unique(c(
      "plant_id",
      input$genotype_col,
      input$time_col,
      input$treatment_col,
      if (!identical(input$replicate_col, "__auto__")) input$replicate_col else NULL,
      "cycle",
      "phase",
      input$physiological_vars
    ))

    keep <- keep[keep %in% names(dat)]
    dat[, keep, drop = FALSE]
  })

  build_manual_events <- function(prepared, input) {
    t_min <- min(prepared$time, na.rm = TRUE)
    t_max <- max(prepared$time, na.rm = TRUE)

    windows <- list(input$c1_drought, input$c1_rewater, input$c2_drought, input$c2_rewater)
    valid_windows <- all(vapply(
      windows,
      function(w) length(w) == 2 && all(is.finite(w)) && w[[1]] <= w[[2]],
      logical(1)
    ))
    if (!valid_windows) {
      stop("Manual windows must have finite start/end values with start <= end.")
    }

    c1d <- input$c1_drought
    c1r <- input$c1_rewater
    c2d <- input$c2_drought
    c2r <- input$c2_rewater

    if (c1d[[2]] > c1r[[1]] || c1r[[2]] > c2d[[1]] || c2d[[2]] > c2r[[1]]) {
      stop("Manual windows must follow chronological order: C1 drought, C1 rewatering, C2 drought, C2 rewatering.")
    }

    tibble::tibble(
      cycle = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
      phase = c(
        "PreDrought", "Drought", "Rewatering", "PostDrought",
        "PreDrought", "Drought", "Rewatering", "PostDrought"
      ),
      start = c(t_min, c1d[[1]], c1r[[1]], c1r[[2]], c1r[[2]], c2d[[1]], c2r[[1]], c2r[[2]]),
      end = c(c1d[[1]], c1d[[2]], c1r[[2]], c2d[[1]], c2d[[1]], c2d[[2]], c2r[[2]], t_max)
    )
  }

  analysis <- shiny::eventReactive(run_token(), ignoreInit = TRUE, {
    dat <- selected_data()
    shiny::req(dat)

    # The full pipeline takes a few seconds on a real dataset; without this the
    # app looks frozen and users click Run again.
    progress <- shiny::Progress$new(session, min = 0, max = 1)
    on.exit(progress$close(), add = TRUE)
    progress$set(message = "Running rehydra analysis", value = 0)
    step <- function(text, value) progress$set(detail = text, value = value)

    result <- tryCatch({
      required_mapping <- c(input$time_col, input$genotype_col, input$treatment_col)
      missing_mapping <- setdiff(required_mapping, names(dat))
      if (length(missing_mapping) > 0) {
        stop(paste0("Missing mapped columns: ", paste(missing_mapping, collapse = ", "), "."))
      }

      replicate_col <- input$replicate_col
      if (identical(replicate_col, "__auto__") || !replicate_col %in% names(dat)) {
        dat$..replicate_auto <- "1"
        replicate_col <- "..replicate_auto"
      }

      is_long <- .is_long_mapping(input$variable_col, input$value_col, names(dat))

      if (is_long) {
        prepared <- rehydra_call("prepare_rehydra_data",
          data = dat,
          time = input$time_col,
          genotype = input$genotype_col,
          treatment = input$treatment_col,
          replicate = replicate_col,
          variable = input$variable_col,
          value = input$value_col,
          cycle = if ("cycle" %in% names(dat)) "cycle" else NULL,
          phase = if ("phase" %in% names(dat)) "phase" else NULL,
          response_direction = input$response_direction,
          transform = input$transform_mode
        )
      } else {
        vars <- input$physiological_vars[input$physiological_vars %in% names(dat)]
        if (length(vars) == 0) {
          stop("Select at least one physiological variable for wide data.")
        }

        prepared <- rehydra_call("prepare_rehydra_data",
          data = dat,
          time = input$time_col,
          genotype = input$genotype_col,
          treatment = input$treatment_col,
          replicate = replicate_col,
          variables = vars,
          cycle = if ("cycle" %in% names(dat)) "cycle" else NULL,
          phase = if ("phase" %in% names(dat)) "phase" else NULL,
          response_direction = input$response_direction,
          transform = input$transform_mode
        )
      }

      if (nrow(prepared) == 0) {
        stop("No rows remained after preparation. Review column mapping and variable filters.")
      }

      events <- NULL
      if (identical(input$seg_method, "manual")) {
        events <- build_manual_events(prepared, input)
      }

      step("segmenting cycles", 0.2)
      segments <- rehydra_call("segment_drought_cycle",
        data = prepared,
        time = time,
        value = transformed_value,
        group_by = c("genotype", "treatment", "replicate", "plant_id", "variable"),
        method = input$seg_method,
        events = events,
        smoothing = isTRUE(input$smoothing),
        smooth_span = input$smooth_span,
        min_drop = input$min_drop,
        min_recovery = input$min_recovery,
        n_cycles = if (isTRUE(input$set_cycles)) as.integer(input$n_cycles) else NULL,
        response_direction = input$response_direction,
        fallback_manual = TRUE
      )

      step("damage and recovery metrics", 0.4)
      checks <- rehydra_call("check_rehydra_data",prepared)
      damage <- rehydra_call("damage_metrics",segments)
      recovery <- rehydra_call("recovery_metrics",segments)
      step("extended indices and curve shape", 0.55)
      recovery_period <- rehydra_call("recovery_period_metrics", segments)
      mean_reduction <- rehydra_call("mean_reduction_metrics", segments)
      curve_shape <- rehydra_call("curve_shape_metrics", segments)
      step("resilience and memory", 0.75)
      resilience <- rehydra_call("resilience_index",segments)
      memory <- rehydra_call("memory_effect",
        resilience_results = resilience,
        damage_results = damage,
        recovery_results = recovery
      )

      # Consecutive-cycle contrasts: every cycle against the one before it,
      # which is what the multi-cycle panel needs when more than two cycles were
      # segmented.
      memory_pairs <- tryCatch(
        rehydra_call("memory_effect",
          resilience_results = resilience,
          damage_results = damage,
          recovery_results = recovery,
          cycle_test = "previous"
        ),
        error = function(e) tibble::tibble()
      )

      trend <- tryCatch(
        rehydra_call("memory_trend", resilience, min_cycles = 2),
        error = function(e) tibble::tibble()
      )

      shape_trend <- tryCatch(
        rehydra_call("memory_trend", curve_shape,
          metrics = c("deficit_auc_normalized", "t50", "recovery_rate_k",
                      "residual_deficit"),
          min_cycles = 2
        ),
        error = function(e) tibble::tibble()
      )

      step("classification and comparisons", 0.9)
      classification <- rehydra_call("priming_classification",memory)
      stability <- rehydra_call("stability_index",segments)
      comparison <- rehydra_call("compare_genotypes",resilience, metric = "Rs")

      analysis_obj <- structure(
        list(
          data = prepared,
          checks = checks,
          segments = segments,
          damage = damage,
          recovery = recovery,
          recovery_period = recovery_period,
          mean_reduction = mean_reduction,
          curve_shape = curve_shape,
          resilience = resilience,
          memory = memory,
          memory_trend = trend,
          stability = stability,
          classification = classification,
          genotype_comparison = comparison,
          parameters = list(
            segmentation_method = input$seg_method,
            smoothing = isTRUE(input$smoothing),
            smooth_span = input$smooth_span,
            min_drop = input$min_drop,
            min_recovery = input$min_recovery
          ),
          references = c(
            "Lloret et al. (2011) doi:10.1111/j.1600-0706.2011.19372.x",
            "Xu et al. (2010) doi:10.4161/psb.5.6.11398",
            "Ribeiro et al. (2021) doi:10.1016/j.jplph.2021.153397"
          )
        ),
        class = "rehydra_analysis"
      )

      last_error(NULL)

      list(
        prepared = prepared,
        checks = checks,
        segments = segments,
        damage = damage,
        recovery = recovery,
        recovery_period = recovery_period,
        mean_reduction = mean_reduction,
        curve_shape = curve_shape,
        resilience = resilience,
        memory = memory,
        memory_pairs = memory_pairs,
        trend = trend,
        shape_trend = shape_trend,
        classification = classification,
        stability = stability,
        comparison = comparison,
        report_object = analysis_obj
      )
    }, error = function(e) {
      msg <- conditionMessage(e)
      last_error(msg)
      shiny::showNotification(paste("Analysis failed:", msg), type = "error", duration = NULL)
      NULL
    })

    result
  })

  output$run_status <- shiny::renderUI({
    using_example <- isTRUE(force_demo()) || isTRUE(input$use_example)
    if (using_example && is.null(raw_data())) {
      return(
        shiny::div(
          class = "rehydra-status error",
          "Bundled example data was not found. Upload a CSV file or run the app from an installed package."
        )
      )
    }

    if (run_token() == 0) {
      return(shiny::div(class = "rehydra-status ready", "Ready. Configure columns and click 'Run analysis'."))
    }

    err <- last_error()
    if (!is.null(err)) {
      return(shiny::div(class = "rehydra-status error", paste("Last run failed:", err)))
    }

    res <- analysis()
    if (is.null(res)) {
      return(shiny::div(class = "rehydra-status ready", "Waiting for a successful run."))
    }

    shiny::div(
      class = "rehydra-status ok",
      shiny::strong("Analysis completed successfully."),
      shiny::tags$div(
        class = "rehydra-mono",
        paste0(
          "Rows: ", nrow(res$prepared),
          " | Segments: ", nrow(res$segments),
          " | Resilience rows: ", nrow(res$resilience),
          " | Memory rows: ", nrow(res$memory)
        )
      )
    )
  })

  # The handful of numbers a reader would otherwise have to open three tables to
  # find. Censored recoveries get their own tile because they are the easiest
  # result to misread: they are missing, not fast.
  output$summary_boxes <- shiny::renderUI({
    if (run_token() == 0) {
      return(NULL)
    }

    res <- analysis()
    if (is.null(res)) {
      return(NULL)
    }

    box <- function(value, label, warn = FALSE) {
      shiny::div(
        class = if (warn) "rehydra-box warn" else "rehydra-box",
        shiny::div(class = "value", value),
        shiny::div(class = "label", label)
      )
    }

    seg <- res$segments
    n_cycles <- dplyr::n_distinct(seg$cycle[!is.na(seg$cycle)])
    n_groups <- nrow(res$resilience)

    censored <- res$recovery_period$recovery_censored
    pct_censored <- if (length(censored) > 0 && any(!is.na(censored))) {
      round(100 * mean(censored, na.rm = TRUE))
    } else {
      NA_real_
    }

    detected <- res$recovery_period$stress_detected
    n_detected <- sum(detected %in% TRUE)

    conf <- suppressWarnings(mean(seg$confidence, na.rm = TRUE))

    shiny::div(
      class = "rehydra-boxes",
      box(n_cycles, "cycles segmented"),
      box(n_groups, "group-cycle rows"),
      box(n_detected, "cycles with stress"),
      box(
        if (is.na(pct_censored)) "-" else paste0(pct_censored, "%"),
        "recoveries censored",
        warn = isTRUE(pct_censored > 25)
      ),
      box(
        if (is.finite(conf)) sprintf("%.2f", conf) else "-",
        "mean segmentation confidence",
        warn = isTRUE(conf < 0.6)
      )
    )
  })

  output$checks_table <- render_table({
    res <- analysis()
    if (is.null(res)) {
      tibble::tibble()
    } else {
      res$checks
    }
  })

  output$segmentation_plot <- shiny::renderPlot({
    shiny::req(analysis())
    rehydra_call("plot_recovery_trajectory",analysis()$segments, show_mean = FALSE)
  })

  output$segmentation_table <- render_table({
    shiny::req(analysis())
    dplyr::select(analysis()$segments, dplyr::any_of(c("genotype", "treatment", "replicate", "variable", "time", "cycle", "phase", "confidence", "method")))
  })

  output$trajectory_plot <- shiny::renderPlot({
    shiny::req(analysis())
    rehydra_call("plot_recovery_trajectory",
      data = analysis()$segments,
      resilience_results = analysis()$resilience,
      annotate_indices = TRUE
    )
  })

  output$damage_plot <- shiny::renderPlot({
    shiny::req(analysis())
    rehydra_call("plot_damage_recovery",analysis()$damage, analysis()$recovery)
  })

  output$damage_table <- render_table({
    shiny::req(analysis())
    analysis()$damage
  })

  output$recovery_table <- render_table({
    shiny::req(analysis())
    analysis()$recovery
  })

  # ---- deficit curve / AUC ---------------------------------------------------

  # The AUC panel draws one series at a time: overlaying every plant would turn
  # the shaded area into an unreadable blob, and the area is a per-series
  # quantity anyway.
  shiny::observeEvent(analysis(), {
    res <- analysis()
    if (is.null(res)) {
      return(invisible(NULL))
    }

    seg <- res$segments
    update_choices <- function(id, values, prefer = NULL) {
      values <- sort(unique(as.character(values[!is.na(values)])))
      if (length(values) == 0) {
        return(invisible(NULL))
      }
      selected <- if (!is.null(prefer) && any(prefer %in% values)) {
        prefer[prefer %in% values][[1]]
      } else {
        values[[1]]
      }
      shiny::updateSelectInput(session, id, choices = values, selected = selected)
    }

    update_choices("auc_variable", seg$variable)
    update_choices("auc_treatment", seg$treatment, prefer = c("Drought", "drought"))
    update_choices("auc_replicate", seg$replicate)

    numeric_indices <- names(res$resilience)[vapply(res$resilience, is.numeric, logical(1))]
    numeric_indices <- setdiff(numeric_indices, "cycle")
    if (length(numeric_indices) > 0) {
      shiny::updateSelectInput(
        session, "anova_response",
        choices = numeric_indices,
        selected = if ("Rs" %in% numeric_indices) "Rs" else numeric_indices[[1]]
      )
    }
  })

  shiny::observeEvent(input$anova_source, {
    res <- analysis()
    if (is.null(res)) {
      return(invisible(NULL))
    }

    tbl <- res[[input$anova_source]]
    if (is.null(tbl) || nrow(tbl) == 0) {
      return(invisible(NULL))
    }

    numeric_indices <- names(tbl)[vapply(tbl, is.numeric, logical(1))]
    numeric_indices <- setdiff(numeric_indices, "cycle")
    if (length(numeric_indices) == 0) {
      return(invisible(NULL))
    }

    preferred <- c("Rs", "deficit_auc_normalized", "total_reduction_relative",
                   "mean_reduction")
    selected <- preferred[preferred %in% numeric_indices]
    shiny::updateSelectInput(
      session, "anova_response",
      choices = numeric_indices,
      selected = if (length(selected) > 0) selected[[1]] else numeric_indices[[1]]
    )
  })

  auc_subset <- shiny::reactive({
    res <- analysis()
    shiny::req(res)

    seg <- res$segments
    keep <- rep(TRUE, nrow(seg))
    if (!is.null(input$auc_variable) && nzchar(input$auc_variable)) {
      keep <- keep & seg$variable == input$auc_variable
    }
    if (!is.null(input$auc_treatment) && nzchar(input$auc_treatment)) {
      keep <- keep & seg$treatment == input$auc_treatment
    }
    if (!is.null(input$auc_replicate) && nzchar(input$auc_replicate)) {
      keep <- keep & seg$replicate == input$auc_replicate
    }

    seg[keep, , drop = FALSE]
  })

  output$deficit_plot <- shiny::renderPlot({
    dat <- auc_subset()
    shiny::validate(shiny::need(
      nrow(dat) > 0,
      "No rows match the selected variable, treatment and replicate."
    ))
    rehydra_call("plot_deficit_auc", dat, facet_by = c("genotype", "cycle"))
  })

  output$curve_shape_plot <- shiny::renderPlot({
    shiny::req(analysis())
    rehydra_call("plot_curve_shape", analysis()$curve_shape)
  })

  output$curve_shape_table <- render_table({
    shiny::req(analysis())
    dplyr::select(
      analysis()$curve_shape,
      dplyr::any_of(c(
        "genotype", "treatment", "replicate", "variable", "cycle",
        "stress_detected", "deficit_auc", "deficit_auc_normalized",
        "auc_damage_share", "t50", "t50_damage", "recovery_symmetry",
        "recovery_rate_k", "recovery_half_life", "rate_r_squared", "rate_method",
        "latency", "lag_to_minimum", "damage_latency", "residual_deficit",
        "overshoot"
      ))
    )
  })

  # ---- period and reduction components ---------------------------------------

  output$extended_plot <- shiny::renderPlot({
    shiny::req(analysis())
    rehydra_call("plot_extended_indices", analysis()$recovery_period, analysis()$mean_reduction)
  })

  output$period_table <- render_table({
    shiny::req(analysis())
    analysis()$recovery_period
  })

  output$reduction_table <- render_table({
    shiny::req(analysis())
    analysis()$mean_reduction
  })

  # ---- multi-cycle memory ----------------------------------------------------

  trend_selection <- shiny::reactive({
    res <- analysis()
    shiny::req(res)

    if (identical(input$trend_family, "shape")) {
      list(
        indices = res$curve_shape,
        trend = res$shape_trend,
        metrics = c("deficit_auc_normalized", "t50", "recovery_rate_k",
                    "residual_deficit")
      )
    } else {
      list(
        indices = res$resilience,
        trend = res$trend,
        metrics = c("Rt", "Rc", "Rs")
      )
    }
  })

  output$memory_trend_plot <- shiny::renderPlot({
    sel <- trend_selection()
    shiny::validate(shiny::need(
      !is.null(sel$indices) && nrow(sel$indices) > 0,
      "No per-cycle indices were available."
    ))
    rehydra_call("plot_memory_trend",
      indices = sel$indices,
      trend_results = sel$trend,
      metrics = sel$metrics
    )
  })

  output$memory_trend_table <- render_table({
    sel <- trend_selection()
    if (is.null(sel$trend) || nrow(sel$trend) == 0) {
      tibble::tibble(
        message = "At least two segmented cycles are needed to fit a memory trend."
      )
    } else {
      sel$trend
    }
  })

  output$memory_pairs_table <- render_table({
    shiny::req(analysis())
    pairs <- analysis()$memory_pairs
    if (is.null(pairs) || nrow(pairs) == 0) {
      tibble::tibble(
        message = "At least two segmented cycles are needed for consecutive contrasts."
      )
    } else {
      dplyr::select(
        pairs,
        dplyr::any_of(c(
          "genotype", "treatment", "variable", "cycle_ref", "cycle_test",
          "Mem_Rt", "Mem_Rc", "Mem_Rs", "Mem_Damage", "Mem_RecRate",
          "Mem_Residual", "interpretation"
        ))
      )
    }
  })

  # ---- designed analysis -----------------------------------------------------

  anova_fit <- shiny::reactive({
    res <- analysis()
    shiny::req(res)

    source_tbl <- res[[input$anova_source %||% "resilience"]]
    shiny::validate(shiny::need(
      !is.null(source_tbl) && nrow(source_tbl) > 0,
      "The selected index table is empty."
    ))

    response <- input$anova_response
    shiny::validate(shiny::need(
      !is.null(response) && nzchar(response) && response %in% names(source_tbl),
      "Select a numeric response index."
    ))

    tryCatch(
      rehydra_call("rehydra_anova",
        indices = source_tbl,
        response = response,
        factors = c("genotype", "treatment"),
        interaction = isTRUE(input$anova_interaction)
      ),
      error = function(e) {
        shiny::validate(shiny::need(FALSE, paste("Model could not be fitted:", conditionMessage(e))))
      }
    )
  })

  output$anova_table <- render_table({
    anova_fit()$anova
  })

  output$anova_means <- render_table({
    anova_fit()$means
  })

  output$anova_comparisons <- render_table({
    anova_fit()$comparisons
  })

  output$anova_diagnostics <- render_table({
    anova_fit()$diagnostics
  })

  output$resilience_plot <- shiny::renderPlot({
    shiny::req(analysis())
    rehydra_call("plot_resilience_indices",analysis()$resilience)
  })

  output$resilience_table <- render_table({
    shiny::req(analysis())
    analysis()$resilience
  })

  output$memory_plot <- shiny::renderPlot({
    shiny::req(analysis())
    rehydra_call("plot_memory_effect",analysis()$memory)
  })

  output$memory_table <- render_table({
    shiny::req(analysis())
    class_key <- intersect(
      c("genotype", "treatment", "variable", "cycle_ref", "cycle_test"),
      intersect(names(analysis()$memory), names(analysis()$classification))
    )

    if (length(class_key) == 0) {
      analysis()$classification
    } else {
      dplyr::left_join(
        analysis()$memory,
        dplyr::select(analysis()$classification, dplyr::all_of(c(class_key, "priming_class"))),
        by = class_key
      )
    }
  })

  output$stability_plot <- shiny::renderPlot({
    shiny::req(analysis())
    rehydra_call("plot_stability",analysis()$stability)
  })

  output$stability_table <- render_table({
    shiny::req(analysis())
    analysis()$stability
  })

  output$comparison_table <- render_table({
    shiny::req(analysis())
    analysis()$comparison$pairwise
  })

  output$download_csv <- shiny::downloadHandler(
    filename = function() {
      paste0("rehydra_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      shiny::req(analysis())
      res <- analysis()

      tag <- function(tbl, label) {
        if (is.null(tbl) || nrow(tbl) == 0) {
          return(NULL)
        }
        dplyr::mutate(tibble::as_tibble(tbl), result_table = label)
      }

      out <- dplyr::bind_rows(
        tag(res$checks, "checks"),
        tag(res$damage, "damage"),
        tag(res$recovery, "recovery"),
        tag(res$recovery_period, "recovery_period"),
        tag(res$mean_reduction, "mean_reduction"),
        tag(res$curve_shape, "curve_shape"),
        tag(res$resilience, "resilience"),
        tag(res$memory, "memory"),
        tag(res$memory_pairs, "memory_consecutive"),
        tag(res$trend, "memory_trend"),
        tag(res$shape_trend, "memory_trend_shape"),
        tag(res$classification, "classification"),
        tag(res$stability, "stability")
      )

      readr::write_csv(out, file)
    }
  )

  output$download_plot <- shiny::downloadHandler(
    filename = function() {
      paste0("rehydra_trajectory_", Sys.Date(), ".png")
    },
    content = function(file) {
      shiny::req(analysis())
      gg <- rehydra_call("plot_recovery_trajectory",
        data = analysis()$segments,
        resilience_results = analysis()$resilience
      )
      ggplot2::ggsave(filename = file, plot = gg, width = 11, height = 7, dpi = 300)
    }
  )

  output$download_report <- shiny::downloadHandler(
    filename = function() {
      paste0("rehydra_report_", Sys.Date(), ".html")
    },
    content = function(file) {
      shiny::req(analysis())

      report_path <- rehydra_call("rehydra_report",
        analysis = analysis()$report_object,
        output_format = "html",
        output_dir = tempdir(),
        output_file = basename(file)
      )

      file.copy(report_path, file, overwrite = TRUE)
    }
  )
}

shiny::shinyApp(ui = ui, server = server)
