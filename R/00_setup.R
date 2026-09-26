# =============================================================================
# 00_setup.R
# Shared configuration sourced by every analysis script:
#   - package loading (with a clear error if something is missing)
#   - file paths and the dataset source URL
#   - the random seed used throughout the project
#   - a restrained ggplot2 theme and colour palette
#   - small helpers for saving figures, tables and key metrics
# All scripts assume the working directory is the project root.
# =============================================================================

if (!file.exists(file.path("R", "00_setup.R"))) {
  stop("Please run the scripts from the project root (the folder that contains R/).",
       call. = FALSE)
}

# ---- Packages ----------------------------------------------------------------
load_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Missing R packages: ", paste(missing, collapse = ", "),
         "\nInstall with: install.packages(c(",
         paste0('"', missing, '"', collapse = ", "), "))", call. = FALSE)
  }
  suppressPackageStartupMessages(
    invisible(lapply(pkgs, library, character.only = TRUE))
  )
}

load_packages(c("readr", "dplyr", "tidyr", "stringr", "forcats", "purrr",
                "tibble", "ggplot2", "scales", "jsonlite", "ragg"))

# ---- Global options -----------------------------------------------------------
# Use a UTF-8 character locale where available so printed output renders properly
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "en_US.UTF-8")))
}
SEED <- 42                       # one fixed seed, used wherever randomness occurs
options(width = 100,             # console width used in the saved transcripts
        scipen = 6,
        warn = 1,                # print warnings immediately (visible in logs)
        dplyr.summarise.inform = FALSE,
        readr.show_col_types = FALSE)

# ---- Paths --------------------------------------------------------------------
DATA_URL <- paste0("https://raw.githubusercontent.com/IBM/telco-customer-churn-on-icp4d/",
                   "master/data/Telco-Customer-Churn.csv")

paths <- list(
  raw_data    = file.path("data", "raw", "Telco-Customer-Churn.csv"),
  processed   = file.path("data", "processed"),
  tables      = file.path("outputs", "tables"),
  models      = file.path("outputs", "models"),
  metrics     = file.path("outputs", "metrics"),
  logs        = file.path("outputs", "logs"),
  figures     = "figures",
  screenshots = "screenshots"
)
invisible(lapply(paths[-1], dir.create, recursive = TRUE, showWarnings = FALSE))

# ---- Plot theme and palette ----------------------------------------------------
# One accent colour (navy) plus a neutral grey. Churned customers are always
# navy and retained customers always grey, so the encoding is consistent
# across every chart in the project.
col_accent  <- "#1F4E79"
col_neutral <- "#7F8B98"
col_text    <- "#222222"
col_muted   <- "#595959"
col_grid    <- "#E6E6E6"
pal_churn   <- c(No = col_neutral, Yes = col_accent)
pal_ordinal <- c("#8FB0D2", "#4A7BAB", "#1F4E79")   # light -> dark, ordered groups

theme_report <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title         = element_text(face = "bold", colour = col_text,
                                        size = rel(1.05), margin = margin(b = 3)),
      plot.subtitle      = element_text(colour = col_muted, size = rel(0.88),
                                        margin = margin(b = 8)),
      plot.caption       = element_text(colour = col_muted, size = rel(0.72), hjust = 0),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      axis.title         = element_text(colour = col_text, size = rel(0.88)),
      axis.text          = element_text(colour = col_muted, size = rel(0.85)),
      panel.grid.major   = element_line(colour = col_grid, linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      legend.position    = "top",
      legend.justification = "left",
      legend.title       = element_text(size = rel(0.85), colour = col_text),
      legend.text        = element_text(size = rel(0.85), colour = col_text),
      strip.text         = element_text(face = "bold", colour = col_text, hjust = 0,
                                        size = rel(0.88)),
      plot.margin        = margin(8, 12, 6, 6)
    )
}
theme_set(theme_report())

# ---- Helpers --------------------------------------------------------------------
save_plot <- function(plot, name, width = 7, height = 4.2, dpi = 300) {
  file <- file.path(paths$figures, paste0(name, ".png"))
  ggsave(file, plot, width = width, height = height, dpi = dpi,
         device = ragg::agg_png, bg = "white")
  message("Saved figure: ", file)
  invisible(file)
}

save_table <- function(df, name) {
  file <- file.path(paths$tables, paste0(name, ".csv"))
  readr::write_csv(df, file)
  message("Saved table: ", file)
  invisible(file)
}

# Key numbers are stored in outputs/metrics/*.json so that the written reports
# quote exactly the values produced by the code (nothing is typed by hand).
.metrics_env <- new.env()
record <- function(key, value) {
  assign(key, value, envir = .metrics_env)
  invisible(value)
}
write_metrics <- function(name) {
  keys <- sort(ls(.metrics_env))
  vals <- mget(keys, envir = .metrics_env)
  file <- file.path(paths$metrics, paste0(name, ".json"))
  jsonlite::write_json(vals, file, auto_unbox = TRUE, digits = NA, pretty = TRUE)
  message("Saved ", length(keys), " metrics: ", file)
  invisible(file)
}

pct <- function(x, digits = 1) sprintf(paste0("%.", digits, "f%%"), 100 * x)
