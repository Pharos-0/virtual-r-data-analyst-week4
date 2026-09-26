# =============================================================================
# run_all.R
# Runs every numbered analysis script in R/ in order. Each script is executed
# with source(echo = TRUE) and its full console transcript (code + printed
# output + messages) is written to outputs/logs/<script>_console.txt.
# The screenshots in screenshots/ are rendered from these transcripts.
#
# Usage (from the project root):   Rscript run_all.R
# To re-run selected scripts only:  Rscript run_all.R 04 05
# =============================================================================

if (!file.exists(file.path("R", "00_setup.R"))) {
  stop("Run this file from the project root (the folder that contains R/).", call. = FALSE)
}

# UTF-8 character locale (where available) so transcripts keep special characters
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "en_US.UTF-8")))
}

scripts <- sort(list.files("R", pattern = "^0[1-9]_.*\\.R$", full.names = TRUE))
selected <- commandArgs(trailingOnly = TRUE)
if (length(selected) > 0) {
  scripts <- scripts[substr(basename(scripts), 1, 2) %in% selected]
}
dir.create(file.path("outputs", "logs"), recursive = TRUE, showWarnings = FALSE)

run_logged <- function(script) {
  log_file <- file.path("outputs", "logs",
                        sub("\\.R$", "_console.txt", basename(script)))
  con <- file(log_file, open = "wt")
  sink(con)
  sink(con, type = "message")
  started <- Sys.time()
  status <- tryCatch({
    source(script, echo = TRUE, max.deparse.length = Inf, keep.source = TRUE,
           spaced = FALSE, local = new.env(parent = globalenv()))
    "OK"
  }, error = function(e) {
    message("ERROR: ", conditionMessage(e))
    "FAILED"
  }, finally = {
    sink(type = "message")
    sink()
    close(con)
  })
  elapsed <- round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1)
  data.frame(script = script, status = status, seconds = elapsed, log = log_file)
}

say <- function(...) {                       # print and keep a copy for the run log
  line <- paste0(...)
  cat(line, "\n", sep = "")
  run_log <<- c(run_log, line)
}
run_log <- c("$ Rscript run_all.R", paste("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
say("Running ", length(scripts), " scripts with ", R.version.string)
results <- do.call(rbind, lapply(scripts, function(s) {
  r <- run_logged(s)
  say(sprintf("  %-32s %-7s %6.1f s   log: %s", s, r$status, r$seconds, r$log))
  r
}))
say("Total time: ", round(sum(results$seconds), 1), " s; failures: ", sum(results$status != "OK"))
if (length(selected) == 0) {
  write.csv(results, file.path("outputs", "logs", "run_summary.csv"), row.names = FALSE)
  writeLines(run_log, file.path("outputs", "logs", "run_all_console.txt"))
}

# Record the software environment used for this run
writeLines(capture.output(sessionInfo()), file.path("outputs", "logs", "session_info.txt"))

# Versions of the packages that the scripts in R/ load (via load_packages())
code <- paste(unlist(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), readLines)),
              collapse = "\n")
calls <- regmatches(code, gregexpr("load_packages\\(c?\\(?[^)]*\\)", code))[[1]]
calls <- calls[!grepl("pkgs", calls)]                  # skip the function definition
pkgs <- unique(unlist(regmatches(calls, gregexpr('"[A-Za-z0-9.]+"', calls))))
pkgs <- gsub('"', "", pkgs)
pkgs <- pkgs[order(tolower(pkgs))]
pkg_versions <- data.frame(
  package = pkgs,
  version = vapply(pkgs, function(p) as.character(utils::packageVersion(p)), character(1)))
write.csv(pkg_versions, file.path("outputs", "logs", "package_versions.csv"), row.names = FALSE)

if (any(results$status != "OK")) {
  stop("One or more scripts failed - see outputs/logs/ for details.", call. = FALSE)
}

# Render console screenshots (skipped with a message if Chrome is unavailable)
source(file.path("R", "render_screenshots.R"))
cat("All scripts completed successfully.\n")
