# =============================================================================
# render_screenshots.R
# Renders PNG "screenshots" of the analysis from genuine execution records:
#   type = "console" : excerpt of a console transcript in outputs/logs/
#                      (the code echoed by R followed by the output it printed)
#   type = "source"  : excerpt of an R script, with line numbers
#   type = "textfile": a complete text log (e.g. the run_all.R summary)
#   type = "html"    : an HTML output (e.g. the interactive plotly chart)
# The excerpts are listed in R/screenshot_specs.R. Non-adjacent excerpts are
# joined with an explicit "[... lines omitted ...]" marker. Images are drawn by
# headless Google Chrome/Chromium. RStudio was not available in the environment
# used for this project, so these are renderings of the real R session
# transcripts rather than RStudio window captures.
# =============================================================================
source("R/00_setup.R")
source("R/screenshot_specs.R")
load_packages("highr")

find_chrome <- function() {
  candidates <- c(Sys.getenv("CHROME_PATH"),
                  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
                  "/Applications/Chromium.app/Contents/MacOS/Chromium",
                  Sys.which(c("google-chrome", "google-chrome-stable", "chromium",
                              "chromium-browser", "chrome")))
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates) == 0) NA_character_ else candidates[[1]]
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

wrap_lines <- function(lines, width) {
  unlist(lapply(lines, function(l) {
    if (nchar(l) <= width) return(l)
    starts <- seq(1, nchar(l), by = width)
    substring(l, starts, pmin(starts + width - 1, nchar(l)))
  }))
}

# Locate a "# ---- <label> ... ----" section; returns first and last line index
section_bounds <- function(lines, label, header_regex) {
  hdr <- grep(header_regex, lines, perl = TRUE)
  hdr_text <- sub(header_regex, "\\1", lines[hdr], perl = TRUE)
  hit <- which(startsWith(hdr_text, paste0(label, " ")))
  if (length(hit) != 1) stop("Section '", label, "' found ", length(hit), " times")
  end <- if (hit < length(hdr)) hdr[hit + 1] - 1 else length(lines)
  c(hdr[hit], end)
}

# Resolve the parts of a spec to line indices within `lines`
excerpt_index <- function(lines, spec, header_regex) {
  parts <- if (!is.null(spec$parts)) spec$parts else lapply(spec$sections, function(s) list(section = s))
  lapply(parts, function(p) {
    b <- section_bounds(lines, p$section, header_regex)
    idx <- b[1]:b[2]
    if (!is.null(p$from)) {
      i <- grep(p$from, lines[idx], perl = TRUE)[1]
      if (is.na(i)) stop("Pattern '", p$from, "' not found in section ", p$section)
      idx <- idx[i:length(idx)]
    }
    if (!is.null(p$until)) {
      j <- grep(p$until, lines[idx], perl = TRUE)[1]
      if (is.na(j)) stop("Pattern '", p$until, "' not found in section ", p$section)
      idx <- idx[seq_len(j - 1)]
    }
    while (length(idx) > 1 && !nzchar(trimws(lines[idx[length(idx)]]))) idx <- idx[-length(idx)]
    idx
  })
}

omitted <- "[... lines omitted ...]"
join_excerpts <- function(lines, chunks) {
  out <- character()
  for (k in seq_along(chunks)) {
    if (k > 1 && chunks[[k]][1] != max(chunks[[k - 1]]) + 1) out <- c(out, omitted)
    out <- c(out, lines[chunks[[k]]])
  }
  out
}

console_html <- function(lines) {
  vapply(lines, function(l) {
    cls <- if (identical(l, omitted)) "omit" else if (grepl("^[>+] ", l) || l %in% c(">", "+")) {
      if (grepl("^> #", l)) "cmt" else "code"
    } else "out"
    sprintf('<div class="%s">%s</div>', cls, if (nzchar(l)) html_escape(l) else "&nbsp;")
  }, character(1), USE.NAMES = FALSE)
}

source_html <- function(num, code) {
  hl <- tryCatch(highr::hi_html(code), error = function(e) html_escape(code))
  if (length(hl) != length(code)) hl <- html_escape(code)
  ifelse(is.na(num), sprintf('<div class="omit">%s</div>', omitted),
         sprintf('<div class="src"><span class="ln">%4d</span>%s</div>', num,
                 ifelse(nzchar(code), hl, "&nbsp;")))
}

chrome_png <- function(html_file, out_file, width, height, chrome, wait_ms = 0) {
  args <- c("--headless=new", "--disable-gpu", "--hide-scrollbars",
            "--force-device-scale-factor=2", sprintf("--window-size=%d,%d", width, height),
            if (wait_ms > 0) sprintf("--virtual-time-budget=%d", wait_ms),
            paste0("--screenshot=", normalizePath(out_file, mustWork = FALSE)),
            paste0("file://", normalizePath(html_file)))
  system2(chrome, shQuote(args), stdout = FALSE, stderr = FALSE)
  file.exists(out_file)
}

render_panel <- function(body_lines, header, out_file, n_chars, n_lines, chrome) {
  char_w <- 7.83; line_h <- 17; pad <- 12; head_h <- 24
  width  <- max(560, ceiling(n_chars * char_w + 2 * pad + 2))
  height <- ceiling(head_h + n_lines * line_h + 2 * pad + 2)
  html <- c(
    "<!DOCTYPE html><html><head><meta charset='utf-8'><style>",
    "html,body{margin:0;padding:0;background:#fff;}",
    sprintf(".box{box-sizing:border-box;width:%dpx;height:%dpx;border:1px solid #c8c8c8;background:#fbfbfb;overflow:hidden;}", width, height),
    sprintf(".hdr{height:%dpx;line-height:%dpx;padding:0 %dpx;background:#eef0f3;border-bottom:1px solid #d6d9de;font:11px -apple-system,Helvetica,Arial,sans-serif;color:#4a4f57;white-space:nowrap;overflow:hidden;}", head_h - 1, head_h - 1, pad),
    sprintf(".body{padding:%dpx %dpx;font:13px/%dpx Menlo,'DejaVu Sans Mono',Consolas,monospace;color:#1a1a1a;}", pad, pad, line_h),
    sprintf(".body div{white-space:pre;height:%dpx;}", line_h),
    ".code{color:#1f3f6e;} .cmt{color:#6a737d;} .out{color:#1a1a1a;} .omit{color:#8a8f96;font-style:italic;}",
    ".ln{display:inline-block;width:3.2em;color:#9aa0a6;} .src{color:#1a1a1a;}",
    ".hl.kwd{color:#1f4e79;font-weight:bold;} .hl.com{color:#6a737d;font-style:italic;}",
    ".hl.str{color:#7a4a12;} .hl.num{color:#1f4e79;} .hl.opt{color:#1a1a1a;} .hl.std{color:#1a1a1a;}",
    "</style></head><body><div class='box'>",
    sprintf("<div class='hdr'>%s</div><div class='body'>", html_escape(header)),
    paste(body_lines, collapse = ""),
    "</div></div></body></html>")
  html_file <- tempfile(fileext = ".html")
  writeLines(html, html_file, useBytes = TRUE)
  ok <- chrome_png(html_file, out_file, width, height, chrome)
  unlink(html_file)
  ok
}

render_all_screenshots <- function(specs = screenshot_specs) {
  chrome <- find_chrome()
  if (is.na(chrome)) {
    message("Chrome/Chromium not found - screenshots skipped (set CHROME_PATH to enable).")
    return(invisible(NULL))
  }
  r_version <- paste0("R ", R.version$major, ".", R.version$minor)
  sep <- "  ·  "
  rendered <- list()
  for (spec in specs) {
    max_chars <- if (is.null(spec$max_chars)) 104 else spec$max_chars
    max_lines <- if (is.null(spec$max_lines)) 52 else spec$max_lines
    out_file  <- file.path(paths$screenshots, paste0(spec$id, ".png"))
    source_file <- switch(spec$type,
                          console  = file.path(paths$logs, paste0(spec$log, "_console.txt")),
                          textfile = spec$file, source = spec$file, html = spec$file)
    if (!file.exists(source_file)) next

    if (spec$type == "html") {
      ok <- chrome_png(source_file, out_file, spec$width, spec$height, chrome, wait_ms = 4000)
    } else if (spec$type == "source") {
      code_lines <- readLines(source_file, encoding = "UTF-8", warn = FALSE)
      chunks <- excerpt_index(code_lines, spec, "^# ---- (.*?) -+$")
      num <- integer(); code <- character()
      for (k in seq_along(chunks)) {
        if (k > 1 && chunks[[k]][1] != max(chunks[[k - 1]]) + 1) { num <- c(num, NA); code <- c(code, "") }
        num <- c(num, chunks[[k]]); code <- c(code, code_lines[chunks[[k]]])
      }
      if (length(code) > max_lines) stop(spec$id, ": ", length(code), " lines exceed max_lines")
      header <- paste0("R script", sep, spec$file, sep, "lines ", min(num, na.rm = TRUE), "-",
                       max(num, na.rm = TRUE), sep, spec$title)
      ok <- render_panel(source_html(num, code), header, out_file,
                         min(max(nchar(code)) + 5, max_chars + 5), length(code), chrome)
    } else {
      all_lines <- readLines(source_file, encoding = "UTF-8", warn = FALSE)
      lines <- if (spec$type == "console") {
        join_excerpts(all_lines, excerpt_index(all_lines, spec, "^> # ---- (.*?) -+$"))
      } else all_lines
      lines <- wrap_lines(lines, max_chars)
      if (length(lines) > max_lines) stop(spec$id, ": ", length(lines), " lines exceed max_lines")
      header <- if (spec$type == "console") {
        paste0("R console transcript", sep, r_version, sep, "source('R/", spec$log,
               ".R', echo = TRUE)", sep, spec$title)
      } else paste0("Text log", sep, spec$file, sep, spec$title)
      ok <- render_panel(console_html(lines), header, out_file,
                         min(max(nchar(lines), 60), max_chars), length(lines), chrome)
    }
    message(sprintf("%-46s %s", out_file, if (ok) "rendered" else "FAILED"))
    rendered[[spec$id]] <- tibble(id = spec$id, type = spec$type, week = spec$week,
                                  title = spec$title, file = out_file, source = source_file, ok = ok)
  }
  index <- bind_rows(rendered)
  if (nrow(index) > 0) write_csv(index, file.path(paths$screenshots, "screenshot_index.csv"))
  invisible(index)
}

# Line counts of every excerpt without rendering (Sys.setenv(SCREENSHOT_CHECK = "1"))
check_specs <- function(specs = screenshot_specs) {
  for (spec in specs) {
    if (!spec$type %in% c("console", "source")) next
    f <- if (spec$type == "console") file.path(paths$logs, paste0(spec$log, "_console.txt")) else spec$file
    if (!file.exists(f)) next
    l <- readLines(f, encoding = "UTF-8", warn = FALSE)
    rx <- if (spec$type == "console") "^> # ---- (.*?) -+$" else "^# ---- (.*?) -+$"
    n <- length(join_excerpts(l, excerpt_index(l, spec, rx)))
    cat(sprintf("%-34s %3d %s\n", spec$id, n, if (n > 52) "<-- too long" else ""))
  }
}

if (identical(Sys.getenv("SCREENSHOT_CHECK"), "1")) check_specs() else render_all_screenshots()
