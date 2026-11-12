#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else "run_all.R"
root <- normalizePath(dirname(script_path), mustWork = TRUE)
old_wd <- setwd(root)
on.exit(setwd(old_wd), add = TRUE)

scripts <- c(
  file.path("R", "01_build_results.R"),
  file.path("R", "02_make_figures.R"),
  file.path("R", "03_validate.R")
)

for (script in scripts) {
  command <- paste(shQuote(file.path(R.home("bin"), "Rscript")), shQuote(script))
  status <- system(command)
  if (!identical(status, 0L)) stop("Failed: ", basename(script))
}
