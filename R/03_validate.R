#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else "R/03_validate.R"
root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
cache_dir <- file.path(root, "output", "cache")
table_dir <- file.path(root, "output", "tables")

models <- c("jules", "lpjml", "mc2")
required <- c(
  file.path(cache_dir, paste0("ssp_impact_regions_map_", models, ".gpkg")),
  file.path(cache_dir, paste0("ssp_country_ecosystem_values_annual_", models, ".rds")),
  file.path(cache_dir, paste0("country_carbon_flux_annual_", models, ".rds")),
  file.path(table_dir, paste0("Table_S", 1:5, c(
    "_emulator_diagnostics.csv",
    "_global_forest_2100.csv",
    "_global_cveg_change_2100.csv",
    "_global_noncarbon_npv.csv",
    "_country_noncarbon_npv_effect_ssp585.csv"
  )))
)
missing <- required[!file.exists(required)]
if (length(missing)) stop("Missing outputs: ", paste(missing, collapse = ", "))

checks <- rbindlist(lapply(models, function(model) {
  regions <- st_read(
    file.path(cache_dir, paste0("ssp_impact_regions_map_", model, ".gpkg")),
    quiet = TRUE
  )
  values <- readRDS(file.path(cache_dir, paste0("ssp_country_ecosystem_values_annual_", model, ".rds")))
  carbon <- readRDS(file.path(cache_dir, paste0("country_carbon_flux_annual_", model, ".rds")))
  data.table(
    model,
    regions = nrow(regions),
    countries = uniqueN(values$iso3),
    years = uniqueN(values$year),
    scenarios = uniqueN(values$ssp),
    forest_2022_Mha = sum(regions$forest_Mha_2022),
    cveg_2022_GtC = sum(regions$cveg_tC_2022) / 1e9,
    missing_values = sum(!is.finite(values$annual_value_usd)),
    carbon_2022_flow_is_missing = all(is.na(carbon$carbon_flux_tCO2[carbon$year == 2022]))
  )
}))

stopifnot(
  all(checks$regions == 22738L),
  all(checks$countries == 251L),
  all(checks$years == 79L),
  all(checks$scenarios == 5L),
  max(abs(checks$forest_2022_Mha - 4282.549)) < 0.01,
  max(abs(checks$cveg_2022_GtC - 312.321)) < 1e-6,
  all(checks$missing_values == 0L),
  all(checks$carbon_2022_flow_is_missing)
)

fwrite(checks, file.path(root, "output", "validation_summary.csv"))
message("Validation passed")
