#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
  library(sf)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else "R/01_build_results.R"
root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
input_dir <- file.path(root, "data", "input")
cache_dir <- file.path(root, "output", "cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

years <- 2022:2100
ssps <- paste0("SSP", 1:5)
models <- c("jules", "lpjml", "mc2")

required <- c(
  file.path(input_dir, "impact_regions.gpkg"),
  file.path(input_dir, "ssp_country_socioeconomic_annual.rds"),
  file.path(input_dir, "fair_gmst_2022_2100.csv"),
  file.path(input_dir, "service_elasticities.csv")
)
for (model in models) {
  required <- c(
    required,
    file.path(input_dir, "coefficients", model, "forest_coefficients.xlsx"),
    file.path(input_dir, "coefficients", model, "forest_cveg_coefficients.xlsx"),
    file.path(input_dir, "coefficients", model, "cveg_global_calibration.csv")
  )
}
missing <- required[!file.exists(required)]
if (length(missing)) stop("Missing inputs: ", paste(missing, collapse = ", "))

common_sf <- st_read(file.path(input_dir, "impact_regions.gpkg"), quiet = TRUE)
common_columns <- c(
  "region_id", "iso", "pop_share", "gdp_share", "region_area_ha",
  "rec_usd_ha_yr_2020", "hab_usd_ha_yr_2020",
  "nwfp_usd_ha_yr_2020", "wat_usd_ha_yr_2020"
)
stopifnot(nrow(common_sf) == 22738L, all(common_columns %in% names(common_sf)))
common_sf <- common_sf[, common_columns]

socioeconomic <- as.data.table(readRDS(file.path(input_dir, "ssp_country_socioeconomic_annual.rds")))
socioeconomic <- socioeconomic[ssp %in% ssps & year %in% years]
fair <- fread(file.path(input_dir, "fair_gmst_2022_2100.csv"))[ssp %in% ssps & year %in% years]
elasticities <- fread(file.path(input_dir, "service_elasticities.csv"))
elasticity_lookup <- split(elasticities, elasticities$service)

saveRDS(socioeconomic, file.path(cache_dir, "ssp_country_socioeconomic_annual.rds"), compress = "xz")
fwrite(fair, file.path(cache_dir, "fair_gmst_2022_2100.csv"))

quality <- vector("list", length(models))

for (model_index in seq_along(models)) {
  model <- models[[model_index]]
  message("Building ", model, " results")
  coefficient_dir <- file.path(input_dir, "coefficients", model)

  forest_source <- as.data.table(read.xlsx(file.path(coefficient_dir, "forest_coefficients.xlsx")))
  cveg_source <- as.data.table(read.xlsx(file.path(coefficient_dir, "forest_cveg_coefficients.xlsx")))
  calibration <- fread(file.path(coefficient_dir, "cveg_global_calibration.csv"))

  forest <- forest_source[, .(
    region_id = impact_region,
    region_area_Mha,
    forest_Mha_2022 = baseline_forest_Mha,
    forest_b1_Mha_per_C = b1_dGMST,
    forest_b2_Mha_per_C2 = b2_dGMST2,
    forest_b1_se_Mha_per_C = se_b1,
    forest_b2_se_Mha_per_C2 = se_b2,
    forest_p_b1 = p_b1,
    forest_p_b2 = p_b2,
    forest_R2 = R2
  )]
  cveg <- cveg_source[, .(
    region_id = impact_region,
    cveg_tC_2022 = baseline_cVeg_tC,
    cveg_b1_tC_per_C = b1_dGMST,
    cveg_b2_tC_per_C2 = b2_dGMST2,
    cveg_b1_se_tC_per_C = se_b1,
    cveg_b2_se_tC_per_C2 = se_b2,
    cveg_p_b1 = p_b1,
    cveg_p_b2 = p_b2,
    cveg_R2 = R2
  )]
  coefficients <- merge(forest, cveg, by = "region_id", all = TRUE)
  stopifnot(
    nrow(coefficients) == 22738L,
    !anyDuplicated(coefficients$region_id),
    abs(sum(coefficients$cveg_tC_2022) / 1e9 - 312.321) < 1e-6
  )

  model_sf <- merge(common_sf, coefficients, by = "region_id", all.x = TRUE, sort = FALSE)
  model_sf$region_area_ha <- model_sf$region_area_Mha * 1e6
  map_path <- file.path(cache_dir, paste0("ssp_impact_regions_map_", model, ".gpkg"))
  if (file.exists(map_path)) unlink(map_path)
  st_write(model_sf, map_path, layer = "impact_regions", quiet = TRUE)

  regions <- as.data.table(st_drop_geometry(model_sf))
  country_values <- vector("list", length(ssps))
  country_carbon <- vector("list", length(ssps))

  for (ssp_index in seq_along(ssps)) {
    ssp_name <- ssps[[ssp_index]]
    message("  ", ssp_name)
    socio <- socioeconomic[ssp == ssp_name, .(
      iso = iso3,
      country,
      year,
      gdp_billion_2005_ppp,
      population_million,
      gdp_per_capita_2005_ppp,
      gdp_growth_vs_2022,
      population_growth_vs_2022,
      gdppc_growth_vs_2022
    )]
    climate <- fair[ssp == ssp_name, .(year, gmst_C, delta_gmst_vs_2022_C)]

    annual <- regions[rep(seq_len(.N), each = length(years))]
    annual[, year := rep(years, times = nrow(regions))]
    annual <- merge(annual, socio, by = c("iso", "year"), all.x = TRUE, sort = FALSE)
    annual <- merge(annual, climate, by = "year", all.x = TRUE, sort = FALSE)
    annual[, ssp := ssp_name]

    annual[, forest_Mha := pmax(
      0,
      pmin(
        region_area_Mha,
        forest_Mha_2022 +
          forest_b1_Mha_per_C * delta_gmst_vs_2022_C +
          forest_b2_Mha_per_C2 * delta_gmst_vs_2022_C^2
      )
    )]
    annual[, forest_change_Mha_vs_2022 := forest_Mha - forest_Mha_2022]

    for (service in names(elasticity_lookup)) {
      spec <- elasticity_lookup[[service]][1]
      baseline <- paste0(service, "_usd_ha_yr_2020")
      marginal <- paste0(service, "_mv_usd_ha_yr")
      climate_value <- paste0(service, "_total_usd_yr")
      fixed_value <- paste0(service, "_total_no_climate_usd_yr")
      annual[, (marginal) := get(baseline) *
        gdppc_growth_vs_2022^spec$gdp_per_capita_elasticity *
        population_growth_vs_2022^spec$population_density_elasticity]
      annual[, (climate_value) := get(marginal) * forest_Mha * 1e6]
      annual[, (fixed_value) := get(marginal) * forest_Mha_2022 * 1e6]
    }

    forest_country <- annual[, .(
      forest_Mha = sum(forest_Mha, na.rm = TRUE),
      forest_change_Mha_vs_2022 = sum(forest_change_Mha_vs_2022, na.rm = TRUE),
      emulator_region_count = .N
    ), by = .(ssp, year, iso3 = iso, country)]

    services_country <- rbindlist(lapply(names(elasticity_lookup), function(service) {
      climate_value <- paste0(service, "_total_usd_yr")
      fixed_value <- paste0(service, "_total_no_climate_usd_yr")
      result <- annual[, .(
        annual_value_usd = sum(get(climate_value), na.rm = TRUE),
        annual_value_no_climate_usd = sum(get(fixed_value), na.rm = TRUE),
        valued_forest_Mha = sum(forest_Mha, na.rm = TRUE),
        valued_region_count = .N
      ), by = .(ssp, year, iso3 = iso, country)]
      result[, service := service]
      result
    }))
    services_country <- merge(
      services_country,
      forest_country,
      by = c("ssp", "year", "iso3", "country"),
      all.x = TRUE
    )
    services_country[, forest_value_coverage_pct := 100 * valued_forest_Mha / forest_Mha]
    country_values[[ssp_index]] <- services_country

    setorder(annual, region_id, year)
    annual[, cveg_stock_tC := pmax(
      0,
      cveg_tC_2022 +
        cveg_b1_tC_per_C * delta_gmst_vs_2022_C +
        cveg_b2_tC_per_C2 * delta_gmst_vs_2022_C^2
    )]
    annual[, carbon_flux_tCO2 := (cveg_stock_tC - shift(cveg_stock_tC)) * 44 / 12, by = region_id]
    annual[year == 2022, carbon_flux_tCO2 := NA_real_]
    country_carbon[[ssp_index]] <- annual[, .(
      cveg_stock_tC = sum(cveg_stock_tC, na.rm = TRUE),
      carbon_flux_tCO2 = if (all(is.na(carbon_flux_tCO2))) NA_real_ else sum(carbon_flux_tCO2, na.rm = TRUE),
      carbon_region_count = .N
    ), by = .(ssp, year, iso3 = iso, country)]

    rm(annual, services_country, forest_country)
    invisible(gc())
  }

  country_values <- rbindlist(country_values)
  country_values <- merge(
    country_values,
    elasticities[, .(service, service_label = label, include_in_official_total)],
    by = "service",
    all.x = TRUE
  )
  setorder(country_values, ssp, iso3, year, service)
  saveRDS(
    country_values,
    file.path(cache_dir, paste0("ssp_country_ecosystem_values_annual_", model, ".rds")),
    compress = "xz"
  )

  country_carbon <- rbindlist(country_carbon)
  setorder(country_carbon, ssp, iso3, year)
  saveRDS(
    country_carbon,
    file.path(cache_dir, paste0("country_carbon_flux_annual_", model, ".rds")),
    compress = "xz"
  )

  quality[[model_index]] <- data.table(
    model,
    regions = nrow(regions),
    countries = uniqueN(country_values$iso3),
    forest_2022_Mha = sum(regions$forest_Mha_2022),
    cveg_2022_GtC = sum(regions$cveg_tC_2022) / 1e9,
    forest_median_R2 = median(regions$forest_R2, na.rm = TRUE),
    cveg_median_R2 = median(regions$cveg_R2, na.rm = TRUE),
    cveg_calibration_factor = calibration$global_calibration_factor
  )
}

fwrite(rbindlist(quality), file.path(cache_dir, "quality_control.csv"))
message("Finished")
