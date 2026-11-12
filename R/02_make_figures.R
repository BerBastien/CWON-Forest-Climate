#!/usr/bin/env Rscript

# Reproducible main-text figures for the Earth's Future manuscript.
# Monetary results intentionally exclude both habitat and carbon valuation.

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(openxlsx)
  library(patchwork)
  library(scales)
  library(sf)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else "R/02_make_figures.R"
project_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
data_root <- file.path(project_root, "output", "cache")
source_root <- file.path(project_root, "data", "input", "coefficients")
out_dir <- file.path(project_root, "output", "figures")
table_dir <- file.path(project_root, "output", "tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

model_specs <- data.table(
  model = c("jules", "lpjml", "mc2"),
  label = c("JULES-ES", "LPJmL", "MC2"),
  source_dir = file.path(source_root, c("jules", "lpjml", "mc2"))
)
ssp_labels <- c(SSP1 = "SSP1-2.6", SSP2 = "SSP2-4.5", SSP3 = "SSP3-7.0", SSP4 = "SSP4-6.0", SSP5 = "SSP5-8.5")
ssp_colours <- c(SSP1 = "#467A9D", SSP2 = "#D49B3E", SSP3 = "#B45A4D", SSP4 = "#7565A8", SSP5 = "#563D55")
ssp_colours_labeled <- setNames(unname(ssp_colours), unname(ssp_labels))
case_colours <- c(
  "Fixed forest and fixed socioeconomics" = "#D7A857",
  "Fixed forest and evolving socioeconomics" = "#367D5B",
  "Climate driven forest change and evolving socioeconomics" = "#B35F44"
)

theme_paper <- function(base_size = 10) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "#D9D9D9", linewidth = 0.28),
      axis.title = element_text(colour = "#222222"),
      axis.text = element_text(colour = "#333333"),
      strip.text = element_text(face = "bold", colour = "#222222"),
      strip.background = element_rect(fill = "#F3F3F1", colour = NA),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.key.height = grid::unit(3.8, "mm"),
      plot.margin = margin(4, 5, 3, 5)
    )
}

save_figure <- function(plot, filename, width, height) {
  ggsave(file.path(out_dir, filename), plot, width = width, height = height,
         units = "in", dpi = 400, bg = "transparent")
}

# ---- Figure 1: common spatial framework ------------------------------------
regions <- st_read(file.path(data_root, "ssp_impact_regions_map_jules.gpkg"), quiet = TRUE)
sf_use_s2(FALSE)
region_dt <- as.data.table(st_drop_geometry(regions))
map_metric <- function(column, label, palette, transform = c("log", "none")) {
  transform <- match.arg(transform)
  dat <- regions
  x <- dat[[column]]
  if (transform == "log") {
    dat$plot_value <- log10(pmax(x, 0) + 1)
    lim <- quantile(dat$plot_value, c(0.02, 0.98), na.rm = TRUE)
    dat$plot_value <- pmin(pmax(dat$plot_value, lim[1]), lim[2])
    scale_lab <- label
  } else {
    lim <- quantile(x, c(0.02, 0.98), na.rm = TRUE)
    dat$plot_value <- pmin(pmax(x, lim[1]), lim[2])
    scale_lab <- label
  }
  ggplot(dat) +
    geom_sf(aes(fill = plot_value), linewidth = 0) +
    scale_fill_gradientn(colours = palette, name = scale_lab, labels = label_number()) +
    coord_sf(expand = FALSE) +
    theme_void(base_size = 8) +
    theme(legend.position = "bottom", legend.direction = "horizontal",
          legend.text = element_text(colour = "#333333", size = 7),
          legend.title = element_text(colour = "#333333", size = 7))
}

p_forest <- map_metric("forest_Mha_2022", "Forest area\nMha per region",
                       c("#F0EEE8", "#C7D6B4", "#5B8B63", "#174A3A"), "log")
p_rec <- map_metric("rec_usd_ha_yr_2020", "Recreation\nUS$ ha-1 yr-1",
                    c("#FFF1E7", "#E8A064", "#B95F4C", "#6D2B35"), "log")
p_nwfp <- map_metric("nwfp_usd_ha_yr_2020", "NWFP\nUS$ ha-1 yr-1",
                     c("#FBF3D3", "#DFBD63", "#A97833", "#5A321B"), "log")
p_water <- map_metric("wat_usd_ha_yr_2020", "Water services\nUS$ ha-1 yr-1",
                      c("#E4F3F3", "#79BDC8", "#33859E", "#173F63"), "log")
fig1 <- (p_forest + p_rec) / (p_nwfp + p_water) + plot_annotation(tag_levels = "a")
save_figure(fig1, "Figure_1_spatial_baseline.png", 7.2, 6.0)

# ---- Figure 2: regional response and fit diagnostics ------------------------
coef_list <- lapply(seq_len(nrow(model_specs)), function(i) {
  s <- model_specs[i]
  f <- as.data.table(read.xlsx(file.path(s$source_dir, "forest_coefficients.xlsx")))
  f[, model := s$model]
  f
})
coef_all <- rbindlist(coef_list, use.names = TRUE)
coef_all[, model_label := factor(model_specs$label[match(model, model_specs$model)], levels = model_specs$label)]

# Latitude of impact-region centroids is identical across models.
centroids <- st_coordinates(st_centroid(st_geometry(regions), of_largest_polygon = TRUE))
lat_lookup <- data.table(impact_region = region_dt$region_id, latitude = centroids[, 2])
coef_all <- merge(coef_all, lat_lookup, by = "impact_region", all.x = TRUE)
coef_all[, significance := ifelse(is.finite(p_b1) & p_b1 < 0.05, "p < 0.05", "p >= 0.05")]
coef_all[, change_display := pmin(pmax(change_at_1C_Mha,
                                       quantile(change_at_1C_Mha, 0.01, na.rm = TRUE)),
                                quantile(change_at_1C_Mha, 0.99, na.rm = TRUE)), by = model]
diagnostic <- coef_all[, .(
  median_R2 = median(R2, na.rm = TRUE),
  significant_linear = 100 * mean(p_b1 < 0.05, na.rm = TRUE),
  significant_quadratic = 100 * mean(p_b2 < 0.05, na.rm = TRUE)
), by = model_label]
diagnostic[, label := sprintf("median R2 = %.2f\nlinear p < 0.05: %.0f%%\nquadratic p < 0.05: %.0f%%",
                              median_R2, significant_linear, significant_quadratic)]

fig2 <- ggplot(coef_all, aes(x = latitude, y = change_display, colour = significance)) +
  geom_hline(yintercept = 0, colour = "#777777", linewidth = 0.3) +
  geom_point(alpha = 0.18, size = 0.26) +
  geom_text(data = diagnostic, aes(x = -52, y = Inf, label = label), inherit.aes = FALSE,
            hjust = 0, vjust = 1.15, size = 2.4, colour = "#303030", lineheight = 1.05) +
  facet_wrap(~model_label, nrow = 1) +
  scale_colour_manual(values = c("p < 0.05" = "#A95347", "p >= 0.05" = "#A7A7A0")) +
  labs(x = "Impact-region centroid latitude (degrees)",
       y = "Forest-area change at +1 degree C (Mha per region)") +
  theme_paper(9) +
  theme(legend.position = "top", panel.spacing = grid::unit(4, "mm"))
save_figure(fig2, "Figure_2_regional_emulator_responses.png", 7.2, 3.1)

# ---- Spatial emulator-fit diagnostics ---------------------------------------
# Use a common 0–1 scale so that model-specific R² patterns are directly
# comparable. The values are within-sample regional emulator diagnostics.
r2_colours <- c("#F0EEE8", "#C7D6B4", "#8CAF79", "#4D8B5E", "#194B3A")

model_r2_map <- function(model, column, metric_label) {
  spatial_data <- st_read(
    file.path(data_root, paste0("ssp_impact_regions_map_", model, ".gpkg")),
    quiet = TRUE
  )
  values <- pmin(pmax(spatial_data[[column]], 0), 1)
  spatial_data$plot_r2 <- values
  label <- model_specs$label[match(model, model_specs$model)]
  median_r2 <- median(values, na.rm = TRUE)
  ggplot(spatial_data) +
    geom_sf(aes(fill = plot_r2), linewidth = 0) +
    scale_fill_gradientn(
      colours = r2_colours,
      limits = c(0, 1),
      oob = scales::squish,
      name = expression(italic(R)^2),
      labels = label_number(accuracy = 0.1)
    ) +
    coord_sf(expand = FALSE) +
    labs(title = paste0(label, "\nmedian ", metric_label, " R² = ", sprintf("%.2f", median_r2))) +
    theme_void(base_size = 8) +
    theme(
      plot.title = element_text(face = "bold", colour = "#222222", size = 8, hjust = 0.5, lineheight = 1.05),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.text = element_text(colour = "#333333", size = 7),
      legend.title = element_text(colour = "#333333", size = 7),
      plot.margin = margin(3, 2, 1, 2)
    )
}

forest_r2_maps <- lapply(model_specs$model, model_r2_map, column = "forest_R2", metric_label = "forest")
fig_forest_r2 <- wrap_plots(forest_r2_maps, nrow = 1, guides = "collect") &
  theme(legend.position = "bottom")
save_figure(fig_forest_r2, "Figure_6_forest_emulator_R2.png", 7.2, 2.65)

cveg_r2_maps <- lapply(model_specs$model, model_r2_map, column = "cveg_R2", metric_label = "cVeg")
fig_cveg_r2 <- wrap_plots(cveg_r2_maps, nrow = 1, guides = "collect") &
  theme(legend.position = "bottom")
save_figure(fig_cveg_r2, "Figure_7_cveg_emulator_R2.png", 7.2, 2.65)

# ---- Inputs for global trajectories and NPV ---------------------------------
read_model_values <- function(model) {
  x <- as.data.table(readRDS(file.path(data_root, paste0("ssp_country_ecosystem_values_annual_", model, ".rds"))))
  cveg <- as.data.table(readRDS(file.path(data_root, paste0("country_carbon_flux_annual_", model, ".rds"))))
  list(values = x, cveg = cveg)
}
model_values <- setNames(lapply(model_specs$model, read_model_values), model_specs$model)

# ---- Figure 3: forest and cVeg trajectories ---------------------------------
fair <- fread(file.path(data_root, "fair_gmst_2022_2100.csv"))

# The coefficient workbooks report standard errors for b1 and b2 but not their
# covariance. Bands use a delta-method approximation with zero b1–b2 covariance
# and independent regional coefficient errors. They quantify regression
# coefficient uncertainty only, not shared-driver or DGVM structural uncertainty.
global_coefficient_trajectory <- function(model) {
  regions_model <- as.data.table(st_drop_geometry(
    st_read(file.path(data_root, paste0("ssp_impact_regions_map_", model, ".gpkg")), quiet = TRUE)
  ))
  rbindlist(lapply(names(ssp_labels), function(scenario) {
    trajectory <- fair[ssp == scenario & year >= 2022 & year <= 2100,
                       .(year, delta_gmst_vs_2022_C)]
    trajectory[, {
      delta <- delta_gmst_vs_2022_C
      forest_estimate <- sum(pmax(
        0,
        pmin(
          regions_model$region_area_ha / 1e6,
          regions_model$forest_Mha_2022 +
            regions_model$forest_b1_Mha_per_C * delta +
            regions_model$forest_b2_Mha_per_C2 * delta^2
        )
      ), na.rm = TRUE)
      forest_se <- sqrt(sum(
        (delta * regions_model$forest_b1_se_Mha_per_C)^2 +
          (delta^2 * regions_model$forest_b2_se_Mha_per_C2)^2,
        na.rm = TRUE
      ))
      cveg_estimate <- sum(pmax(
        0,
        regions_model$cveg_tC_2022 +
          regions_model$cveg_b1_tC_per_C * delta +
          regions_model$cveg_b2_tC_per_C2 * delta^2
      ), na.rm = TRUE) / 1e9
      cveg_se <- sqrt(sum(
        (delta * regions_model$cveg_b1_se_tC_per_C)^2 +
          (delta^2 * regions_model$cveg_b2_se_tC_per_C2)^2,
        na.rm = TRUE
      )) / 1e9
      list(
        forest_Mha = forest_estimate,
        forest_lower_Mha = max(0, forest_estimate - 1.96 * forest_se),
        forest_upper_Mha = min(sum(regions_model$region_area_ha, na.rm = TRUE) / 1e6,
                                forest_estimate + 1.96 * forest_se),
        cveg_stock_GtC = cveg_estimate,
        cveg_lower_GtC = max(0, cveg_estimate - 1.96 * cveg_se),
        cveg_upper_GtC = cveg_estimate + 1.96 * cveg_se
      )
    }, by = .(year, delta_gmst_vs_2022_C)][, ssp := scenario]
  }))
}

global_trajectories <- rbindlist(lapply(model_specs$model, function(model) {
  x <- global_coefficient_trajectory(model)
  x[, model := model]
  x
}))
forest_global <- copy(global_trajectories)
cveg_global <- copy(global_trajectories)
cveg_global <- merge(
  cveg_global,
  cveg_global[year == 2022, .(model, ssp, cveg_2022 = cveg_stock_GtC)],
  by = c("model", "ssp"),
  all.x = TRUE
)
cveg_global[, cveg_change_GtC := cveg_stock_GtC - cveg_2022]
cveg_global[, cveg_change_lower_GtC := cveg_lower_GtC - cveg_2022]
cveg_global[, cveg_change_upper_GtC := cveg_upper_GtC - cveg_2022]
forest_global[, model_label := factor(model_specs$label[match(model, model_specs$model)], levels = model_specs$label)]
cveg_global[, model_label := factor(model_specs$label[match(model, model_specs$model)], levels = model_specs$label)]
forest_global[, ssp_label := factor(ssp_labels[ssp], levels = unname(ssp_labels))]
cveg_global[, ssp_label := factor(ssp_labels[ssp], levels = unname(ssp_labels))]

p_forest_global <- ggplot(forest_global, aes(year, forest_Mha / 1000, colour = ssp_label)) +
  geom_ribbon(
    aes(ymin = forest_lower_Mha / 1000, ymax = forest_upper_Mha / 1000, fill = ssp_label),
    alpha = 0.14, linewidth = 0, show.legend = FALSE
  ) +
  geom_line(linewidth = 0.55) +
  facet_wrap(~model_label, nrow = 1) +
  scale_colour_manual(values = ssp_colours_labeled, drop = FALSE) +
  scale_fill_manual(values = ssp_colours_labeled, drop = FALSE) +
  labs(x = NULL, y = "Global forest area\n(thousand Mha)") +
  theme_paper(9) + theme(legend.position = "top")
p_cveg_global <- ggplot(cveg_global, aes(year, cveg_change_GtC, colour = ssp_label)) +
  geom_hline(yintercept = 0, colour = "#777777", linewidth = 0.3) +
  geom_ribbon(
    aes(ymin = cveg_change_lower_GtC, ymax = cveg_change_upper_GtC, fill = ssp_label),
    alpha = 0.14, linewidth = 0, show.legend = FALSE
  ) +
  geom_line(linewidth = 0.55) +
  facet_wrap(~model_label, nrow = 1) +
  scale_colour_manual(values = ssp_colours_labeled, drop = FALSE) +
  scale_fill_manual(values = ssp_colours_labeled, drop = FALSE) +
  labs(x = "Year", y = "Vegetation-carbon change\nrelative to 2022 (GtC)") +
  theme_paper(9) + theme(legend.position = "bottom")
fig3 <- p_forest_global / p_cveg_global + plot_annotation(tag_levels = "a")
save_figure(fig3, "Figure_3_global_forest_and_cveg_trajectories.png", 7.2, 5.5)

# ---- NPV routine: official three non-carbon services, 4% annual discount ---
npv_by_country <- function(values, rate = 0.04) {
  d <- copy(values)[service %in% c("rec", "nwfp", "wat")]
  d[, fixed_annual := annual_value_no_climate_usd[year == 2022][1], by = .(ssp, iso3, service)]
  d[, discount := (1 + rate)^(year - 2022)]
  d[, .(
    fixed = sum(fixed_annual / discount, na.rm = TRUE),
    evolving = sum(annual_value_no_climate_usd / discount, na.rm = TRUE),
    climate = sum(annual_value_usd / discount, na.rm = TRUE)
  ), by = .(ssp, iso3, country)]
}
npv_country <- rbindlist(lapply(model_specs$model, function(model) {
  z <- npv_by_country(model_values[[model]]$values)
  z[, model := model]
  z
}))
npv_global <- npv_country[, .(fixed = sum(fixed), evolving = sum(evolving), climate = sum(climate)), by = .(model, ssp)]
npv_long <- melt(npv_global, id.vars = c("model", "ssp"), variable.name = "case", value.name = "npv_usd")
npv_long[, case := factor(case, levels = c("fixed", "evolving", "climate"),
                            labels = names(case_colours))]
npv_long[, model_label := factor(model_specs$label[match(model, model_specs$model)], levels = model_specs$label)]
npv_long[, ssp_label := factor(ssp_labels[ssp], levels = unname(ssp_labels))]

fig4 <- ggplot(npv_long, aes(ssp_label, npv_usd / 1e12, fill = case)) +
  geom_col(position = position_dodge2(width = 0.82, padding = 0.07), width = 0.74) +
  facet_wrap(~model_label, nrow = 1) +
  scale_fill_manual(
    values = case_colours, drop = FALSE,
    labels = c(
      "Fixed forest\nfixed socioeconomics",
      "Fixed forest\nevolving socioeconomics",
      "Climate driven forest change\nevolving socioeconomics"
    )
  ) +
  scale_y_continuous(labels = label_number(suffix = " T", accuracy = 1)) +
  labs(x = NULL, y = "Global NPV, 2022 to 2100\n(trillion 2020 US$)") +
  theme_paper(8.5) + theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    legend.position = "bottom",
    legend.text = element_text(size = 7.5)
  )
save_figure(fig4, "Figure_4_global_noncarbon_npv.png", 7.2, 3.5)

# ---- Figure 5: country distribution of climate-driven valuation effect ------
country_impact <- copy(npv_country)[ssp == "SSP5"]
country_impact[, impact_pct := 100 * (climate - evolving) / evolving]
baseline_forest <- rbindlist(lapply(model_specs$model, function(model) {
  x <- model_values[[model]]$values[service == "rec" & year == 2022,
                                     .(baseline_forest_Mha = sum(forest_Mha, na.rm = TRUE)), by = iso3]
  x[, model := model]
  x
}))
country_impact <- merge(country_impact, baseline_forest, by = c("model", "iso3"), all.x = TRUE)
country_impact <- country_impact[is.finite(impact_pct) & baseline_forest_Mha >= 0.5]
country_impact[, model_label := factor(model_specs$label[match(model, model_specs$model)], levels = model_specs$label)]
country_impact[, rank := frank(impact_pct, ties.method = "first"), by = model]
country_impact[, direction := ifelse(impact_pct < 0, "Lower NPV", "Higher NPV")]

fig5 <- ggplot(country_impact, aes(rank, impact_pct, colour = direction)) +
  geom_hline(yintercept = 0, colour = "#777777", linewidth = 0.3) +
  geom_point(alpha = 0.8, size = 1.25) +
  facet_wrap(~model_label, nrow = 1, scales = "free_x") +
  scale_colour_manual(values = c("Lower NPV" = "#A95347", "Higher NPV" = "#356B55")) +
  labs(x = "Countries ranked by climate-driven NPV effect",
       y = "Change relative to fixed-forest NPV (%)") +
  theme_paper(9) + theme(legend.position = "top")
save_figure(fig5, "Figure_5_country_noncarbon_npv_effects_ssp585.png", 7.2, 3.1)

# ---- Machine-readable manuscript results -----------------------------------
fwrite(diagnostic, file.path(table_dir, "Table_S1_emulator_diagnostics.csv"))
fwrite(forest_global[year == 2100, .(model, ssp, forest_Mha)], file.path(table_dir, "Table_S2_global_forest_2100.csv"))
fwrite(cveg_global[year == 2100, .(model, ssp, cveg_change_GtC)], file.path(table_dir, "Table_S3_global_cveg_change_2100.csv"))
fwrite(npv_global, file.path(table_dir, "Table_S4_global_noncarbon_npv.csv"))
fwrite(country_impact[, .(model, iso3, country, baseline_forest_Mha, impact_pct)],
       file.path(table_dir, "Table_S5_country_noncarbon_npv_effect_ssp585.csv"))

message("Wrote seven figures to ", out_dir)
message("Wrote five result tables to ", table_dir)
