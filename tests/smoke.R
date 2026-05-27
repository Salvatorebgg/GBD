source(file.path("R", "data.R"), encoding = "UTF-8")
source(file.path("R", "analysis.R"), encoding = "UTF-8")

example <- make_example_gbd()
stopifnot(nrow(example) > 1000)
stopifnot(all(c("measure", "location", "cause", "metric", "year", "val", "lower", "upper") %in% names(example)))

result <- analyze_gbd(
  example,
  measure = "Prevalence",
  cause = "Diabetes mellitus",
  metric = "Rate",
  age = "Age-standardized",
  sex = "Both",
  locations = c("Global", "China", "United States of America", "India", "Japan"),
  focus_location = "Global"
)

stopifnot(nrow(result$selected) > 0)
stopifnot(nrow(result$trend_table) >= 3)
stopifnot(nrow(result$rank_table) >= 3)
stopifnot(length(result$insights) == 3)
stopifnot(length(result$paper$methods) >= 3)
stopifnot(nrow(gbd_download_manifest()) >= 8)

minimal_public <- expand.grid(
  measure_name = "Deaths",
  location_name = c("Global", "China"),
  sex_name = "Both",
  age_group_name = c("All Ages", "Early Neonatal"),
  year_id = c(1990, 2015, 2023),
  rei_name = "Haemophilus influenzae",
  metric_name = c("Number", "Rate"),
  stringsAsFactors = FALSE
)
minimal_public$val <- seq_len(nrow(minimal_public)) + 10
minimal_public$lower <- minimal_public$val * 0.8
minimal_public$upper <- minimal_public$val * 1.2

combo <- default_filter_combo(minimal_public)
stopifnot(identical(combo$metric, "Rate"))
stopifnot(identical(combo$age, "All Ages"))

public_result <- analyze_gbd(
  minimal_public,
  measure = combo$measure,
  cause = combo$cause,
  metric = combo$metric,
  age = combo$age,
  sex = combo$sex,
  locations = c("Global", "China"),
  focus_location = "Global"
)
stopifnot(nrow(public_result$selected) == 6)
stopifnot(nrow(public_result$trend_table) == 2)

minimal_public_b <- minimal_public
minimal_public_b$rei_name <- "Streptococcus pneumoniae"
minimal_public_b$val <- minimal_public_b$val + 100
minimal_public_b$lower <- minimal_public_b$val * 0.8
minimal_public_b$upper <- minimal_public_b$val * 1.2

tmp_a <- tempfile(fileext = ".csv")
tmp_b <- tempfile(fileext = ".csv")
utils::write.csv(minimal_public, tmp_a, row.names = FALSE)
utils::write.csv(minimal_public_b, tmp_b, row.names = FALSE)
multi_public <- read_gbd_files(
  c(tmp_a, tmp_b),
  c("IHME_GBD_DEATHS_H_INFLUENZAE.csv", "IHME_GBD_DEATHS_PNEUMOCOCCUS.csv")
)
stopifnot(length(unique(multi_public$source_file)) == 2)
stopifnot(length(unique(multi_public$cause)) == 2)

multi_result <- analyze_gbd(
  multi_public,
  measure = "Deaths",
  cause = "__ALL__",
  metric = "Rate",
  age = "All Ages",
  sex = "Both",
  locations = c("Global", "China"),
  focus_location = "Global"
)
stopifnot(nrow(multi_result$selected) == 12)
stopifnot(nrow(multi_result$series) == 6)
stopifnot(nrow(gbd_source_file_summary(multi_public)) == 2)

tmp <- tempfile(fileext = ".png")
open_plot_device(tmp, width = 1200, height = 800, res = 120)
draw_trend_plot(result, preview = TRUE)
dev.off()
stopifnot(file.exists(tmp), file.info(tmp)$size > 1000)

plot_checks <- list(
  heatmap = draw_heatmap_plot,
  eapc = draw_eapc_plot,
  uncertainty = draw_uncertainty_plot,
  quadrant = draw_quadrant_plot
)

for (plot_name in names(plot_checks)) {
  tmp <- tempfile(fileext = ".png")
  open_plot_device(tmp, width = 1400, height = 950, res = 120)
  plot_checks[[plot_name]](result, preview = TRUE)
  dev.off()
  stopifnot(file.exists(tmp), file.info(tmp)$size > 1000)
}

message("GBD smoke test passed.")
