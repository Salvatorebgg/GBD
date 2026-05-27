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
