args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args) >= 1) args[[1]] else ""
out_dir <- if (length(args) >= 2) args[[2]] else "outputs"

source(file.path("R", "data.R"), encoding = "UTF-8")
source(file.path("R", "analysis.R"), encoding = "UTF-8")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
gbd <- if (nzchar(input)) read_gbd_file(input) else make_example_gbd()
result <- analyze_gbd(gbd)

write_csv_excel(result$selected, file.path(out_dir, "gbd_selected_clean.csv"))
write_csv_excel(result$trend_table, file.path(out_dir, "gbd_trend_eapc_table.csv"))
writeLines(analysis_report_lines(result), file.path(out_dir, "gbd_analysis_report.txt"), useBytes = TRUE)
writeLines(manuscript_text_lines(result), file.path(out_dir, "gbd_manuscript_draft.txt"), useBytes = TRUE)

open_plot_device(file.path(out_dir, "figure_trend.png"), width = 3900, height = 2500, res = 360)
draw_trend_plot(result)
dev.off()

open_plot_device(file.path(out_dir, "figure_rank.png"), width = 3800, height = 2700, res = 360)
draw_rank_plot(result)
dev.off()

open_plot_device(file.path(out_dir, "figure_sdi_gradient.png"), width = 3700, height = 2600, res = 360)
draw_equity_plot(result)
dev.off()

open_plot_device(file.path(out_dir, "figure_forecast.png"), width = 3700, height = 2400, res = 360)
draw_forecast_plot(result)
dev.off()

message("GBD pipeline finished: ", normalizePath(out_dir, winslash = "/", mustWork = TRUE))
