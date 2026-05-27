args <- commandArgs(trailingOnly = TRUE)
port <- if (length(args) >= 1) as.integer(args[[1]]) else 3840
host <- if (length(args) >= 2) args[[2]] else "127.0.0.1"
cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE))
} else {
  normalizePath("scripts", winslash = "/", mustWork = TRUE)
}
app_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Please install the shiny package before running this app.")
}

shiny::runApp(
  appDir = app_dir,
  host = host,
  port = port,
  launch.browser = FALSE
)
