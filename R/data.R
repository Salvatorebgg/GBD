`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", as.character(x), fixed = TRUE)))
}

fmt_num <- function(x, digits = 1) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits, big.mark = ","))
}

fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "", paste0(formatC(100 * x, format = "f", digits = digits), "%"))
}

fmt_p <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
}

gbd_results_tool_url <- function() "https://vizhub.healthdata.org/gbd-results/"

gbd_api_docs_url <- function() "https://api-docs.ihme.services/"

gbd_ghdx_url <- function() "https://ghdx.healthdata.org/gbd-results-tool"

write_csv_excel <- function(x, file) {
  x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  lines <- utils::capture.output(utils::write.csv(x, row.names = FALSE, na = ""))
  con <- file(file, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  writeBin(charToRaw(paste(enc2utf8(lines), collapse = "\r\n")), con)
  writeBin(charToRaw("\r\n"), con)
}

open_plot_device <- function(file, width = 3600, height = 2400, res = 360) {
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(filename = file, width = width, height = height, units = "px", res = res, background = "white")
  } else {
    png(file, width = width, height = height, res = res, bg = "white")
  }
}

normalize_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

first_existing <- function(data, candidates) {
  nm <- normalize_name(names(data))
  candidates <- normalize_name(candidates)
  hit <- match(candidates, nm)
  hit <- hit[!is.na(hit)]
  if (length(hit) == 0) return(NA_character_)
  names(data)[hit[[1]]]
}

read_csv_smart <- function(path) {
  first <- try(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM"), silent = TRUE)
  if (!inherits(first, "try-error")) return(first)
  second <- try(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"), silent = TRUE)
  if (!inherits(second, "try-error")) return(second)
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

standardize_gbd_data <- function(data) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  fields <- list(
    measure = c("measure_name", "measure"),
    location = c("location_name", "location", "location_label", "loc_name"),
    sex = c("sex_name", "sex"),
    age = c("age_name", "age", "age_group_name"),
    cause = c("cause_name", "cause", "rei_name", "risk_name", "sequela_name"),
    metric = c("metric_name", "metric"),
    year = c("year", "year_id"),
    val = c("val", "value", "mean", "estimate"),
    upper = c("upper", "upper_ui", "upper_bound", "hi"),
    lower = c("lower", "lower_ui", "lower_bound", "lo"),
    region = c("region", "region_name", "super_region_name"),
    sdi = c("sdi", "sdi_index", "sociodemographic_index"),
    population = c("population", "pop", "population_m"),
    source_file = c("source_file", ".source_file")
  )

  out <- data.frame(.row_id = seq_len(nrow(data)), stringsAsFactors = FALSE)
  for (field in names(fields)) {
    col <- first_existing(data, fields[[field]])
    if (!is.na(col)) out[[field]] <- data[[col]]
  }

  required <- c("year", "val")
  missing_required <- setdiff(required, names(out))
  if (length(missing_required) > 0) {
    stop(paste0("GBD CSV 缺少必要字段：", paste(missing_required, collapse = ", "),
                "。至少需要 year/year_id 与 val/value。"))
  }

  n <- nrow(out)
  defaults <- list(
    measure = "Not specified",
    location = "Not specified",
    sex = "Both",
    age = "Age-standardized",
    cause = "Not specified",
    metric = "Rate",
    region = "Not specified",
    sdi = NA_real_,
    population = NA_real_,
    source_file = "Not specified"
  )
  for (field in names(defaults)) {
    if (!field %in% names(out)) out[[field]] <- rep(defaults[[field]], n)
  }

  out$year <- as.integer(safe_numeric(out$year))
  out$val <- safe_numeric(out$val)
  if (!"lower" %in% names(out)) out$lower <- out$val * 0.90
  if (!"upper" %in% names(out)) out$upper <- out$val * 1.10
  out$lower <- safe_numeric(out$lower)
  out$upper <- safe_numeric(out$upper)
  out$sdi <- safe_numeric(out$sdi)
  out$population <- safe_numeric(out$population)

  text_fields <- c("measure", "location", "sex", "age", "cause", "metric", "region", "source_file")
  for (field in text_fields) out[[field]] <- trimws(as.character(out[[field]]))
  out <- out[is.finite(out$year) & is.finite(out$val), , drop = FALSE]
  out$.row_id <- NULL
  rownames(out) <- NULL
  out
}

is_supported_gbd_file <- function(name) {
  tolower(tools::file_ext(name)) %in% c("csv", "tsv", "txt")
}

read_gbd_file <- function(path, name = basename(path)) {
  ext <- tolower(tools::file_ext(name))
  if (ext == "zip") return(read_gbd_zip(path, name))
  if (ext %in% c("csv", "txt")) {
    out <- standardize_gbd_data(read_csv_smart(path))
    out$source_file <- basename(name)
    return(out)
  }
  if (ext %in% c("tsv")) {
    out <- standardize_gbd_data(utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE))
    out$source_file <- basename(name)
    return(out)
  }
  stop("目前支持 IHME/GBD Results Tool 导出的 CSV/TSV/TXT 文件，或包含这些文件的 ZIP 压缩包。")
}

read_gbd_files <- function(paths, names = basename(paths)) {
  if (length(paths) == 0) stop("没有可读取的 GBD 文件。")
  pieces <- lapply(seq_along(paths), function(i) read_gbd_file(paths[[i]], names[[i]]))
  pieces <- pieces[vapply(pieces, nrow, integer(1)) > 0]
  if (length(pieces) == 0) stop("上传文件中没有可分析记录。")
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  standardize_gbd_data(out)
}

read_gbd_zip <- function(path, name = basename(path)) {
  listing <- utils::unzip(path, list = TRUE)
  if (!nrow(listing)) stop("ZIP 压缩包为空。")
  files <- listing$Name[!grepl("/$", listing$Name) & is_supported_gbd_file(listing$Name)]
  if (length(files) == 0) stop("ZIP 中没有 CSV/TSV/TXT 数据文件。")
  exdir <- tempfile("gbd_zip_")
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  extracted <- utils::unzip(path, files = files, exdir = exdir, junkpaths = FALSE)
  labels <- paste0(basename(name), "::", basename(files))
  read_gbd_files(extracted, labels)
}

read_gbd_url <- function(url) {
  if (!grepl("^https?://", url, ignore.case = TRUE)) stop("请输入 http 或 https 开头的 CSV 直链。")
  tmp <- tempfile(fileext = ".csv")
  utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
  read_gbd_file(tmp, basename(url))
}

gbd_topic_catalog <- function() {
  data.frame(
    topic = c("diabetes_prevalence", "ihd_deaths", "stroke_dalys", "ckd_dalys", "copd_deaths", "depression_prevalence"),
    title = c("糖尿病患病率", "缺血性心脏病死亡率", "卒中 DALYs", "慢性肾病 DALYs", "COPD 死亡率", "抑郁障碍患病率"),
    cause = c("Diabetes mellitus", "Ischemic heart disease", "Stroke", "Chronic kidney disease", "Chronic obstructive pulmonary disease", "Depressive disorders"),
    measure = c("Prevalence", "Deaths", "DALYs", "DALYs", "Deaths", "Prevalence"),
    metric = rep("Rate", 6),
    age = rep("Age-standardized", 6),
    sex = rep("Both", 6),
    clinical_line = c(
      "适合做代谢病负担、地区差异和公共卫生趋势。",
      "临床意义直接，适合死亡负担、变化率和国家排序。",
      "疾病负担完整，DALYs 兼顾死亡和伤残。",
      "适合连接糖尿病、高血压和肾脏结局。",
      "慢病防控主题清楚，图形容易出趋势。",
      "非致死性负担突出，适合强调 YLD/患病率。"
    ),
    stringsAsFactors = FALSE
  )
}

gbd_download_manifest <- function(topic = "diabetes_prevalence", gbd_round = "GBD 2023 或下载文件对应版本") {
  catalog <- gbd_topic_catalog()
  row <- catalog[catalog$topic == topic, , drop = FALSE]
  if (nrow(row) == 0) row <- catalog[1, , drop = FALSE]
  data.frame(
    item = c("官方入口", "数据库版本", "Measure", "Cause", "Metric", "Age", "Sex", "Year", "Location", "导出格式", "可选 API"),
    value = c(
      gbd_results_tool_url(),
      gbd_round,
      row$measure[[1]],
      row$cause[[1]],
      row$metric[[1]],
      row$age[[1]],
      row$sex[[1]],
      "1990-latest",
      "Global + target countries/regions",
      "CSV",
      gbd_api_docs_url()
    ),
    note = c(
      "进入 Results Tool 后按下方字段筛选并导出 CSV。",
      "以 IHME 页面和导出文件中的版本为准。",
      "GBD 的核心维度之一，决定患病率、死亡、DALYs 等。",
      row$clinical_line[[1]],
      "Rate 常用于年龄标化率；Number 可报告绝对负担。",
      "临床论文常先用 Age-standardized 便于跨地区比较。",
      "Both 用于主分析；Male/Female 可做亚组。",
      "尽量下载完整年份，便于 EAPC 和预测。",
      "包含 Global 便于参照；国家/地区用于排序和比较。",
      "本项目直接读取 Results Tool CSV，不改原始文件。",
      "Portal API 往往需要凭证和签名，适合单位账号或自动化部署。"
    ),
    stringsAsFactors = FALSE
  )
}

gbd_variable_template <- function() {
  data.frame(
    field = c("project", "gbd_round", "measure", "cause_or_rei", "metric", "age", "sex", "years", "locations", "clinical_question", "analysis_note"),
    value = c("", "", "", "", "", "", "", "", "", "", ""),
    example = c(
      "CKD burden in adults",
      "GBD 2023",
      "DALYs",
      "Chronic kidney disease",
      "Rate",
      "Age-standardized",
      "Both",
      "1990-2023",
      "Global; China; United States of America",
      "How has CKD burden changed across countries?",
      "Use age-standardized rate as primary outcome; Number as supplement."
    ),
    stringsAsFactors = FALSE
  )
}

gbd_field_summary <- function(data) {
  data <- standardize_gbd_data(data)
  fields <- c("measure", "cause", "metric", "age", "sex", "location", "region", "source_file")
  rows <- lapply(fields, function(field) {
    vals <- sort(unique(data[[field]]))
    data.frame(
      field = field,
      n = length(vals),
      examples = paste(head(vals, 8), collapse = "; "),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- rbind(
    data.frame(field = "year", n = length(unique(data$year)),
               examples = paste0(min(data$year), "-", max(data$year)), stringsAsFactors = FALSE),
    out
  )
  rownames(out) <- NULL
  out
}

gbd_source_file_summary <- function(data) {
  data <- standardize_gbd_data(data)
  files <- sort(unique(data$source_file))
  files <- files[nzchar(files)]
  rows <- lapply(files, function(file) {
    d <- data[data$source_file == file, , drop = FALSE]
    causes <- sort(unique(d$cause))
    metrics <- sort(unique(d$metric))
    ages <- sort(unique(d$age))
    data.frame(
      source_file = file,
      rows = nrow(d),
      causes = paste(head(causes, 3), collapse = "; "),
      metrics = paste(metrics, collapse = "; "),
      ages = paste(head(ages, 3), collapse = "; "),
      years = paste0(min(d$year), "-", max(d$year)),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

make_example_gbd <- function(seed = 20260518) {
  set.seed(seed)
  years <- 1990:2023
  locations <- data.frame(
    location = c("Global", "China", "United States of America", "Japan", "India", "Brazil", "United Kingdom", "Germany", "Singapore", "Republic of Korea", "Australia", "Canada", "France"),
    region = c("World", "East Asia", "High-income North America", "High-income Asia Pacific", "South Asia", "Latin America", "Western Europe", "Western Europe", "High-income Asia Pacific", "East Asia", "Australasia", "High-income North America", "Western Europe"),
    sdi = c(0.64, 0.71, 0.88, 0.86, 0.52, 0.69, 0.86, 0.87, 0.91, 0.84, 0.87, 0.88, 0.86),
    population = c(7950, 1410, 335, 124, 1430, 216, 68, 84, 6, 52, 26, 40, 65),
    loc_shift = c(1.00, 0.94, 1.15, 0.78, 1.26, 1.05, 0.89, 0.86, 0.72, 0.83, 0.80, 0.84, 0.87),
    slope_shift = c(0.000, 0.004, -0.006, -0.010, 0.009, 0.002, -0.008, -0.009, -0.011, -0.004, -0.007, -0.008, -0.007),
    stringsAsFactors = FALSE
  )
  causes <- data.frame(
    cause = c("Diabetes mellitus", "Ischemic heart disease", "Stroke", "Chronic kidney disease", "Chronic obstructive pulmonary disease", "Depressive disorders"),
    measure = c("Prevalence", "Deaths", "DALYs", "DALYs", "Deaths", "Prevalence"),
    base = c(5200, 145, 2850, 780, 58, 3900),
    slope = c(0.022, -0.004, -0.008, 0.010, -0.012, 0.004),
    curve = c(0.10, -0.04, -0.03, 0.08, -0.05, 0.02),
    stringsAsFactors = FALSE
  )
  sexes <- data.frame(
    sex = c("Both", "Male", "Female"),
    sex_mult = c(1.00, 1.08, 0.94),
    stringsAsFactors = FALSE
  )

  grid <- expand.grid(
    year = years,
    location = locations$location,
    cause = causes$cause,
    sex = sexes$sex,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  grid <- merge(grid, locations, by = "location", sort = FALSE)
  grid <- merge(grid, causes, by = "cause", sort = FALSE)
  grid <- merge(grid, sexes, by = "sex", sort = FALSE)
  t <- grid$year - min(years)
  noise <- rnorm(nrow(grid), 0, 0.025)
  smooth_wave <- 0.035 * sin((t + match(grid$location, locations$location)) / 5.2)
  log_rate <- log(grid$base * grid$loc_shift * grid$sex_mult) +
    (grid$slope + grid$slope_shift) * t +
    grid$curve * (t / max(t))^2 +
    smooth_wave + noise
  val <- pmax(0.01, exp(log_rate))
  ui <- 0.08 + 0.02 * abs(sin(t / 4 + grid$sdi))
  data.frame(
    measure = grid$measure,
    location = grid$location,
    sex = grid$sex,
    age = "Age-standardized",
    cause = grid$cause,
    metric = "Rate",
    year = grid$year,
    val = round(val, 3),
    upper = round(val * exp(ui), 3),
    lower = round(val * exp(-ui), 3),
    region = grid$region,
    sdi = grid$sdi,
    population = grid$population,
    stringsAsFactors = FALSE
  )
}

available_choices <- function(data, field, add_all = TRUE) {
  data <- standardize_gbd_data(data)
  vals <- sort(unique(data[[field]]))
  vals <- vals[nzchar(vals)]
  if (isTRUE(add_all)) c("All" = "__ALL__", vals) else vals
}

default_selection <- function(data, field) {
  data <- standardize_gbd_data(data)
  vals <- sort(unique(data[[field]]))
  vals <- vals[nzchar(vals)]
  if (length(vals) == 0) return("")
  preferred <- switch(
    field,
    measure = c("Deaths", "DALYs", "Prevalence", "Incidence"),
    metric = c("Rate", "Number", "Percent"),
    age = c("Age-standardized", "All ages", "All Ages"),
    sex = c("Both", "Male", "Female"),
    vals
  )
  vals_lower <- tolower(vals)
  for (one in preferred) {
    hit <- which(vals_lower == tolower(one))
    if (length(hit) > 0) return(vals[[hit[[1]]]])
  }
  vals[[1]]
}

default_filter_combo <- function(data) {
  data <- standardize_gbd_data(data)
  catalog <- gbd_topic_catalog()
  for (i in seq_len(nrow(catalog))) {
    hit <- data[
      data$measure == catalog$measure[[i]] &
        data$cause == catalog$cause[[i]] &
        data$metric == catalog$metric[[i]] &
        data$age == catalog$age[[i]] &
        data$sex == catalog$sex[[i]],
      ,
      drop = FALSE
    ]
    if (nrow(hit) > 0) {
      return(as.list(catalog[i, c("measure", "cause", "metric", "age", "sex")]))
    }
  }
  first <- data[1, , drop = FALSE]
  list(
    measure = default_selection(data, "measure"),
    cause = first$cause[[1]],
    metric = default_selection(data, "metric"),
    age = default_selection(data, "age"),
    sex = default_selection(data, "sex")
  )
}

gbd_filter_data <- function(data, measure = NULL, cause = NULL, metric = NULL, age = NULL, sex = NULL, locations = NULL) {
  data <- standardize_gbd_data(data)
  keep <- rep(TRUE, nrow(data))
  apply_filter <- function(field, value) {
    if (is.null(value) || length(value) == 0 || any(value == "__ALL__") || all(!nzchar(value))) return(rep(TRUE, nrow(data)))
    data[[field]] %in% value
  }
  keep <- keep & apply_filter("measure", measure)
  keep <- keep & apply_filter("cause", cause)
  keep <- keep & apply_filter("metric", metric)
  keep <- keep & apply_filter("age", age)
  keep <- keep & apply_filter("sex", sex)
  keep <- keep & apply_filter("location", locations)
  data[keep, , drop = FALSE]
}

make_download_code <- function(topic = "diabetes_prevalence") {
  manifest <- gbd_download_manifest(topic)
  paste(
    "# 1. Open the IHME GBD Results Tool and export CSV",
    paste0("# Results Tool: ", gbd_results_tool_url()),
    "",
    "# 2. Use these filters",
    paste(capture.output(print(manifest[, c("item", "value")], row.names = FALSE)), collapse = "\n"),
    "",
    "# 3. Read the exported CSV in this project",
    "source(file.path('R', 'data.R'), encoding = 'UTF-8')",
    "source(file.path('R', 'analysis.R'), encoding = 'UTF-8')",
    "gbd <- read_gbd_file('gbd_export.csv')",
    "result <- analyze_gbd(gbd)",
    "",
    "# 4. Save manuscript-ready figures",
    "open_plot_device('figure_trend.png'); draw_trend_plot(result); dev.off()",
    "open_plot_device('figure_rank.png'); draw_rank_plot(result); dev.off()",
    "write_csv_excel(result$trend_table, 'table_trends.csv')",
    sep = "\n"
  )
}

manuscript_checklist <- function() {
  data.frame(
    item = c("GBD 版本", "维度声明", "年龄标准化", "不确定区间", "EAPC 模型", "地区选择", "因果边界", "图表脚注", "可重复性", "数据可得性"),
    check = c(
      "写清 Global Burden of Disease Study 的年份或导出版本。",
      "Methods 中列出 measure、metric、age、sex、cause/risk、locations、years。",
      "跨地区比较优先使用 age-standardized rate；绝对负担另报 Number。",
      "图表和结果段落同时报告 lower/upper uncertainty interval。",
      "说明 EAPC 来自 log(rate) 对年份的线性回归。",
      "说明纳入国家/地区的规则，不只挑显著结果。",
      "GBD 是建模估计结果，避免写成个体层面因果效应。",
      "每张图脚注说明单位、年龄、性别、metric 和年份。",
      "保存导出的原始 CSV、筛选清单、清洗脚本和版本信息。",
      "Data availability 指向 IHME/GHDx/Results Tool 官方入口。"
    ),
    stringsAsFactors = FALSE
  )
}

beginner_flow_steps <- function() {
  data.frame(
    step = sprintf("%02d", 1:7),
    title = c("先定临床问题", "把问题翻译成 GBD 字段", "打开官网", "筛选并导出 CSV", "上传到本工具", "一键清洗分析绘图", "复制写作骨架"),
    goal = c(
      "明确疾病、指标、地区、年份和人群，不急着下载。",
      "把中文问题拆成 Measure、Cause/Risk、Metric、Age、Sex、Location。",
      "从 IHME GBD Results Tool 进入，登录后才能下载。",
      "保持字段简单，先下载主分析数据，再补充 Number 或 Sex 亚组。",
      "上传官网导出的 CSV；系统自动识别常见字段。",
      "自动生成 EAPC、趋势、排序、SDI 梯度、预测和出版级图片。",
      "输出 Methods、Results、Discussion 要点和投稿核对清单。"
    ),
    deliverable = c(
      "一句研究问题",
      "下载参数表",
      "官网页面",
      "原始 CSV",
      "清洗后数据",
      "表格与 PNG",
      "论文草稿"
    ),
    stringsAsFactors = FALSE
  )
}

clinical_question_framework <- function() {
  data.frame(
    question = c("研究对象", "疾病或风险因素", "指标", "表达方式", "人群", "地区", "年份", "对照或重点"),
    plain_language = c(
      "你想研究哪类病或风险？",
      "GBD 里对应 Cause、Impairment、Risk 或 Etiology。",
      "想看发病、患病、死亡、YLD、YLL 还是 DALYs？",
      "Rate 适合比较，Number 适合说明绝对负担，Percent 适合归因比例。",
      "Both 做主分析，Male/Female 或年龄组做亚组。",
      "Global 加目标国家/地区，方便参照。",
      "尽量选完整年份，从 1990 到最新可用年份。",
      "例如中国 vs 全球、男女差异、高 SDI vs 低 SDI。"
    ),
    gbd_field = c("Cause/Risk", "Cause/Risk", "Measure", "Metric", "Age/Sex", "Location", "Year", "Location/Sex/Age/SDI"),
    example = c(
      "Chronic kidney disease",
      "High systolic blood pressure",
      "DALYs",
      "Age-standardized rate",
      "Both, Age-standardized",
      "Global; China; United States of America",
      "1990-2021 或导出文件最新年份",
      "China compared with Global"
    ),
    stringsAsFactors = FALSE
  )
}

gbd_beginner_research_templates <- function() {
  data.frame(
    type = c("负担现状", "长期趋势", "地区差异", "性别/年龄差异", "风险归因", "预测展示"),
    good_question = c(
      "[疾病] 在 [地区] 的最新年龄标化负担是多少？",
      "[疾病] 在 1990 年以来是否上升或下降？",
      "哪些国家/地区的 [疾病] 负担最高，变化最快？",
      "男性和女性、不同年龄组的趋势是否一致？",
      "[风险因素] 对 [结局] 的归因负担有多大？",
      "如果现有趋势延续，到 2030 年负担大概如何？"
    ),
    recommended_fields = c(
      "Measure=DALYs/Deaths/Prevalence; Metric=Rate; Age=Age-standardized",
      "Year=1990-latest; Metric=Rate; 用 EAPC",
      "Location=Global + 多国家/地区; Metric=Rate",
      "Sex=Male/Female 或 Age=分年龄组；先做 Both 主分析",
      "Measure=Attributable DALYs/Deaths; Risk=目标风险因素",
      "完整时间序列；至少 10 个年份更稳"
    ),
    beginner_note = c(
      "最容易入手，图表清楚。",
      "GBD 论文最常见套路，EAPC 是核心表。",
      "适合做高级图，但别只挑显著地区。",
      "适合做 Supplement 或亚组图。",
      "字段更复杂，先用疾病负担练手。",
      "只能做趋势外推，不能当严格预测模型。"
    ),
    stringsAsFactors = FALSE
  )
}

gbd_official_download_steps <- function() {
  data.frame(
    step = sprintf("步骤 %d", 1:9),
    action = c(
      "打开 IHME GBD Results Tool",
      "登录或注册 IHME 账号",
      "选择 GBD 数据集或结果工具默认版本",
      "设置 Measure",
      "设置 Cause / Risk / Impairment",
      "设置 Metric、Age、Sex",
      "设置 Years 和 Locations",
      "点击 Search / Apply 查看结果",
      "点击 Download CSV 保存文件"
    ),
    detail = c(
      "优先使用 https://vizhub.healthdata.org/gbd-results/。页面加载慢时换 Chrome/Edge 或刷新。",
      "IHME 官方说明要求创建账号后才能搜索和下载 GBD 数据。",
      "以页面显示和导出文件为准；论文 Methods 里写清版本。",
      "常用：Prevalence、Incidence、Deaths、YLDs、YLLs、DALYs。",
      "输入英文关键词，例如 diabetes、stroke、chronic kidney disease。",
      "小白主分析建议：Metric=Rate，Age=Age-standardized，Sex=Both。",
      "年份尽量全选；地区至少包含 Global 和目标国家/地区。",
      "先确认表格中有 year、location、val、upper、lower。",
      "下载后的 CSV 不要手工改列名，直接上传到本工具。"
    ),
    common_mistake = c(
      "进错到 GBD Compare 只看图，没导出 CSV。",
      "未登录导致下载按钮不可用。",
      "忘记记录 GBD 版本。",
      "Measure 和 Cause 组合不存在，结果为空。",
      "疾病名称拼写不同，需用官网下拉项。",
      "把 Number 当作跨国比较主指标。",
      "只下载某一年，无法做趋势。",
      "只看网页图，不保存原始 CSV。",
      "用 Excel 打开后保存导致编码或列名变化。"
    ),
    stringsAsFactors = FALSE
  )
}

upload_cleaning_steps <- function() {
  data.frame(
    stage = c("读取", "字段识别", "筛选", "清洗", "趋势建模", "图形输出", "写作输出"),
    what_system_does = c(
      "读取 CSV/TSV 或 CSV 直链。",
      "识别 measure、location、sex、age、cause、metric、year、val、upper、lower。",
      "按左侧 Measure/Cause/Metric/Age/Sex/Location 筛选。",
      "去掉 year 或 val 缺失的行，统一数值和文本字段。",
      "按地区拟合 log(rate) ~ year，计算 EAPC 和 95%CI。",
      "生成趋势图、起止变化图、SDI 梯度图、近端预测图。",
      "自动形成 Methods、Results、Limitations、投稿核对清单。"
    ),
    what_user_checks = c(
      "确认上传的是官网下载原始 CSV。",
      "看字段体检表，确认年份和地区数正常。",
      "确认筛选后记录不是 0。",
      "确认 latest year、UI、单位和 GBD 版本。",
      "EAPC 方向是否符合常识，异常地区回到 CSV 检查。",
      "图题、图注、单位、地区是否符合论文问题。",
      "把自动段落改成自己的语言，补充临床解释和文献。"
    ),
    stringsAsFactors = FALSE
  )
}

gbd_figure_explainer <- function() {
  data.frame(
    output = c("趋势图", "起止变化图", "趋势表/EAPC", "SDI 梯度图", "预测图", "写作草稿"),
    use_for = c(
      "主图，展示焦点地区和高负担地区的长期变化。",
      "直观看哪些地区升降明显，适合 Figure 2 或补充图。",
      "核心结果表，写 Results 时优先引用。",
      "展示负担与社会发展水平的生态关系。",
      "讨论未来压力，作为探索性结果。",
      "快速搭建 Methods、Results、Limitations。"
    ),
    manuscript_note = c(
      "图注写清 Measure、Metric、Age、Sex、Years。",
      "不要只解释最高地区，也要解释变化最快地区。",
      "EAPC 是平均年度变化，不等于每年都线性变化。",
      "只做生态描述，不写个体因果。",
      "写成 scenario screening，避免过度预测。",
      "必须人工补充临床机制和已有研究对照。"
    ),
    stringsAsFactors = FALSE
  )
}

discussion_scaffold <- function() {
  data.frame(
    paragraph = c("第一段：主发现", "第二段：和既往研究比较", "第三段：可能机制", "第四段：临床和公共卫生意义", "第五段：优势与局限"),
    write_what = c(
      "用 2-3 句话说清最新负担、趋势方向、地区差异。",
      "找 3-5 篇同主题 GBD 或临床流行病学文献，说明一致与不一致。",
      "从疾病自然史、危险因素、筛查治疗可及性、人口老龄化解释趋势。",
      "说明哪些人群或地区需要筛查、预防、资源配置。",
      "优势写全球可比和长时间序列；局限写模型估计、输入数据差异、生态分析、残余不确定性。"
    ),
    avoid = c(
      "不要重复 Results 的所有数字。",
      "不要只说“与前人一致”，要解释为什么一致或不同。",
      "不要把 GBD 生态趋势写成个体因果机制已经被证明。",
      "不要泛泛写“加强管理”，要对应结果。",
      "不要把局限写成套话，至少对应本研究数据结构。"
    ),
    stringsAsFactors = FALSE
  )
}

beginner_manual_lines <- function(topic = "diabetes_prevalence") {
  manifest <- gbd_download_manifest(topic)
  c(
    "GBD 小白一键式工作流",
    "",
    "一、先定题",
    "1. 用一句话写清：我想研究 [疾病/风险] 在 [地区/人群] 中 [某指标] 从 [年份] 到 [年份] 的变化。",
    "2. 初学者优先选择：Age-standardized Rate + Both sex + 1990 到最新年份 + Global 和目标国家。",
    "3. 先做疾病负担，不建议第一篇就做复杂风险归因或多年龄组大表。",
    "",
    "二、去官网下载",
    paste0("1. 打开：", gbd_results_tool_url()),
    "2. 登录或注册 IHME 账号。",
    "3. 按下面参数筛选：",
    paste(capture.output(print(manifest[, c("item", "value")], row.names = FALSE)), collapse = "\n"),
    "4. 点击 Search / Apply 后检查结果表。",
    "5. 点击 Download CSV。下载后不要改列名，保存原始文件。",
    "",
    "三、上传到本工具",
    "1. 打开 Shiny 页面，左侧选择“上传 CSV”。",
    "2. 上传官网下载的 CSV。",
    "3. 在左侧确认 Measure、Cause、Metric、Age、Sex、Locations。",
    "4. 系统会自动清洗、筛选、计算 EAPC 并生成图表。",
    "",
    "四、分析与绘图",
    "1. 先看概览：确认样本记录、地区数、年份范围。",
    "2. 再看趋势图：确认焦点地区和主要国家走势。",
    "3. 再看趋势表：引用 latest value、UI、percent change、EAPC。",
    "4. 最后看 SDI 和预测图：只做描述和讨论，不作个体因果。",
    "",
    "五、写作",
    "1. Methods 写清 GBD 版本、下载字段、年龄/性别/地区、EAPC 计算方法。",
    "2. Results 先写主地区，再写最高负担和变化最快地区。",
    "3. Discussion 解释趋势背后的临床和公共卫生原因。",
    "4. Limitations 必须写：GBD 是建模估计，输入数据质量不一，生态分析不能推个体因果。"
  )
}

gbd_route_cards <- function() {
  data.frame(
    step = c("01", "02", "03", "04", "05"),
    title = c("定题", "官网下载", "上传", "自动分析", "写作"),
    text = c(
      "写清疾病/风险、指标、地区、年份和人群。",
      "在 IHME Results Tool 按字段筛选并导出 CSV。",
      "上传官网 CSV，不改列名、不删 upper/lower。",
      "系统自动清洗、算 EAPC、出图出表出结论。",
      "复制 Methods/Results/Discussion 模板后人工润色。"
    ),
    stringsAsFactors = FALSE
  )
}

gbd_readiness_questions <- function() {
  data.frame(
    自查问题 = c(
      "我的问题能否写成“某疾病/风险在某地区的某负担指标趋势”？",
      "我是否知道要用 Prevalence、Incidence、Deaths、YLDs、YLLs 还是 DALYs？",
      "跨地区比较是否优先使用 Age-standardized Rate？",
      "是否下载了完整年份，而不是只下最新一年？",
      "是否保留了 val、lower、upper 三个估计字段？",
      "结论是否只写 population-level burden，而不写个体因果？"
    ),
    合格标准 = c(
      "例如：1990年以来中国慢性肾病年龄标化 DALYs 率变化。",
      "疾病负担常用 DALYs/Deaths，疾病频率常用 Prevalence/Incidence。",
      "Rate 适合比较，Number 适合补充绝对负担。",
      "至少 10 个年份，最好 1990 到最新可用年份。",
      "不确定区间是 GBD 结果解释的基本组成。",
      "GBD 是模型估计和生态层面数据，不能替代个体队列研究。"
    ),
    产出 = c("研究题目", "Measure", "Metric", "EAPC", "UI", "局限性"),
    stringsAsFactors = FALSE
  )
}

gbd_database_fit_table <- function() {
  data.frame(
    适合程度 = c("非常适合", "适合但需谨慎", "不太适合", "不适合"),
    研究类型 = c(
      "疾病负担趋势、死亡率/DALYs/患病率变化、国家和地区比较",
      "SDI 梯度、风险归因、年龄/性别亚组、短期外推",
      "个体危险因素与结局的因果关系、药物疗效、临床预测模型",
      "治疗方案比较、医院病例结局、影像/检验原始数据分析"
    ),
    原因 = c(
      "GBD 提供长期、全球可比、带不确定区间的标准化估计。",
      "需要写清生态分析边界和模型估计不确定性。",
      "GBD 没有个体层面暴露、协变量和结局随访。",
      "这些问题需要临床试验、队列、病历或注册数据库。"
    ),
    写作边界 = c(
      "可写 burden、trend、inequality、EAPC。",
      "可写 descriptive association，不写 individual causality。",
      "只适合作为背景或公共卫生负担补充。",
      "建议更换数据源。"
    ),
    stringsAsFactors = FALSE
  )
}

gbd_measure_guide <- function() {
  data.frame(
    Measure = c("Prevalence", "Incidence", "Deaths", "YLDs", "YLLs", "DALYs"),
    中文理解 = c("现患负担", "新发负担", "死亡负担", "伤残生存损失", "早死损失", "总疾病负担"),
    适合问题 = c(
      "慢病、精神障碍、长期状态的现状和趋势。",
      "新发疾病风险变化，例如癌症、感染性疾病。",
      "死亡结局清晰、临床意义强的疾病。",
      "非致死但影响生活质量的疾病。",
      "死亡提前造成的寿命损失。",
      "综合死亡和伤残，最常用于疾病负担论文。"
    ),
    新手建议 = c("入门友好", "需要注意诊断口径", "直观好写", "适合补充", "适合死亡主导疾病", "最通用"),
    stringsAsFactors = FALSE
  )
}

gbd_download_resource_catalog <- function() {
  data.frame(
    资源 = c("官方下载清单 CSV", "本地演示数据 CSV", "变量记录模板 CSV", "可复现 R 代码", "小白操作手册", "投稿核对清单"),
    用途 = c(
      "照着在 IHME Results Tool 里选择 Measure/Cause/Metric/Age/Sex/Location。",
      "不联网也能体验上传、清洗、分析、绘图和写作。",
      "记录 GBD 版本、下载条件、变量口径和论文题目。",
      "离开网页后用 Rscript 复现结果。",
      "从定题到上传写作的完整步骤。",
      "投稿前检查 GBD 版本、UI、EAPC、图注、局限性。"
    ),
    stringsAsFactors = FALSE
  )
}
