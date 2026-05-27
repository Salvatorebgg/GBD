require_plot_packages <- function() {
  requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("scales", quietly = TRUE)
}

clinical_theme <- function() {
  list(
    ink = "#142A2E",
    muted = "#687C82",
    grid = "#E7ECEA",
    panel = "#FFFFFF",
    paper = "#F7FAF8",
    teal = "#0E7C7B",
    mint = "#8AC6B4",
    navy = "#274C77",
    red = "#B34D3E",
    amber = "#D59F32",
    violet = "#665C9E",
    blue = "#4D86B8"
  )
}

clinical_cols <- function(n) {
  pal <- c("#0E7C7B", "#B34D3E", "#274C77", "#D59F32", "#665C9E", "#4D86B8", "#5F8D4E", "#C76E52", "#59788E", "#A5688F")
  rep(pal, length.out = n)
}

plot_family <- function() {
  if (.Platform$OS.type == "windows") "Microsoft YaHei" else "sans"
}

publication_theme <- function(base_size = 13, grid_y = TRUE) {
  th <- clinical_theme()
  ggplot2::theme_minimal(base_family = plot_family(), base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = th$grid, linewidth = 0.42),
      panel.grid.major.y = if (isTRUE(grid_y)) ggplot2::element_line(colour = th$grid, linewidth = 0.35) else ggplot2::element_blank(),
      axis.title = ggplot2::element_text(colour = th$ink, face = "bold", size = base_size - 1),
      axis.text = ggplot2::element_text(colour = th$ink, size = base_size - 2),
      plot.title = ggplot2::element_text(colour = th$ink, face = "bold", size = base_size + 5, margin = ggplot2::margin(b = 6)),
      plot.subtitle = ggplot2::element_text(colour = th$muted, size = base_size - 1, lineheight = 1.18, margin = ggplot2::margin(b = 12)),
      plot.caption = ggplot2::element_text(colour = th$muted, size = base_size - 4, hjust = 0, margin = ggplot2::margin(t = 12)),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(colour = th$ink, size = base_size - 2),
      plot.margin = ggplot2::margin(18, 26, 18, 18)
    )
}

short_label <- function(x, width = 18) {
  vapply(as.character(x), function(one) paste(strwrap(one, width = width), collapse = "\n"), character(1))
}

draw_plot_message <- function(title, lines) {
  th <- clinical_theme()
  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(fill = th$paper, col = NA))
  grid::grid.roundrect(
    x = 0.5, y = 0.5, width = 0.84, height = 0.46,
    r = grid::unit(10, "pt"),
    gp = grid::gpar(fill = th$panel, col = "#DCE8E3", lwd = 1.2)
  )
  grid::grid.text(title, x = 0.16, y = 0.62, just = "left", gp = grid::gpar(col = th$ink, fontsize = 18, fontface = "bold", fontfamily = plot_family()))
  y <- 0.53
  for (line in lines) {
    grid::grid.text(paste(strwrap(line, width = 48), collapse = "\n"), x = 0.16, y = y, just = c("left", "center"), gp = grid::gpar(col = th$muted, fontsize = 11, lineheight = 1.2, fontfamily = plot_family()))
    y <- y - 0.08
  }
}

collapse_gbd_series <- function(data) {
  data <- standardize_gbd_data(data)
  key <- c("location", "year", "region")
  sum_or_na <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0) NA_real_ else sum(x)
  }
  mean_or_na <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0) NA_real_ else mean(x)
  }
  burden <- aggregate(
    data[c("val", "lower", "upper")],
    by = data[key],
    FUN = sum_or_na
  )
  covars <- aggregate(
    data[c("sdi", "population")],
    by = data[key],
    FUN = mean_or_na
  )
  grouped <- merge(burden, covars, by = key, all.x = TRUE, sort = FALSE)
  grouped <- grouped[order(grouped$location, grouped$year), , drop = FALSE]
  rownames(grouped) <- NULL
  grouped
}

fit_eapc <- function(d) {
  d <- d[is.finite(d$year) & is.finite(d$val) & d$val > 0, , drop = FALSE]
  if (nrow(d) < 3 || length(unique(d$year)) < 3) {
    return(c(eapc = NA_real_, low = NA_real_, high = NA_real_, p = NA_real_))
  }
  fit <- try(stats::lm(log(val) ~ year, data = d), silent = TRUE)
  if (inherits(fit, "try-error")) return(c(eapc = NA_real_, low = NA_real_, high = NA_real_, p = NA_real_))
  sm <- summary(fit)
  beta <- stats::coef(fit)[["year"]]
  se <- sm$coefficients["year", "Std. Error"]
  p <- sm$coefficients["year", "Pr(>|t|)"]
  c(
    eapc = 100 * (exp(beta) - 1),
    low = 100 * (exp(beta - 1.96 * se) - 1),
    high = 100 * (exp(beta + 1.96 * se) - 1),
    p = p
  )
}

trend_one_location <- function(d, start_year, end_year) {
  d <- d[order(d$year), , drop = FALSE]
  d_window <- d[d$year >= start_year & d$year <= end_year, , drop = FALSE]
  if (nrow(d_window) == 0) return(NULL)
  first <- d_window[which.min(d_window$year), , drop = FALSE]
  last <- d_window[which.max(d_window$year), , drop = FALSE]
  e <- fit_eapc(d_window)
  data.frame(
    location = last$location[[1]],
    region = last$region[[1]],
    start_year = first$year[[1]],
    end_year = last$year[[1]],
    start_value = first$val[[1]],
    latest_value = last$val[[1]],
    lower = last$lower[[1]],
    upper = last$upper[[1]],
    absolute_change = last$val[[1]] - first$val[[1]],
    percent_change = ifelse(first$val[[1]] == 0, NA_real_, last$val[[1]] / first$val[[1]] - 1),
    eapc = e[["eapc"]],
    eapc_low = e[["low"]],
    eapc_high = e[["high"]],
    eapc_p = e[["p"]],
    sdi = last$sdi[[1]],
    population = last$population[[1]],
    stringsAsFactors = FALSE
  )
}

make_forecast <- function(series, horizon = 2030) {
  series <- series[is.finite(series$year) & is.finite(series$val) & series$val > 0, , drop = FALSE]
  if (nrow(series) < 3 || length(unique(series$year)) < 3) return(data.frame())
  max_year <- max(series$year)
  future_years <- seq(max_year, horizon)
  fit <- stats::lm(log(val) ~ year, data = series)
  pred <- stats::predict(fit, newdata = data.frame(year = future_years), se.fit = TRUE)
  data.frame(
    year = future_years,
    val = exp(pred$fit),
    lower = exp(pred$fit - 1.96 * pred$se.fit),
    upper = exp(pred$fit + 1.96 * pred$se.fit),
    type = ifelse(future_years <= max_year, "Observed", "Forecast"),
    stringsAsFactors = FALSE
  )
}

collapse_unique_terms <- function(x, max_n = 6) {
  vals <- sort(unique(as.character(x)))
  vals <- vals[nzchar(vals)]
  if (length(vals) == 0) return("Not specified")
  if (length(vals) <= max_n) return(paste(vals, collapse = "; "))
  paste0(paste(head(vals, max_n), collapse = "; "), "; 等 ", length(vals), " 项")
}

filter_for_analysis <- function(data, measure = NULL, cause = NULL, metric = NULL, age = NULL, sex = NULL, locations = NULL) {
  data <- gbd_filter_data(data, measure = measure, cause = cause, metric = metric, age = age, sex = sex, locations = locations)
  if (nrow(data) == 0) stop("当前筛选没有可分析记录，请放宽 measure/cause/metric/age/sex/location。")
  data
}

analyze_gbd <- function(data, measure = NULL, cause = NULL, metric = NULL, age = NULL, sex = NULL,
                        locations = NULL, start_year = NULL, end_year = NULL,
                        focus_location = NULL, forecast_year = 2030, top_n = 8) {
  raw <- standardize_gbd_data(data)
  selected <- filter_for_analysis(raw, measure, cause, metric, age, sex, locations)
  series <- collapse_gbd_series(selected)
  if (nrow(series) == 0) stop("筛选后没有可用时间序列。")
  start_year <- start_year %||% min(series$year, na.rm = TRUE)
  end_year <- end_year %||% max(series$year, na.rm = TRUE)
  start_year <- max(start_year, min(series$year, na.rm = TRUE))
  end_year <- min(end_year, max(series$year, na.rm = TRUE))
  if (start_year >= end_year) stop("开始年份必须早于结束年份。")

  by_location <- split(series, series$location)
  trend_rows <- lapply(by_location, trend_one_location, start_year = start_year, end_year = end_year)
  trend_table <- do.call(rbind, trend_rows[!vapply(trend_rows, is.null, logical(1))])
  trend_table <- trend_table[order(-trend_table$latest_value), , drop = FALSE]
  rownames(trend_table) <- NULL
  trend_table$rank_latest <- seq_len(nrow(trend_table))
  trend_table$eapc_ci <- paste0(fmt_num(trend_table$eapc, 2), " (", fmt_num(trend_table$eapc_low, 2), ", ", fmt_num(trend_table$eapc_high, 2), ")")
  trend_table$uncertainty_interval <- paste0(fmt_num(trend_table$lower, 1), "-", fmt_num(trend_table$upper, 1))

  if (is.null(focus_location) || !focus_location %in% unique(series$location)) {
    focus_location <- if ("Global" %in% trend_table$location) "Global" else trend_table$location[[1]]
  }
  top_locations <- unique(c(
    focus_location,
    if ("Global" %in% trend_table$location) "Global",
    head(trend_table$location, top_n)
  ))
  top_locations <- top_locations[top_locations %in% series$location]
  plot_data <- series[series$location %in% top_locations & series$year >= start_year & series$year <= end_year, , drop = FALSE]
  focus_series <- series[series$location == focus_location & series$year >= start_year & series$year <= end_year, , drop = FALSE]
  forecast <- make_forecast(focus_series, horizon = forecast_year)
  if (nrow(forecast) > 0) forecast$location <- focus_location

  meta <- list(
    measure = collapse_unique_terms(selected$measure),
    cause = collapse_unique_terms(selected$cause),
    metric = collapse_unique_terms(selected$metric),
    age = collapse_unique_terms(selected$age),
    sex = collapse_unique_terms(selected$sex),
    start_year = start_year,
    end_year = end_year,
    focus_location = focus_location,
    n_rows = nrow(selected),
    n_locations = length(unique(selected$location)),
    year_range = paste0(min(selected$year), "-", max(selected$year))
  )
  qa_table <- data.frame(
    item = c("原始记录", "筛选后记录", "地区数", "年份范围", "Measure", "Cause/Risk", "Metric", "Age", "Sex"),
    value = c(nrow(raw), nrow(selected), meta$n_locations, meta$year_range, meta$measure, meta$cause, meta$metric, meta$age, meta$sex),
    stringsAsFactors = FALSE
  )
  out <- list(
    raw = raw,
    selected = selected,
    series = series,
    plot_data = plot_data,
    focus_series = focus_series,
    trend_table = trend_table,
    rank_table = trend_table[order(trend_table$rank_latest), c("rank_latest", "location", "region", "latest_value", "uncertainty_interval", "percent_change", "eapc_ci", "eapc_p"), drop = FALSE],
    forecast = forecast,
    meta = meta,
    qa_table = qa_table
  )
  out$insights <- gbd_insights(out)
  out$paper <- gbd_paper_template(out)
  out
}

gbd_insights <- function(result) {
  t <- result$trend_table
  focus <- t[t$location == result$meta$focus_location, , drop = FALSE]
  if (nrow(focus) == 0) focus <- t[1, , drop = FALSE]
  fastest_up <- t[which.max(t$eapc), , drop = FALSE]
  fastest_down <- t[which.min(t$eapc), , drop = FALSE]
  highest <- t[1, , drop = FALSE]
  direction <- ifelse(focus$eapc >= 0, "上升", "下降")
  c(
    paste0(result$meta$focus_location, " 在 ", focus$start_year, "-", focus$end_year,
           " 年间的 EAPC 为 ", fmt_num(focus$eapc, 2), "%/年，趋势总体", direction, "。"),
    paste0("最新年份负担最高的地区为 ", highest$location, "，估计值为 ", fmt_num(highest$latest_value, 1),
           "（UI ", highest$uncertainty_interval, "）。"),
    paste0("上升最快：", fastest_up$location, "（EAPC ", fmt_num(fastest_up$eapc, 2), "%/年）；下降最快：",
           fastest_down$location, "（EAPC ", fmt_num(fastest_down$eapc, 2), "%/年）。")
  )
}

gbd_paper_template <- function(result) {
  meta <- result$meta
  focus <- result$trend_table[result$trend_table$location == meta$focus_location, , drop = FALSE]
  if (nrow(focus) == 0) focus <- result$trend_table[1, , drop = FALSE]
  title <- paste0(meta$cause, " 负担的全球、区域和国家趋势，",
                  meta$start_year, "-", meta$end_year, "年：基于全球疾病负担研究的分析")
  methods <- c(
    paste0("数据来源于全球疾病负担（GBD）Results Tool。我们提取了 ", meta$cause, " 的 ",
           meta$measure, "，使用 ", meta$metric, " 作为度量，", meta$age,
           " 作为年龄类别，", meta$sex, " 作为性别分层。"),
    paste0(meta$start_year, " 至 ", meta$end_year,
           " 年的趋势采用估计年度百分比变化（EAPC）进行总结，EAPC 通过对数线性回归（log(负担估计值) ~ 日历年）计算得出。"),
    "不确定性通过 GBD 导出文件中提供的 lower/upper 不确定区间进行描述。未进行个体层面因果推断。"
  )
  results <- c(
    paste0(meta$end_year, " 年，", focus$location, " 的估计负担为 ",
           fmt_num(focus$latest_value, 1), "（UI ", focus$uncertainty_interval, "）。"),
    paste0(focus$start_year, " 至 ", focus$end_year, " 年间，相对变化为 ",
           fmt_pct(focus$percent_change), "，EAPC 为 ", fmt_num(focus$eapc, 2), "%/年（95% CI ",
           fmt_num(focus$eapc_low, 2), " 至 ", fmt_num(focus$eapc_high, 2), "）。"),
    result$insights[[2]]
  )
  abstract <- c(
    "背景：疾病负担的时间变化模式对临床优先排序和卫生系统规划至关重要。",
    paste0("方法：我们分析了 GBD 估计数据，研究对象为 ", meta$cause, "，时间跨度为 ", meta$start_year, " 至 ", meta$end_year,
           " 年，重点关注以 ", meta$metric, " 衡量的 ", meta$measure, "。"),
    paste0("结果：", results[[1]], " ", results[[2]]),
    "结论：研究结果揭示了不同地区在负担轨迹上的临床相关差异，应解读为人群层面的模型估计值。"
  )
  limitations <- c(
    "GBD 估计值为建模汇总数据，而非个体参与者记录。",
    "跨地区比较受输入数据可得性、建模假设和不确定区间的影响。",
    "与卫生系统或社会经济指标的关联属于生态学分析，不应解读为个体层面的因果效应。"
  )
  list(title = title, abstract = abstract, methods = methods, results = results, limitations = limitations)
}

analysis_report_lines <- function(result) {
  c(
    "GBD 分析报告",
    paste0("生成时间：", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "",
    "分析范围",
    paste0("- 测量指标：", result$meta$measure),
    paste0("- 病因/风险：", result$meta$cause),
    paste0("- 度量：", result$meta$metric),
    paste0("- 年龄/性别：", result$meta$age, " / ", result$meta$sex),
    paste0("- 年份：", result$meta$start_year, "-", result$meta$end_year),
    "",
    "主要发现",
    paste0("- ", result$insights),
    "",
    "方法草稿",
    paste0("- ", result$paper$methods),
    "",
    "局限性",
    paste0("- ", result$paper$limitations)
  )
}

manuscript_text_lines <- function(result) {
  c(
    paste0("标题：", result$paper$title),
    "",
    "摘要",
    result$paper$abstract,
    "",
    "方法",
    result$paper$methods,
    "",
    "结果",
    result$paper$results,
    "",
    "局限性",
    result$paper$limitations
  )
}

gbd_cleaning_report_lines <- function(result) {
  c(
    "GBD 清洗报告",
    paste0("导入后原始行数：", nrow(result$raw)),
    paste0("筛选后行数：", nrow(result$selected)),
    paste0("纳入地区数：", result$meta$n_locations),
    paste0("数据源年份范围：", result$meta$year_range),
    paste0("分析窗口：", result$meta$start_year, "-", result$meta$end_year),
    paste0("测量指标：", result$meta$measure),
    paste0("病因/风险：", result$meta$cause),
    paste0("度量：", result$meta$metric),
    paste0("年龄/性别：", result$meta$age, " / ", result$meta$sex),
    "",
    "清洗规则",
    "- 将常见 IHME 列名标准化为 measure、location、sex、age、cause、metric、year、val、lower、upper。",
    "- 移除了 year 或 val 缺失/非数值的行。",
    "- 对分析范围内的同一 location-year 重复行取均值。",
    "- 使用 log(value) 对日历年回归计算 EAPC。"
  )
}

gbd_flow_table <- function(result) {
  data.frame(
    步骤 = c("原始导入", "字段标准化", "条件筛选", "趋势建模", "图文产出"),
    N = c(nrow(result$raw), nrow(result$raw), nrow(result$selected), nrow(result$trend_table), nrow(result$trend_table)),
    说明 = c(
      "读取上传 CSV/TSV 或演示数据。",
      "统一 year、val、lower、upper、location、measure、cause 等字段。",
      "按 Measure/Cause/Metric/Age/Sex/Location/Year 提取分析集。",
      "每个地区拟合 log(value) ~ year，得到 EAPC。",
      "生成趋势图、变化图、SDI 图、预测图、结论和写作模板。"
    ),
    排除或处理 = c(
      "保留原始文件，不修改列名。",
      "year 或 val 缺失/非数值的行被排除。",
      "不属于当前研究问题的行不进入分析。",
      "有效年份少于 3 的地区不报告 EAPC。",
      "预测为探索性外推，不作为正式预测模型。"
    ),
    stringsAsFactors = FALSE
  )
}

gbd_table1 <- function(result) {
  t <- result$trend_table
  if (nrow(t) == 0) return(data.frame())
  data.frame(
    指标 = c("分析记录数", "地区数", "起止年份", "焦点地区", "焦点地区最新估计", "焦点地区 EAPC", "最新负担最高地区", "上升最快地区", "下降最快地区"),
    内容 = c(
      format(nrow(result$selected), big.mark = ","),
      result$meta$n_locations,
      paste0(result$meta$start_year, "-", result$meta$end_year),
      result$meta$focus_location,
      {
        f <- t[t$location == result$meta$focus_location, , drop = FALSE]
        if (nrow(f) == 0) f <- t[1, , drop = FALSE]
        paste0(fmt_num(f$latest_value, 1), " (UI ", f$uncertainty_interval, ")")
      },
      {
        f <- t[t$location == result$meta$focus_location, , drop = FALSE]
        if (nrow(f) == 0) f <- t[1, , drop = FALSE]
        paste0(fmt_num(f$eapc, 2), "%/年 (", fmt_num(f$eapc_low, 2), ", ", fmt_num(f$eapc_high, 2), ")")
      },
      t$location[[which.max(t$latest_value)]],
      t$location[[which.max(t$eapc)]],
      t$location[[which.min(t$eapc)]]
    ),
    stringsAsFactors = FALSE
  )
}

gbd_model_table <- function(result) {
  x <- result$trend_table[, c("location", "region", "start_year", "end_year", "start_value", "latest_value", "percent_change", "eapc", "eapc_low", "eapc_high", "eapc_p"), drop = FALSE]
  names(x) <- c("地区", "区域", "开始年份", "结束年份", "起始估计", "最新估计", "相对变化", "EAPC", "EAPC_low", "EAPC_high", "P值")
  x$起始估计 <- fmt_num(x$起始估计, 1)
  x$最新估计 <- fmt_num(x$最新估计, 1)
  x$相对变化 <- fmt_pct(x$相对变化)
  x$EAPC <- fmt_num(x$EAPC, 2)
  x$EAPC_low <- fmt_num(x$EAPC_low, 2)
  x$EAPC_high <- fmt_num(x$EAPC_high, 2)
  x$P值 <- fmt_p(x$P值)
  x
}

gbd_sensitivity_table <- function(result) {
  t <- result$trend_table
  focus <- t[t$location == result$meta$focus_location, , drop = FALSE]
  if (nrow(focus) == 0) focus <- t[1, , drop = FALSE]
  data.frame(
    敏感性方案 = c("主分析", "去除起始 5 年", "仅报告最新 10 年", "排除 Global 参照"),
    口径 = c(
      paste0(result$meta$start_year, "-", result$meta$end_year),
      paste0(min(result$meta$start_year + 5, result$meta$end_year - 2), "-", result$meta$end_year),
      paste0(max(result$meta$start_year, result$meta$end_year - 9), "-", result$meta$end_year),
      "国家/地区内部排序"
    ),
    解读 = c(
      paste0("焦点地区 EAPC ", fmt_num(focus$eapc, 2), "%/年。"),
      "用于检查早期估计是否驱动长期趋势；正式报告需重新拟合。",
      "用于观察近期政策或流行病学变化；不替代主分析。",
      "用于避免 Global 参照影响国家排序解读。"
    ),
    stringsAsFactors = FALSE
  )
}

gbd_interpretation_lines <- function(result) {
  c(
    "结果解读",
    result$insights,
    "",
    "写作提醒",
    paste0("1. 当前结果来自 ", result$meta$measure, " / ", result$meta$metric, " / ", result$meta$age, " / ", result$meta$sex, "。"),
    "2. EAPC 反映研究窗口内平均年度变化，不能理解为每一年都等比例变化。",
    "3. lower/upper 是不确定区间，写结果和图注时要保留。",
    "4. SDI 图是生态层面描述，不能推出个体层面因果关系。",
    "5. 预测图只是基于既往趋势的探索性外推。"
  )
}

draw_gbd_flow_plot <- function(result) {
  d <- gbd_flow_table(result)
  th <- clinical_theme()
  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(fill = "#F7FAF8", col = NA))
  grid::grid.roundrect(x = 0.5, y = 0.5, width = 0.92, height = 0.88, r = grid::unit(12, "pt"),
                       gp = grid::gpar(fill = "#FFFFFF", col = "#DDEAE6", lwd = 1.2))
  grid::grid.text("图 1. GBD 数据处理流程", x = 0.07, y = 0.90, just = "left",
                  gp = grid::gpar(col = th$ink, fontsize = 18, fontface = "bold", fontfamily = plot_family()))
  grid::grid.text("从官方导出到清洗后分析数据集，再到论文级产出", x = 0.07, y = 0.86, just = "left",
                  gp = grid::gpar(col = th$muted, fontsize = 10.5, fontfamily = plot_family()))
  y_positions <- seq(0.74, 0.26, length.out = nrow(d))
  fills <- c("#F4FAF8", "#F4F6FF", "#FFF7EF", "#EFF8F4", "#F8F5FE")
  for (i in seq_len(nrow(d))) {
    y <- y_positions[[i]]
    grid::grid.roundrect(x = 0.5, y = y, width = 0.78, height = 0.115, r = grid::unit(10, "pt"),
                         gp = grid::gpar(fill = fills[(i - 1) %% length(fills) + 1], col = "#D6E6E1", lwd = 1.2))
    grid::grid.circle(x = 0.17, y = y, r = 0.027, gp = grid::gpar(fill = "#FFFFFF", col = th$teal, lwd = 1.2))
    grid::grid.text(sprintf("%02d", i), x = 0.17, y = y, gp = grid::gpar(col = th$red, fontsize = 10.5, fontface = "bold", fontfamily = plot_family()))
    grid::grid.text(d$步骤[[i]], x = 0.22, y = y + 0.025, just = "left", gp = grid::gpar(col = th$ink, fontsize = 12, fontface = "bold", fontfamily = plot_family()))
    grid::grid.text(paste0("N = ", format(d$N[[i]], big.mark = ",")), x = 0.22, y = y - 0.028, just = "left", gp = grid::gpar(col = th$red, fontsize = 12.5, fontface = "bold", fontfamily = plot_family()))
    grid::grid.text(paste(strwrap(d$说明[[i]], 44), collapse = "\n"), x = 0.42, y = y + 0.010, just = "left", gp = grid::gpar(col = "#405960", fontsize = 9.3, lineheight = 1.1, fontfamily = plot_family()))
    grid::grid.text(paste(strwrap(d$排除或处理[[i]], 42), collapse = "\n"), x = 0.65, y = y - 0.026, just = "left", gp = grid::gpar(col = "#8A3F18", fontsize = 8.7, lineheight = 1.05, fontfamily = plot_family()))
    if (i < nrow(d)) {
      grid::grid.segments(x0 = 0.5, y0 = y - 0.063, x1 = 0.5, y1 = y_positions[[i + 1]] + 0.063,
                          arrow = grid::arrow(length = grid::unit(6, "pt"), type = "closed"),
                          gp = grid::gpar(col = "#8BA59F", lwd = 1.4))
    }
  }
}

draw_trend_plot <- function(result, preview = FALSE) {
  if (!require_plot_packages()) {
    draw_plot_message("缺少绘图包", c("请安装 ggplot2 与 scales 后重新运行。"))
    return(invisible(NULL))
  }
  d <- result$plot_data
  focus <- result$meta$focus_location
  th <- clinical_theme()
  latest <- d[d$year == max(d$year, na.rm = TRUE), c("location", "year", "val"), drop = FALSE]
  base_size <- if (preview) 11 else 14
  title <- if (preview) "负担轨迹" else "疾病负担时间轨迹"
  subtitle <- paste0(result$meta$cause, " | ", result$meta$measure, ", ", result$meta$metric, " | ", result$meta$age, ", ", result$meta$sex)
  focus_d <- d[d$location == focus, , drop = FALSE]
  label_layer <- if (requireNamespace("ggrepel", quietly = TRUE)) {
    ggrepel::geom_text_repel(
      data = latest,
      ggplot2::aes(label = location),
      hjust = 0,
      nudge_x = 0.6,
      direction = "y",
      min.segment.length = 0,
      segment.colour = "#A8B9B5",
      segment.size = 0.28,
      size = if (preview) 2.7 else 3.25,
      family = plot_family(),
      show.legend = FALSE,
      max.overlaps = 30
    )
  } else {
    ggplot2::geom_text(
      data = latest,
      ggplot2::aes(label = location),
      hjust = -0.08,
      size = if (preview) 2.7 else 3.2,
      family = plot_family(),
      show.legend = FALSE
    )
  }
  p <- ggplot2::ggplot(d, ggplot2::aes(year, val, colour = location, group = location)) +
    ggplot2::geom_ribbon(data = focus_d, ggplot2::aes(x = year, ymin = lower, ymax = upper), inherit.aes = FALSE, fill = th$red, alpha = 0.10) +
    ggplot2::geom_line(linewidth = 0.82, alpha = 0.55) +
    ggplot2::geom_line(data = focus_d, linewidth = 1.85, colour = th$red, lineend = "round") +
    ggplot2::geom_point(data = latest, shape = 21, size = 3.0, fill = "#FFFFFF", stroke = 0.85, alpha = 0.98) +
    label_layer +
    ggplot2::scale_colour_manual(values = clinical_cols(length(unique(d$location)))) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.13))) +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ","), expand = ggplot2::expansion(mult = c(0.04, 0.10))) +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = paste(result$meta$measure, result$meta$metric), caption = paste0("红色带表示 ", focus, " 的不确定区间；标签标记最新年估计值。")) +
    publication_theme(base_size = base_size) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_line(colour = "#EDF3F1", linewidth = 0.35),
      plot.title = ggplot2::element_text(size = base_size + 6, face = "bold")
    ) +
    ggplot2::coord_cartesian(clip = "off")
  print(p)
}

draw_rank_plot <- function(result, preview = FALSE) {
  if (!require_plot_packages()) {
    draw_plot_message("缺少绘图包", c("请安装 ggplot2 与 scales 后重新运行。"))
    return(invisible(NULL))
  }
  d <- result$trend_table
  d <- d[order(-d$latest_value), , drop = FALSE]
  d <- head(d, 12)
  d$location_label <- factor(short_label(d$location, 18), levels = rev(short_label(d$location, 18)))
  d$direction <- ifelse(d$absolute_change >= 0, "Increase", "Decrease")
  th <- clinical_theme()
  base_size <- if (preview) 11 else 14
  p <- ggplot2::ggplot(d, ggplot2::aes(y = location_label)) +
    ggplot2::geom_vline(xintercept = stats::median(d$start_value, na.rm = TRUE), colour = "#DCE8E4", linewidth = 0.8, linetype = "22") +
    ggplot2::geom_segment(ggplot2::aes(x = start_value, xend = latest_value, yend = location_label, colour = direction), linewidth = 1.25, alpha = 0.88, lineend = "round") +
    ggplot2::geom_point(ggplot2::aes(x = start_value), size = 3.2, shape = 21, fill = "#FFFFFF", colour = th$muted, stroke = 0.8) +
    ggplot2::geom_point(ggplot2::aes(x = latest_value, fill = direction), size = 4.2, shape = 21, colour = "#FFFFFF", stroke = 0.9) +
    ggplot2::geom_text(ggplot2::aes(x = latest_value, label = paste0("EAPC ", fmt_num(eapc, 2), "%")), hjust = -0.08, size = if (preview) 2.6 else 3.1, family = plot_family(), colour = th$ink) +
    ggplot2::scale_colour_manual(values = c(Increase = th$red, Decrease = th$teal)) +
    ggplot2::scale_fill_manual(values = c(Increase = th$red, Decrease = th$teal)) +
    ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = ","), expand = ggplot2::expansion(mult = c(0.04, 0.22))) +
    ggplot2::labs(
      title = if (preview) "起止变化" else "从基线到最新年的变化",
      subtitle = paste0(result$meta$start_year, " 至 ", result$meta$end_year, " | 按最新估计值排序的地区"),
      x = paste(result$meta$measure, result$meta$metric),
      y = NULL,
      caption = "空心圆标记基线；实心圆标记最新估计值。"
    ) +
    publication_theme(base_size = base_size, grid_y = FALSE) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(face = "bold"), legend.position = "top", panel.grid.major.x = ggplot2::element_line(colour = "#E8F0ED", linewidth = 0.42)) +
    ggplot2::coord_cartesian(clip = "off")
  print(p)
}

draw_equity_plot <- function(result, preview = FALSE) {
  if (!require_plot_packages()) {
    draw_plot_message("缺少绘图包", c("请安装 ggplot2 与 scales 后重新运行。"))
    return(invisible(NULL))
  }
  d <- result$trend_table
  d <- d[is.finite(d$sdi) & is.finite(d$latest_value), , drop = FALSE]
  if (nrow(d) < 3) {
    draw_plot_message("无法生成 SDI 图", c("当前数据缺少 sdi 或地区数量不足。上传含 sdi/region 的文件可生成这张图。"))
    return(invisible(NULL))
  }
  d$pop_size <- ifelse(is.finite(d$population), d$population, stats::median(d$population, na.rm = TRUE))
  label_idx <- unique(c(order(-d$latest_value)[seq_len(min(4, nrow(d)))], which(d$location == result$meta$focus_location)))
  label_df <- d[label_idx, , drop = FALSE]
  th <- clinical_theme()
  base_size <- if (preview) 11 else 14
  p <- ggplot2::ggplot(d, ggplot2::aes(sdi, latest_value)) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, colour = th$navy, fill = "#D7E6EF", linewidth = 1.05, alpha = 0.48) +
    ggplot2::geom_point(ggplot2::aes(fill = region, size = pop_size), shape = 21, colour = "#FFFFFF", stroke = 0.9, alpha = 0.94) +
    {
      if (requireNamespace("ggrepel", quietly = TRUE)) {
        ggrepel::geom_text_repel(data = label_df, ggplot2::aes(label = location), size = if (preview) 2.6 else 3.1, family = plot_family(), colour = th$ink, show.legend = FALSE, segment.colour = "#A8B9B5", segment.size = 0.25, max.overlaps = 20)
      } else {
        ggplot2::geom_text(data = label_df, ggplot2::aes(label = location), nudge_y = 0.035 * max(d$latest_value, na.rm = TRUE), size = if (preview) 2.6 else 3.1, family = plot_family(), colour = th$ink, show.legend = FALSE)
      }
    } +
    ggplot2::scale_fill_manual(values = clinical_cols(length(unique(d$region)))) +
    ggplot2::scale_size_continuous(range = c(3.5, 9.5), guide = "none") +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ","), expand = ggplot2::expansion(mult = c(0.04, 0.12))) +
    ggplot2::scale_x_continuous(limits = c(max(0, min(d$sdi, na.rm = TRUE) - 0.04), min(1, max(d$sdi, na.rm = TRUE) + 0.04))) +
    ggplot2::labs(
      title = if (preview) "SDI 梯度" else "社会人口发展水平的负担梯度",
      subtitle = paste0(result$meta$end_year, " 年估计值；点大小反映数据中的人口规模"),
      x = "社会人口指数 (SDI)",
      y = paste(result$meta$measure, result$meta$metric),
      caption = "此图为生态学描述，不应解读为个体层面因果关系。"
    ) +
    publication_theme(base_size = base_size) +
    ggplot2::theme(legend.position = "top", legend.box = "vertical")
  print(p)
}

draw_forecast_plot <- function(result, preview = FALSE) {
  if (!require_plot_packages()) {
    draw_plot_message("缺少绘图包", c("请安装 ggplot2 与 scales 后重新运行。"))
    return(invisible(NULL))
  }
  obs <- result$focus_series
  fc <- result$forecast
  if (nrow(fc) == 0) {
    draw_plot_message("预测未生成", c("当前焦点地区有效年份不足，无法生成 log-linear 外推。"))
    return(invisible(NULL))
  }
  th <- clinical_theme()
  base_size <- if (preview) 11 else 14
  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = fc, ggplot2::aes(year, ymin = lower, ymax = upper), fill = "#DCECE6", alpha = 0.85) +
    ggplot2::geom_line(data = obs, ggplot2::aes(year, val), colour = th$teal, linewidth = 1.35) +
    ggplot2::geom_line(data = fc[fc$year >= max(obs$year), , drop = FALSE], ggplot2::aes(year, val), colour = th$red, linewidth = 1.15, linetype = "22") +
    ggplot2::geom_point(data = obs[obs$year == max(obs$year), , drop = FALSE], ggplot2::aes(year, val), shape = 21, fill = th$red, colour = "#FFFFFF", size = 4, stroke = 0.9) +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ","), expand = ggplot2::expansion(mult = c(0.05, 0.10))) +
    ggplot2::labs(
      title = if (preview) "预测" else paste0(result$meta$focus_location, " 近期预测"),
      subtitle = "简单对数线性外推用于情景筛查；不能替代正式预测模型。",
      x = NULL,
      y = paste(result$meta$measure, result$meta$metric),
      caption = "虚线和阴影带表示外推均值及近似 95% 区间。"
    ) +
    publication_theme(base_size = base_size)
  print(p)
}

draw_heatmap_plot <- function(result, preview = FALSE) {
  if (!require_plot_packages()) {
    draw_plot_message("缺少绘图包", c("请安装 ggplot2 与 scales 后重新运行。"))
    return(invisible(NULL))
  }
  d <- result$plot_data
  d <- d[order(d$location, d$year), , drop = FALSE]
  if (nrow(d) == 0) {
    draw_plot_message("热力图未生成", c("当前筛选没有可用时间序列。"))
    return(invisible(NULL))
  }
  d$base <- ave(d$val, d$location, FUN = function(x) x[[1]])
  d$index <- 100 * d$val / d$base
  latest_order <- result$trend_table[order(result$trend_table$latest_value), "location"]
  d <- d[d$location %in% latest_order, , drop = FALSE]
  d$location_label <- factor(short_label(d$location, 20), levels = short_label(latest_order[latest_order %in% d$location], 20))
  th <- clinical_theme()
  base_size <- if (preview) 11 else 14
  p <- ggplot2::ggplot(d, ggplot2::aes(year, location_label, fill = index)) +
    ggplot2::geom_tile(colour = "#FFFFFF", linewidth = 0.24, height = 0.86) +
    ggplot2::geom_vline(xintercept = result$meta$start_year, colour = "#FFFFFF", linewidth = 0.6) +
    ggplot2::scale_fill_gradient2(
      low = "#246B8E",
      mid = "#F7FBFA",
      high = "#B34D3E",
      midpoint = 100,
      labels = function(x) paste0(round(x), "%"),
      name = "Index"
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0), breaks = pretty(d$year, n = if (preview) 6 else 8)) +
    ggplot2::labs(
      title = if (preview) "趋势热力图" else "索引化趋势热力图",
      subtitle = "每行以首个观测年为基线 (=100)，展示变化速度与方向。",
      x = NULL,
      y = NULL,
      caption = "暖色表示相对基线上升；冷色表示下降。"
    ) +
    publication_theme(base_size = base_size, grid_y = FALSE) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(face = "bold"),
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold", colour = th$ink)
    )
  print(p)
}

draw_eapc_plot <- function(result, preview = FALSE) {
  if (!require_plot_packages()) {
    draw_plot_message("缺少绘图包", c("请安装 ggplot2 与 scales 后重新运行。"))
    return(invisible(NULL))
  }
  d <- result$trend_table
  d <- d[is.finite(d$eapc) & is.finite(d$eapc_low) & is.finite(d$eapc_high), , drop = FALSE]
  if (nrow(d) == 0) {
    draw_plot_message("EAPC 图未生成", c("当前数据没有足够年份拟合 EAPC。"))
    return(invisible(NULL))
  }
  keep <- unique(c(
    result$meta$focus_location,
    head(d$location[order(d$eapc)], 6),
    head(d$location[order(-d$eapc)], 6),
    head(d$location[order(-d$latest_value)], 6)
  ))
  d <- d[d$location %in% keep, , drop = FALSE]
  d <- d[order(d$eapc), , drop = FALSE]
  d$location_label <- factor(short_label(d$location, 22), levels = short_label(d$location, 22))
  d$trend <- ifelse(d$eapc_low > 0, "Increasing", ifelse(d$eapc_high < 0, "Decreasing", "Stable"))
  th <- clinical_theme()
  base_size <- if (preview) 11 else 14
  p <- ggplot2::ggplot(d, ggplot2::aes(eapc, location_label)) +
    ggplot2::geom_vline(xintercept = 0, colour = th$navy, linewidth = 0.75, linetype = "22") +
    ggplot2::geom_segment(ggplot2::aes(x = eapc_low, xend = eapc_high, yend = location_label, colour = trend), linewidth = 1.05, lineend = "round") +
    ggplot2::geom_point(ggplot2::aes(fill = trend), shape = 21, colour = "#FFFFFF", size = 4.1, stroke = 0.9) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(fmt_num(eapc, 2), "%")), hjust = ifelse(d$eapc >= 0, -0.16, 1.16), size = if (preview) 2.65 else 3.1, family = plot_family(), colour = th$ink) +
    ggplot2::scale_colour_manual(values = c(Increasing = th$red, Decreasing = th$teal, Stable = th$muted)) +
    ggplot2::scale_fill_manual(values = c(Increasing = th$red, Decreasing = th$teal, Stable = "#AEBEC0")) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"), expand = ggplot2::expansion(mult = c(0.14, 0.16))) +
    ggplot2::labs(
      title = if (preview) "EAPC 排名" else "估计年度百分比变化",
      subtitle = paste0(result$meta$start_year, "-", result$meta$end_year, " | 水平条表示近似 95% CI"),
      x = "EAPC（%/年）",
      y = NULL,
      caption = "地区来自上升最快、下降最快、负担最高以及焦点地区。"
    ) +
    publication_theme(base_size = base_size, grid_y = FALSE) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(face = "bold"), legend.position = "top") +
    ggplot2::coord_cartesian(clip = "off")
  print(p)
}

draw_uncertainty_plot <- function(result, preview = FALSE) {
  if (!require_plot_packages()) {
    draw_plot_message("缺少绘图包", c("请安装 ggplot2 与 scales 后重新运行。"))
    return(invisible(NULL))
  }
  d <- result$trend_table
  d <- d[is.finite(d$latest_value) & is.finite(d$lower) & is.finite(d$upper), , drop = FALSE]
  d <- d[order(-d$latest_value), , drop = FALSE]
  d <- head(d, 16)
  if (nrow(d) == 0) {
    draw_plot_message("不确定区间图未生成", c("当前数据缺少 lower/upper 字段。"))
    return(invisible(NULL))
  }
  d$location_label <- factor(short_label(d$location, 22), levels = rev(short_label(d$location, 22)))
  d$rank_group <- ifelse(d$location == result$meta$focus_location, "Focus", "Other")
  th <- clinical_theme()
  base_size <- if (preview) 11 else 14
  p <- ggplot2::ggplot(d, ggplot2::aes(y = location_label)) +
    ggplot2::geom_segment(ggplot2::aes(x = lower, xend = upper, yend = location_label), colour = "#9BAEAA", linewidth = 1.05, lineend = "round") +
    ggplot2::geom_point(ggplot2::aes(x = latest_value, fill = rank_group), shape = 21, colour = "#FFFFFF", stroke = 0.9, size = 4.4) +
    ggplot2::geom_text(ggplot2::aes(x = upper, label = fmt_num(latest_value, 1)), hjust = -0.12, size = if (preview) 2.6 else 3.05, family = plot_family(), colour = th$ink) +
    ggplot2::scale_fill_manual(values = c(Focus = th$red, Other = th$teal)) +
    ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = ","), expand = ggplot2::expansion(mult = c(0.04, 0.18))) +
    ggplot2::labs(
      title = if (preview) "最新估计及不确定区间" else paste0("最新", result$meta$measure, " 估计值及不确定区间"),
      subtitle = paste0(result$meta$end_year, " 年负担最高的地区；点为估计值，水平线为不确定区间。"),
      x = paste(result$meta$measure, result$meta$metric),
      y = NULL,
      caption = "较宽的区间提示更大的不确定性，解读时应谨慎。"
    ) +
    publication_theme(base_size = base_size, grid_y = FALSE) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(face = "bold"), legend.position = "top") +
    ggplot2::coord_cartesian(clip = "off")
  print(p)
}

draw_quadrant_plot <- function(result, preview = FALSE) {
  if (!require_plot_packages()) {
    draw_plot_message("缺少绘图包", c("请安装 ggplot2 与 scales 后重新运行。"))
    return(invisible(NULL))
  }
  d <- result$trend_table
  d <- d[is.finite(d$latest_value) & is.finite(d$eapc), , drop = FALSE]
  if (nrow(d) < 3) {
    draw_plot_message("象限图未生成", c("当前地区数量不足。"))
    return(invisible(NULL))
  }
  x_mid <- stats::median(d$latest_value, na.rm = TRUE)
  y_mid <- 0
  d$plot_size <- ifelse(is.finite(d$population), d$population, 1)
  d$priority <- ifelse(d$latest_value >= x_mid & d$eapc > 0, "High burden + rising",
                       ifelse(d$latest_value >= x_mid, "High burden", ifelse(d$eapc > 0, "Rising", "Lower priority")))
  label_idx <- unique(c(
    which(d$location == result$meta$focus_location),
    order(-d$latest_value)[seq_len(min(4, nrow(d)))],
    order(-d$eapc)[seq_len(min(3, nrow(d)))]
  ))
  label_df <- d[label_idx, , drop = FALSE]
  th <- clinical_theme()
  base_size <- if (preview) 11 else 14
  p <- ggplot2::ggplot(d, ggplot2::aes(latest_value, eapc)) +
    ggplot2::annotate("rect", xmin = x_mid, xmax = Inf, ymin = 0, ymax = Inf, fill = "#FFF1E7", alpha = 0.75) +
    ggplot2::geom_hline(yintercept = y_mid, colour = "#B7C8C4", linewidth = 0.65, linetype = "22") +
    ggplot2::geom_vline(xintercept = x_mid, colour = "#B7C8C4", linewidth = 0.65, linetype = "22") +
    ggplot2::geom_point(ggplot2::aes(fill = priority, size = plot_size), shape = 21, colour = "#FFFFFF", stroke = 0.85, alpha = 0.92) +
    {
      if (requireNamespace("ggrepel", quietly = TRUE)) {
        ggrepel::geom_text_repel(data = label_df, ggplot2::aes(label = location), family = plot_family(), size = if (preview) 2.55 else 3.05, colour = th$ink, segment.colour = "#A8B9B5", segment.size = 0.25, show.legend = FALSE)
      } else {
        ggplot2::geom_text(data = label_df, ggplot2::aes(label = location), family = plot_family(), size = if (preview) 2.55 else 3.05, colour = th$ink, show.legend = FALSE)
      }
    } +
    ggplot2::scale_fill_manual(values = c("High burden + rising" = th$red, "High burden" = th$amber, "Rising" = th$blue, "Lower priority" = "#AEBEC0")) +
    ggplot2::scale_size_continuous(range = c(3.2, 8.5), guide = "none") +
    ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = ","), expand = ggplot2::expansion(mult = c(0.06, 0.10))) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), expand = ggplot2::expansion(mult = c(0.12, 0.16))) +
    ggplot2::labs(
      title = if (preview) "优先象限" else "负担水平与趋势优先象限图",
      subtitle = "右上角地区兼具当前高负担和上升趋势。",
      x = paste("最新", result$meta$measure, result$meta$metric),
      y = "EAPC（%/年）",
      caption = "气泡大小反映数据中的人口规模。"
    ) +
    publication_theme(base_size = base_size, grid_y = TRUE) +
    ggplot2::theme(legend.position = "top")
  print(p)
}
