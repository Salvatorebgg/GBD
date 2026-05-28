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
  if (start_year > end_year) stop("开始年份不能晚于结束年份。")

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
    data_type = collapse_unique_terms(selected$data_type),
    start_year = start_year,
    end_year = end_year,
    focus_location = focus_location,
    n_rows = nrow(selected),
    n_locations = length(unique(selected$location)),
    n_causes = length(unique(selected$cause)),
    n_measures = length(unique(selected$measure)),
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
  out$diagnostic_table <- gbd_diagnostic_table(out)
  out$priority_table <- gbd_priority_table(out)
  out$contribution_table <- gbd_contribution_table(out)
  out$age_pattern_table <- gbd_age_pattern_table(out)
  out$insights <- gbd_insights(out)
  out$paper <- gbd_paper_template(out)
  out
}

gbd_insights <- function(result) {
  t <- result$trend_table
  focus <- t[t$location == result$meta$focus_location, , drop = FALSE]
  if (nrow(focus) == 0) focus <- t[1, , drop = FALSE]
  trend_signal <- ifelse(is.finite(t$eapc), t$eapc, ifelse(is.finite(t$percent_change), 100 * t$percent_change, NA_real_))
  if (all(!is.finite(trend_signal))) trend_signal <- rep(0, nrow(t))
  fastest_up <- t[which.max(trend_signal), , drop = FALSE]
  fastest_down <- t[which.min(trend_signal), , drop = FALSE]
  highest <- t[1, , drop = FALSE]
  focus_signal <- ifelse(is.finite(focus$eapc), focus$eapc, ifelse(is.finite(focus$percent_change), 100 * focus$percent_change, 0))
  direction <- ifelse(focus_signal >= 0, "上升", "下降")
  trend_text <- if (is.finite(focus$eapc)) {
    paste0("EAPC 为 ", fmt_num(focus$eapc, 2), "%/年")
  } else {
    paste0("起止相对变化为 ", fmt_pct(focus$percent_change))
  }
  c(
    paste0(result$meta$focus_location, " 在 ", focus$start_year, "-", focus$end_year,
           " 年间的 ", trend_text, "，趋势总体", direction, "。"),
    paste0("最新年份负担最高的地区为 ", highest$location, "，估计值为 ", fmt_num(highest$latest_value, 1),
           "（UI ", highest$uncertainty_interval, "）。"),
    paste0("变化信号最高：", fastest_up$location, "；变化信号最低：", fastest_down$location, "。")
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
  trend_signal <- ifelse(is.finite(t$eapc), t$eapc, ifelse(is.finite(t$percent_change), 100 * t$percent_change, 0))
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
        if (is.finite(f$eapc)) paste0(fmt_num(f$eapc, 2), "%/年 (", fmt_num(f$eapc_low, 2), ", ", fmt_num(f$eapc_high, 2), ")") else "年份不足，未拟合 EAPC"
      },
      t$location[[which.max(t$latest_value)]],
      t$location[[which.max(trend_signal)]],
      t$location[[which.min(trend_signal)]]
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

gbd_diagnostic_table <- function(result) {
  s <- result$selected
  finite_sdi <- sum(is.finite(s$sdi))
  finite_pop <- sum(is.finite(s$population))
  years <- sort(unique(s$year))
  data.frame(
    item = c("数据类型", "分析记录", "文件来源", "年份结构", "核心维度", "SDI 可用性", "人口权重", "趋势模型", "自动策略"),
    value = c(
      result$meta$data_type,
      format(nrow(s), big.mark = ","),
      paste0(length(unique(s$source_file)), " 个文件"),
      paste0(min(years), "-", max(years), "；", length(years), " 个年份"),
      paste0(result$meta$n_causes, " 个 cause/risk；", result$meta$n_locations, " 个地区"),
      if (finite_sdi > 0) paste0("可用：", format(finite_sdi, big.mark = ","), " 行") else "缺失",
      if (finite_pop > 0) paste0("可用：", format(finite_pop, big.mark = ","), " 行") else "缺失",
      if (length(years) >= 3) "可计算 EAPC" else "年份不足，仅做描述性比较",
      if (finite_sdi > 0) "使用真实 SDI 梯度" else "使用地区负担分位/排序作为替代梯度，图注中标明"
    ),
    note = c(
      "由字段和文件类型自动识别。",
      "进入当前筛选条件后的记录数。",
      "批量上传时保留 source_file 便于回溯。",
      "Forecast 文件会保留未来年份，历史文件保留原始年份。",
      "决定是否适合做构成、地区差异和趋势比较。",
      "多数 GHDx 公共 CSV 不自带 SDI，不能因此让图空白。",
      "缺失时气泡图使用统一点大小。",
      "EAPC 至少需要 3 个有效年份。",
      "保证所有上传类型都有可解释输出。"
    ),
    stringsAsFactors = FALSE
  )
}

gbd_priority_table <- function(result) {
  d <- result$trend_table
  if (nrow(d) == 0) return(data.frame())
  latest_rank <- rank(-d$latest_value, ties.method = "average")
  burden_pct <- 1 - (latest_rank - 1) / max(1, nrow(d) - 1)
  trend_signal <- ifelse(is.finite(d$eapc), d$eapc, ifelse(is.finite(d$percent_change), 100 * d$percent_change, 0))
  trend_pct <- if (length(unique(trend_signal[is.finite(trend_signal)])) > 1) {
    rank(trend_signal, ties.method = "average") / nrow(d)
  } else {
    rep(0.5, nrow(d))
  }
  score <- 100 * (0.62 * burden_pct + 0.38 * trend_pct)
  action <- ifelse(burden_pct >= 0.66 & trend_signal > 0, "优先关注：高负担且上升",
                   ifelse(burden_pct >= 0.66, "重点描述：高负担",
                          ifelse(trend_signal > 0, "预警观察：低负担但上升", "常规报告")))
  out <- data.frame(
    location = d$location,
    latest_value = d$latest_value,
    uncertainty_interval = d$uncertainty_interval,
    percent_change = d$percent_change,
    eapc = d$eapc,
    priority_score = score,
    action = action,
    stringsAsFactors = FALSE
  )
  out <- out[order(-out$priority_score), , drop = FALSE]
  out$latest_value <- fmt_num(out$latest_value, 2)
  out$percent_change <- fmt_pct(out$percent_change)
  out$eapc <- fmt_num(out$eapc, 2)
  out$priority_score <- fmt_num(out$priority_score, 1)
  rownames(out) <- NULL
  out
}

gbd_contribution_table <- function(result) {
  s <- result$selected
  latest <- max(s$year, na.rm = TRUE)
  focus <- result$meta$focus_location
  d_all <- s[s$year == latest, , drop = FALSE]
  d_focus <- d_all
  if (focus %in% d_all$location) d_focus <- d_all[d_all$location == focus, , drop = FALSE]
  if (length(unique(d_focus$cause)) > 1) {
    d <- d_focus
    dimension <- "cause"
    location_label <- focus
  } else if (length(unique(d_focus$age)) > 1) {
    d <- d_focus
    dimension <- "age"
    location_label <- focus
  } else if (length(unique(d_focus$measure)) > 1) {
    d <- d_focus
    dimension <- "measure"
    location_label <- focus
  } else if (length(unique(d_focus$sex)) > 1) {
    d <- d_focus
    dimension <- "sex"
    location_label <- focus
  } else if (length(unique(d_all$location)) > 1) {
    d <- d_all
    dimension <- "location"
    location_label <- "Selected locations"
  } else {
    d <- d_focus
    dimension <- "source_file"
    location_label <- focus
  }
  grouped <- aggregate(d[c("val", "lower", "upper")], by = list(component = d[[dimension]]), FUN = function(x) sum(x[is.finite(x)], na.rm = TRUE))
  grouped <- grouped[order(-grouped$val), , drop = FALSE]
  total <- sum(grouped$val, na.rm = TRUE)
  grouped$share <- if (is.finite(total) && total > 0) grouped$val / total else NA_real_
  grouped$dimension <- dimension
  grouped$year <- latest
  grouped$location <- location_label
  grouped <- grouped[, c("dimension", "component", "location", "year", "val", "share", "lower", "upper"), drop = FALSE]
  rownames(grouped) <- NULL
  grouped
}

gbd_age_pattern_table <- function(result) {
  s <- result$selected
  latest <- max(s$year, na.rm = TRUE)
  focus <- result$meta$focus_location
  d <- s[s$year == latest, , drop = FALSE]
  if (focus %in% d$location) d <- d[d$location == focus, , drop = FALSE]
  grouped <- aggregate(d[c("val", "lower", "upper")], by = list(age = d$age), FUN = function(x) sum(x[is.finite(x)], na.rm = TRUE))
  grouped <- grouped[order(-grouped$val), , drop = FALSE]
  grouped$year <- latest
  grouped$location <- if (focus %in% s$location) focus else "All selected locations"
  grouped <- grouped[, c("age", "location", "year", "val", "lower", "upper"), drop = FALSE]
  rownames(grouped) <- NULL
  grouped
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
  d <- d[is.finite(d$latest_value), , drop = FALSE]
  if (nrow(d) == 0) {
    draw_plot_message("梯度图未生成", c("当前筛选没有可用于比较的估计值。"))
    return(invisible(NULL))
  }
  has_sdi <- sum(is.finite(d$sdi)) >= 2
  if (has_sdi) {
    d$gradient_x <- d$sdi
    x_lab <- "Socio-demographic Index (SDI)"
    title <- if (preview) "SDI 梯度" else "SDI 梯度与当前负担"
    subtitle <- paste0(result$meta$end_year, " 年估计值；点大小反映人口，缺失人口时使用统一大小。")
    caption <- "生态学描述视角，不能解释为个体层面因果关系。"
  } else {
    d <- d[order(d$latest_value), , drop = FALSE]
    d$gradient_x <- seq_len(nrow(d))
    x_lab <- "地区负担排序（SDI 缺失时的替代梯度）"
    title <- if (preview) "地区梯度" else "无 SDI 时的地区负担梯度"
    subtitle <- paste0(result$meta$end_year, " 年估计值；源文件无 SDI，横轴改用负担从低到高排序。")
    caption <- "源文件未提供 SDI，因此本图不声称社会发展梯度，只用于展示地区差异。"
  }
  pop_med <- stats::median(d$population[is.finite(d$population)], na.rm = TRUE)
  if (!is.finite(pop_med)) pop_med <- 1
  d$pop_size <- ifelse(is.finite(d$population), d$population, pop_med)
  label_idx <- unique(c(order(-d$latest_value)[seq_len(min(4, nrow(d)))], which(d$location == result$meta$focus_location)))
  label_df <- d[label_idx, , drop = FALSE]
  th <- clinical_theme()
  base_size <- if (preview) 11 else 14
  smooth_layer <- if (nrow(d) >= 3 && length(unique(d$gradient_x)) >= 3) {
    ggplot2::geom_smooth(method = "lm", se = TRUE, colour = th$navy, fill = "#D7E6EF", linewidth = 1.05, alpha = 0.42)
  } else {
    NULL
  }
  p <- ggplot2::ggplot(d, ggplot2::aes(gradient_x, latest_value)) +
    smooth_layer +
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
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.06, 0.10))) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_lab,
      y = paste(result$meta$measure, result$meta$metric),
      caption = caption
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
    d2 <- result$trend_table
    d2 <- d2[is.finite(d2$percent_change), , drop = FALSE]
    if (nrow(d2) == 0) {
      draw_plot_message("EAPC 图未生成", c("当前数据没有足够年份拟合 EAPC，也没有可用的起止变化。"))
      return(invisible(NULL))
    }
    d2$change_pct <- 100 * d2$percent_change
    d2 <- d2[order(d2$change_pct), , drop = FALSE]
    d2 <- head(d2, 16)
    d2$location_label <- factor(short_label(d2$location, 22), levels = short_label(d2$location, 22))
    d2$trend <- ifelse(d2$change_pct > 0, "Increase", "Decrease")
    th <- clinical_theme()
    base_size <- if (preview) 11 else 14
    p <- ggplot2::ggplot(d2, ggplot2::aes(change_pct, location_label)) +
      ggplot2::geom_vline(xintercept = 0, colour = th$navy, linewidth = 0.75, linetype = "22") +
      ggplot2::geom_col(ggplot2::aes(fill = trend), width = 0.62, alpha = 0.9) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(fmt_num(change_pct, 1), "%")), hjust = ifelse(d2$change_pct >= 0, -0.12, 1.12), size = if (preview) 2.65 else 3.1, family = plot_family(), colour = th$ink) +
      ggplot2::scale_fill_manual(values = c(Increase = th$red, Decrease = th$teal)) +
      ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"), expand = ggplot2::expansion(mult = c(0.16, 0.18))) +
      ggplot2::labs(
        title = if (preview) "起止变化排名" else "年份不足时的起止相对变化排名",
        subtitle = "当前数据不足以拟合 EAPC，改用分析窗口起止年份的相对变化。",
        x = "相对变化",
        y = NULL,
        caption = "该图是 EAPC 的替代描述，不代表年均变化率。"
      ) +
      publication_theme(base_size = base_size, grid_y = FALSE) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(face = "bold"), legend.position = "top") +
      ggplot2::coord_cartesian(clip = "off")
    print(p)
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
  d <- d[is.finite(d$latest_value), , drop = FALSE]
  if (nrow(d) == 0) {
    draw_plot_message("象限图未生成", c("当前筛选没有可用于比较的估计值。"))
    return(invisible(NULL))
  }
  d$trend_signal <- ifelse(is.finite(d$eapc), d$eapc, ifelse(is.finite(d$percent_change), 100 * d$percent_change, 0))
  x_mid <- stats::median(d$latest_value, na.rm = TRUE)
  y_mid <- 0
  d$plot_size <- ifelse(is.finite(d$population), d$population, 1)
  d$priority <- ifelse(d$latest_value >= x_mid & d$trend_signal > 0, "High burden + rising",
                       ifelse(d$latest_value >= x_mid, "High burden", ifelse(d$trend_signal > 0, "Rising", "Lower priority")))
  label_idx <- unique(c(
    which(d$location == result$meta$focus_location),
    order(-d$latest_value)[seq_len(min(4, nrow(d)))],
    order(-d$trend_signal)[seq_len(min(3, nrow(d)))]
  ))
  label_df <- d[label_idx, , drop = FALSE]
  th <- clinical_theme()
  base_size <- if (preview) 11 else 14
  p <- ggplot2::ggplot(d, ggplot2::aes(latest_value, trend_signal)) +
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
      y = if (any(is.finite(d$eapc))) "EAPC（%/年）" else "相对变化（%）",
      caption = "气泡大小反映数据中的人口规模；人口缺失时使用统一大小。"
    ) +
    publication_theme(base_size = base_size, grid_y = TRUE) +
    ggplot2::theme(legend.position = "top")
  print(p)
}

draw_contribution_plot <- function(result, preview = FALSE) {
  if (!require_plot_packages()) {
    draw_plot_message("缺少绘图包", c("请安装 ggplot2 与 scales 后重新运行。"))
    return(invisible(NULL))
  }
  d <- result$contribution_table
  if (is.null(d) || nrow(d) == 0) {
    draw_plot_message("构成图未生成", c("当前筛选没有可用于构成分析的记录。"))
    return(invisible(NULL))
  }
  d <- d[order(-d$val), , drop = FALSE]
  if (nrow(d) > 12) {
    top <- d[seq_len(11), , drop = FALSE]
    other <- d[-seq_len(11), , drop = FALSE]
    d <- rbind(
      top,
      data.frame(
        dimension = top$dimension[[1]],
        component = "Other",
        location = top$location[[1]],
        year = top$year[[1]],
        val = sum(other$val, na.rm = TRUE),
        share = sum(other$share, na.rm = TRUE),
        lower = sum(other$lower, na.rm = TRUE),
        upper = sum(other$upper, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    )
  }
  d$component_label <- factor(short_label(d$component, 18), levels = rev(short_label(d$component, 18)))
  th <- clinical_theme()
  base_size <- if (preview) 11 else 14
  p <- ggplot2::ggplot(d, ggplot2::aes(val, component_label)) +
    ggplot2::geom_col(ggplot2::aes(fill = share), width = 0.66, colour = "#FFFFFF", linewidth = 0.35) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(fmt_pct(share), "  ", fmt_num(val, 2))), hjust = -0.08, size = if (preview) 2.6 else 3.05, family = plot_family(), colour = th$ink) +
    ggplot2::scale_fill_gradient(low = "#BFE1D4", high = th$red, labels = scales::label_percent(accuracy = 1)) +
    ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = ","), expand = ggplot2::expansion(mult = c(0.02, 0.26))) +
    ggplot2::labs(
      title = if (preview) "构成贡献" else "最新年份构成贡献",
      subtitle = paste0(d$location[[1]], " | ", d$year[[1]], " | 按 ", d$dimension[[1]], " 汇总"),
      x = paste(result$meta$measure, result$meta$metric),
      y = NULL,
      caption = "多病因/多年龄/多文件上传时，该图用于识别主要贡献来源。"
    ) +
    publication_theme(base_size = base_size, grid_y = FALSE) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(face = "bold"), legend.position = "right", legend.title = ggplot2::element_blank()) +
    ggplot2::coord_cartesian(clip = "off")
  print(p)
}

draw_age_pattern_plot <- function(result, preview = FALSE) {
  if (!require_plot_packages()) {
    draw_plot_message("缺少绘图包", c("请安装 ggplot2 与 scales 后重新运行。"))
    return(invisible(NULL))
  }
  d <- result$age_pattern_table
  if (is.null(d) || nrow(d) == 0) {
    draw_plot_message("年龄谱图未生成", c("当前筛选没有年龄分层。"))
    return(invisible(NULL))
  }
  d <- d[order(-d$val), , drop = FALSE]
  d <- head(d, 18)
  d$age_label <- factor(short_label(d$age, 16), levels = rev(short_label(d$age, 16)))
  th <- clinical_theme()
  base_size <- if (preview) 11 else 14
  p <- ggplot2::ggplot(d, ggplot2::aes(val, age_label)) +
    ggplot2::geom_segment(ggplot2::aes(x = lower, xend = upper, yend = age_label), colour = "#AABBB7", linewidth = 0.95, lineend = "round") +
    ggplot2::geom_point(shape = 21, fill = th$teal, colour = "#FFFFFF", stroke = 0.9, size = 4.2) +
    ggplot2::geom_text(ggplot2::aes(label = fmt_num(val, 2)), hjust = -0.12, size = if (preview) 2.55 else 3.0, family = plot_family(), colour = th$ink) +
    ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = ","), expand = ggplot2::expansion(mult = c(0.03, 0.20))) +
    ggplot2::labs(
      title = if (preview) "年龄谱" else "最新年份年龄分层谱",
      subtitle = paste0(d$location[[1]], " | ", d$year[[1]], " | 点为估计值，线为区间"),
      x = paste(result$meta$measure, result$meta$metric),
      y = NULL,
      caption = "适合判断疾病/暴露负担主要集中在哪些年龄层；若仅有 All Ages，则作为总量展示。"
    ) +
    publication_theme(base_size = base_size, grid_y = FALSE) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(face = "bold")) +
    ggplot2::coord_cartesian(clip = "off")
  print(p)
}

draw_waterfall_plot <- function(result, preview = FALSE) {
  if (!require_plot_packages()) {
    draw_plot_message("缺少绘图包", c("请安装 ggplot2 与 scales 后重新运行。"))
    return(invisible(NULL))
  }
  d <- result$trend_table
  d <- d[is.finite(d$absolute_change), , drop = FALSE]
  if (nrow(d) == 0) {
    draw_plot_message("变化瀑布图未生成", c("当前筛选没有可用于起止变化分析的记录。"))
    return(invisible(NULL))
  }
  d <- d[order(abs(d$absolute_change), decreasing = TRUE), , drop = FALSE]
  d <- head(d, 18)
  d <- d[order(d$absolute_change), , drop = FALSE]
  d$location_label <- factor(short_label(d$location, 22), levels = short_label(d$location, 22))
  d$direction <- ifelse(d$absolute_change >= 0, "Increase", "Decrease")
  th <- clinical_theme()
  base_size <- if (preview) 11 else 14
  p <- ggplot2::ggplot(d, ggplot2::aes(absolute_change, location_label)) +
    ggplot2::geom_vline(xintercept = 0, colour = "#AFC2BD", linewidth = 0.8, linetype = "22") +
    ggplot2::geom_col(ggplot2::aes(fill = direction), width = 0.68, alpha = 0.92) +
    ggplot2::geom_text(ggplot2::aes(label = fmt_num(absolute_change, 2)), hjust = ifelse(d$absolute_change >= 0, -0.12, 1.12), size = if (preview) 2.55 else 3.0, family = plot_family(), colour = th$ink) +
    ggplot2::scale_fill_manual(values = c(Increase = th$red, Decrease = th$teal)) +
    ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = ","), expand = ggplot2::expansion(mult = c(0.18, 0.18))) +
    ggplot2::labs(
      title = if (preview) "变化瀑布图" else "起止年份绝对变化瀑布图",
      subtitle = paste0(result$meta$start_year, "-", result$meta$end_year, " | 按绝对变化幅度筛选"),
      x = paste("变化量：", result$meta$measure, result$meta$metric),
      y = NULL,
      caption = "用于快速识别变化贡献最大的地区；正值表示上升，负值表示下降。"
    ) +
    publication_theme(base_size = base_size, grid_y = FALSE) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(face = "bold"), legend.position = "top") +
    ggplot2::coord_cartesian(clip = "off")
  print(p)
}

draw_bump_rank_plot <- function(result, preview = FALSE) {
  if (!require_plot_packages()) {
    draw_plot_message("缺少绘图包", c("请安装 ggplot2 与 scales 后重新运行。"))
    return(invisible(NULL))
  }
  d <- result$plot_data
  if (nrow(d) == 0 || length(unique(d$year)) < 2 || length(unique(d$location)) < 2) {
    draw_plot_message("排名迁移图未生成", c("至少需要 2 个地区和 2 个年份。"))
    return(invisible(NULL))
  }
  d <- d[is.finite(d$val), , drop = FALSE]
  years_keep <- unique(round(seq(min(d$year), max(d$year), length.out = min(8, length(unique(d$year))))))
  years_keep <- sort(unique(d$year[d$year %in% years_keep]))
  if (length(years_keep) < 2) years_keep <- sort(unique(d$year))
  latest_locations <- result$trend_table$location[seq_len(min(10, nrow(result$trend_table)))]
  d <- d[d$year %in% years_keep & d$location %in% latest_locations, , drop = FALSE]
  d$rank <- ave(-d$val, d$year, FUN = function(x) rank(x, ties.method = "first"))
  d$location_label <- short_label(d$location, 14)
  th <- clinical_theme()
  base_size <- if (preview) 11 else 14
  latest <- d[d$year == max(d$year), , drop = FALSE]
  p <- ggplot2::ggplot(d, ggplot2::aes(year, rank, group = location, colour = location)) +
    ggplot2::geom_line(linewidth = 1.2, alpha = 0.86, lineend = "round") +
    ggplot2::geom_point(shape = 21, fill = "#FFFFFF", stroke = 0.9, size = 3.0) +
    {
      if (requireNamespace("ggrepel", quietly = TRUE)) {
        ggrepel::geom_text_repel(data = latest, ggplot2::aes(label = location_label), direction = "y", nudge_x = 0.5, hjust = 0, family = plot_family(), size = if (preview) 2.45 else 2.95, colour = th$ink, segment.colour = "#B6C6C1", segment.size = 0.22, show.legend = FALSE)
      } else {
        ggplot2::geom_text(data = latest, ggplot2::aes(label = location_label), hjust = -0.05, family = plot_family(), size = if (preview) 2.45 else 2.95, show.legend = FALSE)
      }
    } +
    ggplot2::scale_colour_manual(values = clinical_cols(length(unique(d$location)))) +
    ggplot2::scale_y_reverse(breaks = sort(unique(d$rank)), expand = ggplot2::expansion(mult = c(0.06, 0.12))) +
    ggplot2::scale_x_continuous(breaks = years_keep, expand = ggplot2::expansion(mult = c(0.03, 0.16))) +
    ggplot2::labs(
      title = if (preview) "排名迁移" else "地区负担排名迁移图",
      subtitle = "显示不同年份中高负担地区的排序变化，适合讲趋势故事。",
      x = NULL,
      y = "Rank",
      caption = "Rank 1 表示当年最高负担；右侧标签为最新年份地区。"
    ) +
    publication_theme(base_size = base_size) +
    ggplot2::theme(legend.position = "none", axis.text.y = ggplot2::element_text(face = "bold")) +
    ggplot2::coord_cartesian(clip = "off")
  print(p)
}

draw_small_multiples_plot <- function(result, preview = FALSE) {
  if (!require_plot_packages()) {
    draw_plot_message("缺少绘图包", c("请安装 ggplot2 与 scales 后重新运行。"))
    return(invisible(NULL))
  }
  d <- result$plot_data
  if (nrow(d) == 0) {
    draw_plot_message("小多图未生成", c("当前筛选没有可用时间序列。"))
    return(invisible(NULL))
  }
  d <- d[d$location %in% unique(result$trend_table$location[seq_len(min(12, nrow(result$trend_table)))]), , drop = FALSE]
  d$location_label <- short_label(d$location, 18)
  th <- clinical_theme()
  base_size <- if (preview) 10 else 13
  p <- ggplot2::ggplot(d, ggplot2::aes(year, val)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = "#DCECE6", alpha = 0.75) +
    ggplot2::geom_line(colour = th$teal, linewidth = 1.05, lineend = "round") +
    ggplot2::geom_point(data = d[d$year == max(d$year), , drop = FALSE], shape = 21, fill = th$red, colour = "#FFFFFF", size = 2.5, stroke = 0.7) +
    ggplot2::facet_wrap(~ location_label, scales = "free_y", ncol = if (preview) 4 else 3) +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
    ggplot2::labs(
      title = if (preview) "地区小多图" else "地区趋势小多图",
      subtitle = "同一筛选下多个地区的时间轨迹并列展示，能直观看到异质性。",
      x = NULL,
      y = paste(result$meta$measure, result$meta$metric),
      caption = "每个小图使用独立 y 轴，适合观察形态；比较绝对值请结合排名表。"
    ) +
    publication_theme(base_size = base_size) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", colour = th$ink),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "none"
    )
  print(p)
}

draw_distribution_plot <- function(result, preview = FALSE) {
  if (!require_plot_packages()) {
    draw_plot_message("缺少绘图包", c("请安装 ggplot2 与 scales 后重新运行。"))
    return(invisible(NULL))
  }
  d <- result$series
  d <- d[d$year >= result$meta$start_year & d$year <= result$meta$end_year & is.finite(d$val), , drop = FALSE]
  if (nrow(d) == 0) {
    draw_plot_message("分布图未生成", c("当前筛选没有可用于分布分析的记录。"))
    return(invisible(NULL))
  }
  years <- sort(unique(d$year))
  keep_years <- unique(round(seq(min(years), max(years), length.out = min(8, length(years)))))
  keep_years <- years[years %in% keep_years]
  if (length(keep_years) < 2) keep_years <- years
  d <- d[d$year %in% keep_years, , drop = FALSE]
  th <- clinical_theme()
  base_size <- if (preview) 11 else 14
  p <- ggplot2::ggplot(d, ggplot2::aes(factor(year), val)) +
    ggplot2::geom_boxplot(width = 0.55, outlier.shape = 21, outlier.fill = "#FFFFFF", outlier.colour = th$red, colour = "#7E928D", fill = "#EAF4F0", linewidth = 0.65) +
    ggplot2::geom_jitter(width = 0.12, size = 1.6, alpha = 0.42, colour = th$teal) +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ","), expand = ggplot2::expansion(mult = c(0.04, 0.12))) +
    ggplot2::labs(
      title = if (preview) "地区分布" else "地区负担分布演变",
      subtitle = "比较不同年份各地区估计值的离散程度和异常高值。",
      x = NULL,
      y = paste(result$meta$measure, result$meta$metric),
      caption = "箱线表示地区分布，中位数和离散度可辅助描述不平等/异质性。"
    ) +
    publication_theme(base_size = base_size) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
  print(p)
}

draw_share_stream_plot <- function(result, preview = FALSE) {
  if (!require_plot_packages()) {
    draw_plot_message("缺少绘图包", c("请安装 ggplot2 与 scales 后重新运行。"))
    return(invisible(NULL))
  }
  s <- result$selected
  focus <- result$meta$focus_location
  d_focus <- s
  if (focus %in% s$location) d_focus <- s[s$location == focus, , drop = FALSE]
  if (length(unique(d_focus$cause)) > 1) {
    d <- d_focus
    dimension <- "cause"
    place <- focus
  } else if (length(unique(d_focus$age)) > 1) {
    d <- d_focus
    dimension <- "age"
    place <- focus
  } else if (length(unique(s$location)) > 1) {
    d <- s
    dimension <- "location"
    place <- "Selected locations"
  } else if (length(unique(d_focus$measure)) > 1) {
    d <- d_focus
    dimension <- "measure"
    place <- focus
  } else {
    d <- d_focus
    dimension <- "source_file"
    place <- focus
  }
  grouped <- aggregate(d["val"], by = list(year = d$year, component = d[[dimension]]), FUN = function(x) sum(x[is.finite(x)], na.rm = TRUE))
  if (nrow(grouped) == 0 || length(unique(grouped$year)) < 2) {
    draw_plot_message("构成流图未生成", c("至少需要 2 个年份才能展示构成随时间变化。"))
    return(invisible(NULL))
  }
  latest <- grouped[grouped$year == max(grouped$year), , drop = FALSE]
  keep <- head(latest$component[order(-latest$val)], 8)
  grouped$component <- ifelse(grouped$component %in% keep, grouped$component, "Other")
  grouped <- aggregate(grouped["val"], by = grouped[c("year", "component")], FUN = sum, na.rm = TRUE)
  totals <- aggregate(val ~ year, grouped, sum, na.rm = TRUE)
  grouped$total <- totals$val[match(grouped$year, totals$year)]
  grouped$share <- ifelse(grouped$total > 0, grouped$val / grouped$total, NA_real_)
  grouped$component_label <- short_label(grouped$component, 16)
  th <- clinical_theme()
  base_size <- if (preview) 11 else 14
  p <- ggplot2::ggplot(grouped, ggplot2::aes(year, share, fill = component_label)) +
    ggplot2::geom_area(colour = "#FFFFFF", linewidth = 0.22, alpha = 0.94) +
    ggplot2::scale_fill_manual(values = clinical_cols(length(unique(grouped$component_label)))) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1), expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0), breaks = pretty(grouped$year, n = if (preview) 6 else 9)) +
    ggplot2::labs(
      title = if (preview) "构成流图" else "构成随时间变化流图",
      subtitle = paste0(place, " | 按 ", dimension, " 展示份额变化"),
      x = NULL,
      y = "Share",
      caption = "适合病因拆分、年龄拆分或多地区比较；显示结构而非绝对负担。"
    ) +
    publication_theme(base_size = base_size) +
    ggplot2::theme(legend.position = "right", panel.grid.major = ggplot2::element_blank())
  print(p)
}

draw_uncertainty_fan_plot <- function(result, preview = FALSE) {
  if (!require_plot_packages()) {
    draw_plot_message("缺少绘图包", c("请安装 ggplot2 与 scales 后重新运行。"))
    return(invisible(NULL))
  }
  d <- result$focus_series
  if (nrow(d) == 0 || length(unique(d$year)) < 2) {
    draw_plot_message("不确定性扇形图未生成", c("焦点地区至少需要 2 个年份。"))
    return(invisible(NULL))
  }
  d$ui_width <- d$upper - d$lower
  d$relative_ui <- ifelse(d$val != 0, d$ui_width / abs(d$val), NA_real_)
  th <- clinical_theme()
  base_size <- if (preview) 11 else 14
  p <- ggplot2::ggplot(d, ggplot2::aes(year, val)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, fill = relative_ui), alpha = 0.78) +
    ggplot2::geom_line(colour = th$ink, linewidth = 1.35, lineend = "round") +
    ggplot2::geom_point(shape = 21, fill = "#FFFFFF", colour = th$red, stroke = 0.9, size = 3.2) +
    ggplot2::scale_fill_gradient(low = "#DDEFE9", high = th$red, labels = scales::label_percent(accuracy = 1), name = "Relative UI") +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ","), expand = ggplot2::expansion(mult = c(0.05, 0.12))) +
    ggplot2::labs(
      title = if (preview) "不确定性扇形" else paste0(result$meta$focus_location, " 不确定性扇形图"),
      subtitle = "同时展示估计值、上下限与相对不确定性宽度。",
      x = NULL,
      y = paste(result$meta$measure, result$meta$metric),
      caption = "颜色越深表示相对不确定性越宽，结果解释应更谨慎。"
    ) +
    publication_theme(base_size = base_size) +
    ggplot2::theme(legend.position = "right")
  print(p)
}

draw_storyboard_plot <- function(result, preview = FALSE) {
  th <- clinical_theme()
  t <- result$trend_table
  if (nrow(t) == 0) {
    draw_plot_message("综合看板未生成", c("当前筛选没有可用于总览的结果。"))
    return(invisible(NULL))
  }
  focus <- t[t$location == result$meta$focus_location, , drop = FALSE]
  if (nrow(focus) == 0) focus <- t[1, , drop = FALSE]
  top <- head(t[order(-t$latest_value), , drop = FALSE], 6)
  top$bar <- top$latest_value / max(top$latest_value, na.rm = TRUE)
  contrib <- result$contribution_table
  contrib <- contrib[order(-contrib$val), , drop = FALSE]
  contrib <- head(contrib, 5)
  contrib$bar <- if (max(contrib$val, na.rm = TRUE) > 0) contrib$val / max(contrib$val, na.rm = TRUE) else 0
  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(fill = "#F5F8F7", col = NA))
  grid::grid.roundrect(x = 0.5, y = 0.5, width = 0.94, height = 0.90, r = grid::unit(14, "pt"),
                       gp = grid::gpar(fill = "#FFFFFF", col = "#D8E7E2", lwd = 1.2))
  grid::grid.text("GBD Result Intelligence Board", x = 0.07, y = 0.91, just = "left",
                  gp = grid::gpar(col = th$ink, fontsize = if (preview) 20 else 24, fontface = "bold", fontfamily = plot_family()))
  grid::grid.text(paste0(result$meta$cause, " | ", result$meta$measure, " / ", result$meta$metric, " | ", result$meta$start_year, "-", result$meta$end_year),
                  x = 0.07, y = 0.865, just = "left",
                  gp = grid::gpar(col = th$muted, fontsize = if (preview) 10 else 12, fontfamily = plot_family()))
  card_x <- c(0.18, 0.39, 0.60, 0.81)
  card_titles <- c("分析记录", "地区数", "焦点最新值", "变化信号")
  signal <- if (is.finite(focus$eapc)) paste0(fmt_num(focus$eapc, 2), "%/年") else fmt_pct(focus$percent_change)
  card_values <- c(format(nrow(result$selected), big.mark = ","), result$meta$n_locations, fmt_num(focus$latest_value, 2), signal)
  card_subs <- c(result$meta$data_type, result$meta$year_range, result$meta$focus_location, "EAPC 或起止变化")
  for (i in seq_along(card_x)) {
    grid::grid.roundrect(x = card_x[[i]], y = 0.76, width = 0.18, height = 0.13, r = grid::unit(10, "pt"),
                         gp = grid::gpar(fill = c("#F2FAF7", "#F7F8FE", "#FFF7EC", "#F8F4FC")[[i]], col = "#DCE8E3", lwd = 1))
    grid::grid.text(card_titles[[i]], x = card_x[[i]] - 0.07, y = 0.79, just = "left", gp = grid::gpar(col = th$muted, fontsize = 9.5, fontfamily = plot_family()))
    grid::grid.text(card_values[[i]], x = card_x[[i]] - 0.07, y = 0.755, just = "left", gp = grid::gpar(col = th$ink, fontsize = if (preview) 16 else 19, fontface = "bold", fontfamily = plot_family()))
    grid::grid.text(card_subs[[i]], x = card_x[[i]] - 0.07, y = 0.715, just = "left", gp = grid::gpar(col = th$muted, fontsize = 8.8, fontfamily = plot_family()))
  }
  grid::grid.roundrect(x = 0.29, y = 0.43, width = 0.42, height = 0.46, r = grid::unit(12, "pt"), gp = grid::gpar(fill = "#FAFCFB", col = "#E1ECE8"))
  grid::grid.text("High-Burden Ranking", x = 0.10, y = 0.62, just = "left", gp = grid::gpar(col = th$ink, fontsize = 14, fontface = "bold", fontfamily = plot_family()))
  y0 <- 0.57
  for (i in seq_len(nrow(top))) {
    y <- y0 - (i - 1) * 0.055
    grid::grid.text(short_label(top$location[[i]], 20), x = 0.10, y = y, just = "left", gp = grid::gpar(col = th$ink, fontsize = 9, fontfamily = plot_family()))
    grid::grid.roundrect(x = 0.31, y = y, width = 0.20, height = 0.022, r = grid::unit(5, "pt"), gp = grid::gpar(fill = "#ECF2F0", col = NA))
    grid::grid.roundrect(x = 0.21 + 0.10 * top$bar[[i]], y = y, width = 0.20 * top$bar[[i]], height = 0.022, r = grid::unit(5, "pt"), gp = grid::gpar(fill = clinical_cols(6)[[i]], col = NA))
    grid::grid.text(fmt_num(top$latest_value[[i]], 2), x = 0.53, y = y, just = "right", gp = grid::gpar(col = th$muted, fontsize = 8.5, fontfamily = plot_family()))
  }
  grid::grid.roundrect(x = 0.74, y = 0.43, width = 0.36, height = 0.46, r = grid::unit(12, "pt"), gp = grid::gpar(fill = "#FAFCFB", col = "#E1ECE8"))
  grid::grid.text("Contribution Structure", x = 0.59, y = 0.62, just = "left", gp = grid::gpar(col = th$ink, fontsize = 14, fontface = "bold", fontfamily = plot_family()))
  y1 <- 0.57
  for (i in seq_len(nrow(contrib))) {
    y <- y1 - (i - 1) * 0.060
    grid::grid.text(short_label(contrib$component[[i]], 20), x = 0.59, y = y, just = "left", gp = grid::gpar(col = th$ink, fontsize = 9, fontfamily = plot_family()))
    grid::grid.roundrect(x = 0.78, y = y, width = 0.18, height = 0.024, r = grid::unit(5, "pt"), gp = grid::gpar(fill = "#ECF2F0", col = NA))
    grid::grid.roundrect(x = 0.69 + 0.09 * contrib$bar[[i]], y = y, width = 0.18 * contrib$bar[[i]], height = 0.024, r = grid::unit(5, "pt"), gp = grid::gpar(fill = c(th$red, th$teal, th$navy, th$amber, th$violet)[[i]], col = NA))
    grid::grid.text(fmt_pct(contrib$share[[i]]), x = 0.90, y = y, just = "right", gp = grid::gpar(col = th$muted, fontsize = 8.5, fontfamily = plot_family()))
  }
  grid::grid.roundrect(x = 0.5, y = 0.15, width = 0.84, height = 0.16, r = grid::unit(12, "pt"),
                       gp = grid::gpar(fill = "#F8FBFA", col = "#E1ECE8"))
  insight <- paste(result$insights, collapse = "  ")
  grid::grid.text("Interpretation-ready Signal", x = 0.10, y = 0.20, just = "left",
                  gp = grid::gpar(col = th$red, fontsize = 12, fontface = "bold", fontfamily = plot_family()))
  grid::grid.text(paste(strwrap(insight, width = if (preview) 118 else 135), collapse = "\n"), x = 0.10, y = 0.135, just = "left",
                  gp = grid::gpar(col = th$ink, fontsize = if (preview) 8.5 else 10, lineheight = 1.2, fontfamily = plot_family()))
}
