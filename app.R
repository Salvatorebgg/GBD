library(shiny)
library(htmltools)

options(sass.cache = FALSE, shiny.maxRequestSize = 300 * 1024^2)

source(file.path("R", "data.R"), encoding = "UTF-8")
source(file.path("R", "analysis.R"), encoding = "UTF-8")

# ---------- UI helpers ----------
metric_card <- function(label, value, sub = NULL) {
  div(class = "metric-card",
      div(class = "metric-label", label),
      div(class = "metric-value", value),
      if (!is.null(sub)) div(class = "metric-sub", sub))
}

panel_head <- function(step, title, desc = NULL) {
  div(class = "panel-head",
      span(class = "step", step),
      div(h2(title), if (!is.null(desc)) p(desc)))
}

tip_box <- function(text, type = "info") {
  div(class = paste("status-msg", type), text)
}

template_card <- function(title, tag, body) {
  div(class = "template-card",
      div(class = "template-card-head",
          h4(title), span(class = "template-tag", tag)),
      div(class = "template-card-body", body))
}

empty_state <- function(title, text) {
  div(class = "empty-state",
      div(class = "empty-ring"),
      h4(title), p(text))
}

compact_table <- function(data) {
  data_expr <- substitute(data)
  data_env <- parent.frame()
  renderTable({
    eval(data_expr, data_env)
  }, striped = TRUE, hover = TRUE, bordered = FALSE, spacing = "s", width = "100%")
}

soft_badge <- function(text, cls = "badge-mint") {
  span(class = paste("soft-badge", cls), text)
}

resource_button <- function(output_id, label) {
  downloadButton(output_id, label, class = "download-btn")
}

# ---------- GBD-specific UI components ----------
gbd_download_plan_panel <- function(topic) {
  manifest <- gbd_download_manifest(topic)
  compact <- manifest[manifest$item %in% c("Measure", "Cause", "Metric", "Age", "Sex", "Year", "Location"), , drop = FALSE]
  div(class = "download-plan",
      div(class = "download-plan-head",
          div(div(class = "section-title", "下载清单"), p(class = "muted-line", "在 IHME Results Tool 中匹配以下字段，导出为 CSV。")),
          div(class = "download-actions-row",
              tags$a("GBD Results Tool", href = gbd_results_tool_url(), target = "_blank", class = "mini-link primary-link"),
              tags$a("GHDx", href = gbd_ghdx_url(), target = "_blank", class = "mini-link"))
      ),
      div(class = "keyword-strip",
          lapply(compact$value, function(x) span(class = "keyword-pill", x))),
      div(class = "route-steps",
          div(class = "route-step active", span(class = "route-num", "1"), div(strong("测量指标"), p(compact$value[compact$item == "Measure"]))),
          div(class = "route-step", span(class = "route-num", "2"), div(strong("病因/风险因素"), p(compact$value[compact$item == "Cause"]))),
          div(class = "route-step", span(class = "route-num", "3"), div(strong("度量/年龄/性别"), p(paste(compact$value[compact$item %in% c("Metric", "Age", "Sex")], collapse = " / ")))),
          div(class = "route-step", span(class = "route-num", "4"), div(strong("CSV 导出"), p("保留 val、lower、upper；在步骤 03 上传。")))
      ),
      div(class = "status-msg info", style = "margin-top:10px;",
          "新手建议：一个 Measure + 一个 Cause + Rate + Age-standardized + Both + 多国家/地区 + 全部年份。")
  )
}

gbd_official_cards <- function(topic) {
  manifest <- gbd_download_manifest(topic)
  rows <- manifest[manifest$item %in% c("Measure", "Cause", "Metric", "Age", "Sex", "Year", "Location"), , drop = FALSE]
  div(class = "file-rec-grid",
      lapply(seq_len(nrow(rows)), function(i) {
        div(class = if (i == 1) "file-rec-card recommended" else "file-rec-card",
            div(class = "file-rec-top",
                span(class = "file-rec-module", rows$item[[i]]),
                span(class = "file-rec-pos", "GBD 字段")),
            h4(rows$value[[i]]),
            p(rows$note[[i]]),
            div(class = "file-rec-vars", paste0("字段: ", rows$item[[i]])),
            div(class = "file-rec-actions",
                tags$a("Results Tool", href = gbd_results_tool_url(), target = "_blank"),
                tags$a("GHDx", href = gbd_ghdx_url(), target = "_blank"))
        )
      })
  )
}

annotated_ihme_guide <- function() {
  div(class = "shot",
      div(class = "shot-head",
          span(class = "dot red"), span(class = "dot yellow"), span(class = "dot green"),
          div(class = "urlbar", gbd_results_tool_url())),
      div(class = "shot-body",
          div(class = "shot-title", "IHME GBD Results Tool - 选择字段并导出 CSV"),
          div(class = "mock-tabs",
              div(class = "mock-tab active", "Measure"),
              div(class = "mock-tab", "Cause"),
              div(class = "mock-tab", "Metric"),
              div(class = "mock-tab", "Age"),
              div(class = "mock-tab", "Location")),
          div(class = "file-item",
              div(class = "file-icon", "1"),
              div(div(class = "file-name", "Measure = DALYs / Deaths / Prevalence"), div(class = "file-desc", "先确定负担类型")),
              div(class = "file-size", "必选"),
              tags$button(class = "file-dl", "选择")),
          div(class = "file-item",
              div(class = "file-icon", "2"),
              div(div(class = "file-name", "Metric = Rate; Age = Age-standardized"), div(class = "file-desc", "跨国比较请用年龄标化率")),
              div(class = "file-size", "推荐"),
              tags$button(class = "file-dl", "应用")),
          div(class = "file-item",
              div(class = "file-icon", "CSV"),
              div(div(class = "file-name", "下载结果为 CSV"), div(class = "file-desc", "不要改列名，直接上传")),
              div(class = "file-size", "val/UI"),
              tags$button(class = "file-dl", "下载"))
      )
  )
}

# ---------- UI ----------
ui <- fluidPage(
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title("GBD 数据库工作台"),
    tags$link(rel = "stylesheet", href = "css/gbd-theme.css?v=20260526-v3"),
    tags$link(rel = "stylesheet", href = "css/gbd-layout.css?v=20260526-v3"),
    tags$link(rel = "stylesheet", href = "css/gbd-components.css?v=20260526-v3"),
    tags$style(HTML("
      .gbd-busy-mask {
        position: fixed;
        inset: 0;
        z-index: 9999;
        display: none;
        align-items: center;
        justify-content: center;
        background: rgba(247, 250, 248, 0.72);
        backdrop-filter: blur(3px);
      }
      .gbd-busy-mask.is-visible { display: flex; }
      .gbd-busy-card {
        width: min(420px, calc(100vw - 40px));
        padding: 26px 28px;
        border: 1px solid #DCE8E3;
        border-radius: 12px;
        background: #FFFFFF;
        box-shadow: 0 22px 60px rgba(20, 42, 46, 0.14);
        text-align: left;
      }
      .gbd-busy-row {
        display: flex;
        gap: 16px;
        align-items: center;
      }
      .gbd-busy-spinner {
        width: 42px;
        height: 42px;
        border-radius: 999px;
        border: 4px solid #DCE8E3;
        border-top-color: #0E7C7B;
        animation: gbdSpin 0.9s linear infinite;
        flex: 0 0 auto;
      }
      .gbd-busy-title {
        margin: 0;
        color: #142A2E;
        font-size: 22px;
        font-weight: 800;
        letter-spacing: 0;
      }
      .gbd-busy-text {
        margin: 6px 0 0;
        color: #687C82;
        font-size: 13px;
        line-height: 1.55;
      }
      .gbd-busy-dismiss {
        margin-top: 14px;
        border: 1px solid #DCE8E3;
        border-radius: 8px;
        background: #F7FAF8;
        color: #142A2E;
        font-size: 12px;
        font-weight: 700;
        padding: 7px 12px;
        cursor: pointer;
      }
      .gbd-busy-dismiss:hover { background: #EEF6F3; }
      body.gbd-busy-active .btn-generate,
      body.gbd-busy-active .download-btn,
      body.gbd-busy-active .hero-action {
        pointer-events: none;
        opacity: 0.72;
      }
      @keyframes gbdSpin { to { transform: rotate(360deg); } }
    ")),
    tags$script(HTML("
      (function() {
        var busyTimer = null;
        var busyDelayTimer = null;
        var busyHardTimer = null;
        var minVisibleUntil = 0;
        function mask() { return document.getElementById('gbd_busy_mask'); }
        function showBusy(text, maxMs) {
          var el = mask();
          if (!el) return;
          var body = document.body;
          var msg = el.querySelector('[data-busy-message]');
          if (msg && text) msg.textContent = text;
          minVisibleUntil = Date.now() + 650;
          window.clearTimeout(busyTimer);
          window.clearTimeout(busyHardTimer);
          el.classList.add('is-visible');
          if (body) body.classList.add('gbd-busy-active');
          busyHardTimer = window.setTimeout(function() {
            hideBusy(true);
          }, maxMs || 120000);
        }
        function showBusySoon(text, delayMs, maxMs) {
          window.clearTimeout(busyDelayTimer);
          busyDelayTimer = window.setTimeout(function() {
            showBusy(text, maxMs);
          }, delayMs || 650);
        }
        function hideBusy(force) {
          window.clearTimeout(busyDelayTimer);
          var wait = force ? 0 : Math.max(0, minVisibleUntil - Date.now());
          window.clearTimeout(busyTimer);
          busyTimer = window.setTimeout(function() {
            var el = mask();
            if (el) el.classList.remove('is-visible');
            if (document.body) document.body.classList.remove('gbd-busy-active');
            window.clearTimeout(busyHardTimer);
          }, wait);
        }
        document.addEventListener('change', function(e) {
          if (e.target && e.target.type === 'file' && e.target.files && e.target.files.length > 0) {
            showBusy('正在上传并识别 GBD 文件；大文件、ZIP 或 TIF 可能需要更久，请不要重复点击。', 180000);
          }
        }, true);
        document.addEventListener('click', function(e) {
          if (e.target && e.target.closest && e.target.closest('[data-busy-dismiss]')) {
            hideBusy(true);
            return;
          }
          var target = e.target && e.target.closest ? e.target.closest('#run_analysis, .btn-generate, .hero-action') : null;
          if (target) showBusy('分析中，请稍等。系统正在清洗、建模并生成图表。', 120000);
        }, true);
        document.addEventListener('shiny:busy', function() {
          showBusySoon('分析中，请稍等。大型 GBD 文件正在读取、清洗或绘图。', 700, 120000);
        });
        document.addEventListener('shiny:idle', hideBusy);
        function bindJqueryBusyEvents() {
          if (!window.jQuery) {
            window.setTimeout(bindJqueryBusyEvents, 200);
            return;
          }
          window.jQuery(document)
            .on('shiny:busy', function() {
              showBusySoon('分析中，请稍等。大型 GBD 文件正在读取、清洗或绘图。', 700, 120000);
            })
            .on('shiny:idle', hideBusy);
        }
        bindJqueryBusyEvents();
        window.gbdShowBusy = showBusy;
        window.gbdHideBusy = hideBusy;
      })();

      Shiny.addCustomMessageHandler('clearFileInput', function(id) {
        var root = document.getElementById(id);
        if (!root) return;
        var file = root.querySelector('input[type=\"file\"]');
        if (file) {
          file.value = '';
          file.dispatchEvent(new Event('change', { bubbles: true }));
        }
        var progress = root.querySelector('.progress');
        if (progress) progress.style.display = 'none';
        var name = root.querySelector('.form-control');
        if (name && name.value !== undefined) name.value = '';
      });
      Shiny.addCustomMessageHandler('gbdBusy', function(message) {
        if (message && message.show === false) {
          if (window.gbdHideBusy) window.gbdHideBusy();
        } else {
          if (window.gbdShowBusy) window.gbdShowBusy((message && message.text) || '分析中，请稍等。', (message && message.maxMs) || 120000);
        }
      });
    "))
  ),

  div(
    id = "gbd_busy_mask",
    class = "gbd-busy-mask",
    div(
      class = "gbd-busy-card",
      div(
        class = "gbd-busy-row",
        div(class = "gbd-busy-spinner"),
        div(
          h3(class = "gbd-busy-title", "分析中，请稍等"),
          p(class = "gbd-busy-text", `data-busy-message` = TRUE,
            "系统正在读取、清洗、建模并生成图表。大型 GBD 文件可能需要几十秒，请不要重复点击。"),
          tags$button(type = "button", class = "gbd-busy-dismiss", `data-busy-dismiss` = TRUE, "隐藏提示")
        )
      )
    )
  ),

  div(class = "app-shell",

      # ---- Topbar ----
      tags$header(class = "topbar",
          div(class = "topbar-left",
              h1("GBD 数据库工作台"),
              span(class = "topbar-sub", "全球疾病负担研究 - 全流程工作台")
          ),
          div(class = "flow-line",
              span(class = "is-active", "研究设计"),
              span("数据下载"),
              span("上传清洗"),
              span("图表分析"),
              span("写作导出")
          )
      ),

      # ---- Command Hero ----
      tags$section(class = "command-hero",
          div(class = "command-copy",
              div(class = "hero-kicker", "GBD 工作流"),
              div(class = "hero-title", "从研究问题到论文成稿"),
              div(class = "hero-subtitle", "设计研究方案、下载 GBD 数据、上传自动清洗、分析建模、生成出版级图表与写作模板——一站式完成。")
          ),
          div(class = "hero-actions",
              actionButton("hero_start", label = tagList(
                  span(class = "hero-action-title", "下载数据"),
                  span(class = "hero-action-sub", "IHME Results Tool")
              ), class = "hero-action primary"),
              actionButton("hero_path", label = tagList(
                  span(class = "hero-action-title", "全流程"),
                  span(class = "hero-action-sub", "五步工作流")
              ), class = "hero-action secondary"),
              actionButton("hero_demo", label = tagList(
                  span(class = "hero-action-title", "查看结果"),
                  span(class = "hero-action-sub", "图表与报告")
              ), class = "hero-action secondary")
          ),
          div(class = "summary-strip",
              div(class = "summary-card", span("年份"), strong("1990+"), tags$small("长期趋势")),
              div(class = "summary-card", span("UI"), strong("95%"), tags$small("不确定区间")),
              div(class = "summary-card", span("EAPC"), strong("趋势"), tags$small("年均百分比变化")),
              div(class = "summary-card", span("流程"), strong("一站式"), tags$small("上传到写作"))
          )
      ),

      # ---- Main Tabs ----
      tabsetPanel(
        id = "main_tab",
        type = "pills",

        # ==================== STEP 01: Study Design ====================
        tabPanel(
          title = "研究设计",
          value = "design",
          div(class = "workbench-grid",
              tags$aside(class = "input-panel",
                  div(class = "panel-section",
                      panel_head("01", "数据库适配性", "GBD 是否适合你的研究问题？"),
                      div(class = "status-msg info",
                          "GBD 提供全球可比的模型估计数据。适用于疾病负担趋势、地区比较和健康差异分析。不适用于个体因果推断。"),
                      tags$ul(style = "font-size:12px;color:var(--text);line-height:1.8;padding-left:16px;",
                          tags$li(tags$strong("最适合："), "负担趋势、死亡率/DALYs/患病率、国家间比较"),
                          tags$li(tags$strong("谨慎使用："), "生态学分析、SDI 梯度、短期外推"),
                          tags$li(tags$strong("不适合："), "个体风险预测、治疗效果、临床因果推断"))
                  ),
                  div(class = "panel-section",
                      panel_head("02", "快速测量指标", "选择你的主要测量指标"),
                      div(class = "keyword-strip",
                          span(class = "keyword-pill", "DALYs - 总负担"),
                          span(class = "keyword-pill", "Deaths - 死亡率"),
                          span(class = "keyword-pill", "Prevalence - 现患病例"),
                          span(class = "keyword-pill", "Incidence - 新发病例"),
                          span(class = "keyword-pill", "YLDs - 伤残寿命"),
                          span(class = "keyword-pill", "YLLs - 早死寿命"))
                  ),
                  div(class = "panel-section",
                      panel_head("03", "官方链接", "数据来源"),
                      div(class = "source-links",
                          tags$a("GBD Results Tool", href = gbd_results_tool_url(), target = "_blank"),
                          tags$a("GHDx 门户", href = gbd_ghdx_url(), target = "_blank"),
                          tags$a("IHME API 文档", href = gbd_api_docs_url(), target = "_blank"),
                          tags$a("IHME 主页", href = "https://www.healthdata.org/", target = "_blank"))
                  ),
                  div(class = "panel-section panel-section-action",
                      actionButton("go_download_design", "前往数据下载", class = "btn-generate")
                  )
              ),
              tags$section(class = "center-panel",
                  div(class = "overview-panel",
                      panel_head("总览", "研究设计控制台", "在下载前确认你的研究问题适合使用 GBD。"),
                      div(class = "summary-strip",
                          div(class = "summary-card", span("关卡 1"), strong("疾病"), tags$small("是否存在 GBD 中")),
                          div(class = "summary-card", span("关卡 2"), strong("测量指标"), tags$small("DALYs/Deaths/Prev")),
                          div(class = "summary-card", span("关卡 3"), strong("年份"), tags$small("1990 至最新")),
                          div(class = "summary-card", span("关卡 4"), strong("边界"), tags$small("负担，非因果关系"))
                      )
                  ),
                  div(class = "analysis-card",
                      div(class = "analysis-head", h3("GBD 最适合做什么"), span("研究范围")),
                      div(class = "source-grid",
                          div(class = "source-card recommended",
                              span(class = "source-tag", "最适合"),
                              h4("疾病负担与趋势"),
                              p("死亡/DALYs/患病率的长期变化、国家比较、SDI 梯度。")),
                          div(class = "source-card",
                              span(class = "source-tag", "谨慎"),
                              h4("风险归因与亚组"),
                              p("归因负担、年龄/性别亚组。需明确标注生态学边界。")),
                          div(class = "source-card",
                              span(class = "source-tag", "避免"),
                              h4("治疗/个体风险"),
                              p("药物疗效、手术结局、临床预测模型。应使用临床队列数据。"))
                      )
                  ),
                  div(class = "analysis-card",
                      div(class = "analysis-head", h3("入门模板"), span("经过验证的研究设计")),
                      div(class = "source-grid",
                          div(class = "source-card",
                              span(class = "source-tag", "趋势类"),
                              h4("慢性病趋势"),
                              p("CKD / 糖尿病 / COPD。DALYs 或 Prevalence，Rate，Age-standardized。")),
                          div(class = "source-card",
                              span(class = "source-tag", "死亡类"),
                              h4("死亡负担"),
                              p("IHD / 卒中。Deaths，Age-standardized Rate，多国比较。")),
                          div(class = "source-card",
                              span(class = "source-tag", "差异类"),
                              h4("地区差异"),
                              p("中国 vs 全球。多地区，1990-最新，Rate 比较。"))
                      )
                  ),
                  div(class = "analysis-card",
                      div(class = "analysis-head", h3("常见误区"), span("注意避免")),
                      div(class = "status-msg warning",
                          "只下载单一年份；跨国比较用 Number 而非 Rate；删掉 lower/upper 列；把生态趋势当成个体因果关系；遗漏 GBD 版本号。")
                  )
              )
          )
        ),

        # ==================== STEP 02: Download ====================
        tabPanel(
          title = "数据下载",
          value = "download",
          div(class = "workbench-grid",
              tags$aside(class = "input-panel",
                  div(class = "panel-section",
                      panel_head("01", "主题配置", "生成下载规格说明"),
                      selectInput("download_topic", "分析路径",
                          choices = stats::setNames(gbd_topic_catalog()$topic, gbd_topic_catalog()$title),
                          selected = "diabetes_prevalence", selectize = FALSE),
                      div(class = "control-note",
                          "推荐入门：糖尿病患病率。字段清晰，趋势稳定，适合演示。")
                  ),
                  div(class = "panel-section",
                      panel_head("02", "资源", "模板与手册"),
                      resource_button("download_official_manifest", "下载清单 CSV"),
                      resource_button("download_example_csv", "演示数据 CSV"),
                      resource_button("download_variable_template", "变量记录 CSV"),
                      resource_button("download_beginner_manual", "入门手册 TXT")
                  ),
                  div(class = "panel-section panel-section-action",
                      actionButton("go_upload_download", "前往上传清洗", class = "btn-generate"),
                      tags$a("打开 GBD Results Tool", href = gbd_results_tool_url(),
                             target = "_blank", class = "download-link")
                  )
              ),
              tags$section(class = "center-panel",
                  div(class = "overview-panel",
                      panel_head("总览", "数据下载控制台", "将研究问题翻译为 GBD 字段；从 IHME 导出 CSV。"),
                      uiOutput("download_plan_ui")
                  ),
                  div(class = "analysis-card",
                      div(class = "analysis-head", h3("如何使用 IHME 工具"), span("分步指南")),
                      annotated_ihme_guide()
                  ),
                  div(class = "analysis-card",
                      div(class = "analysis-head", h3("官方字段参考"), span("GBD 字段目录")),
                      uiOutput("official_cards_ui")
                  ),
                  tags$details(class = "detail-panel", open = TRUE,
                      tags$summary("下载步骤与最佳实践"),
                      div(class = "detail-body",
                          div(class = "section-title", "下载后请保存"),
                          div(class = "route-steps",
                              div(class = "route-step active", span(class = "route-num", "1"), div(strong("定义"), p("疾病 / 风险因素"))),
                              div(class = "route-step", span(class = "route-num", "2"), div(strong("选择"), p("测量指标 / 度量"))),
                              div(class = "route-step", span(class = "route-num", "3"), div(strong("地区"), p("全球 + 目标国家"))),
                              div(class = "route-step", span(class = "route-num", "4"), div(strong("年份"), p("1990 至最新")))
                          ),
                          div(class = "section-title section-title-spaced", "官方下载步骤"),
                          div(class = "table-scroll", tableOutput("official_steps_table"))
                      )
                  )
              )
          )
        ),

        # ==================== STEP 03: Upload & Clean ====================
        tabPanel(
          title = "上传清洗",
          value = "upload",
          div(class = "workbench-grid",
              tags$aside(class = "input-panel",
                  div(class = "panel-section",
                      panel_head("01", "数据来源", "演示数据或上传文件"),
                      radioButtons("data_source", NULL,
                          choices = c("批量上传 CSV/ZIP" = "upload", "演示数据" = "demo"),
                          selected = "upload"),
                      conditionalPanel("input.data_source == 'upload'",
                          fileInput("user_file", "上传 GBD CSV/TSV/TXT/XLSX/TIF/GZ 或 ZIP",
                              multiple = TRUE,
                              accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls", ".tif", ".tiff", ".gz", ".zip"),
                              buttonLabel = "浏览", placeholder = "可一次选择多个文件，或上传一个 ZIP")),
                      conditionalPanel("input.data_source == 'url'",
                          textInput("gbd_url", "CSV URL", placeholder = "https://.../gbd_export.csv"),
                          actionButton("fetch_url", "获取 URL", class = "btn-generate")),
                      actionButton("cancel_upload", "取消上传", class = "download-btn cancel-btn"),
                      uiOutput("upload_status_ui")
                  ),
                  div(class = "panel-section",
                      panel_head("02", "分析范围", "可选：指定筛选条件"),
                      selectizeInput("measure", "测量指标", choices = NULL, multiple = FALSE),
                      selectizeInput("cause", "病因/风险因素", choices = NULL, multiple = FALSE),
                      selectizeInput("metric", "度量", choices = NULL, multiple = FALSE),
                      selectizeInput("age", "年龄", choices = NULL, multiple = FALSE),
                      selectizeInput("sex", "性别", choices = NULL, multiple = FALSE),
                      selectizeInput("locations", "地区", choices = NULL, multiple = TRUE),
                      selectizeInput("focus_location", "重点地区", choices = NULL, multiple = FALSE),
                      sliderInput("year_range", "年份范围", min = 1990, max = 2023, value = c(1990, 2023), sep = ""),
                      numericInput("forecast_year", "预测至", value = 2030, min = 2024, max = 2050, step = 1)
                  ),
                  div(class = "panel-section panel-section-action",
                      actionButton("run_analysis", "重新分析", class = "btn-generate")
                  )
              ),
              tags$section(class = "center-panel",
                  div(class = "overview-panel",
                      panel_head("总览", "上传与自动清洗", "上传 GBD CSV，系统自动识别字段、筛选、清洗、计算 EAPC 并生成结果。"),
                      uiOutput("data_error_ui"),
                      div(class = "metric-grid",
                          metric_card("自动识别", "measure/year/val", "标准字段"),
                          metric_card("筛选", "按分析范围", "Measure/Cause/Metric"),
                          metric_card("清洗", "去除缺失值", "year 和 val 必须"),
                          metric_card("建模", "EAPC", "log(rate) ~ year")
                      )
                  ),
                  div(class = "analysis-card",
                      div(class = "analysis-head", h3("文件识别"), span("自动映射字段")),
                      uiOutput("auto_mapping_ui"),
                      uiOutput("file_inventory_ui")
                  ),
                  div(class = "analysis-card",
                      div(class = "analysis-head", h3("数据质量总览"), span("数据指标")),
                      uiOutput("quality_metrics")
                  ),
                  uiOutput("cleaning_report_ui"),
                  tags$details(class = "detail-panel",
                      tags$summary("字段摘要 / 数据预览 / R 代码"),
                      div(class = "detail-body",
                          div(class = "section-title", "字段摘要"),
                          div(class = "table-scroll", tableOutput("field_summary_table")),
                          div(class = "section-title section-title-spaced", "数据预览（前 12 行）"),
                          div(class = "table-scroll", tableOutput("data_preview")),
                          div(class = "section-title section-title-spaced", "可复现 R 代码"),
                          tags$pre(style = "background:#0f172a;color:#e5eef8;padding:14px;border-radius:8px;font-size:12px;white-space:pre-wrap;max-height:400px;overflow:auto;",
                              make_download_code("diabetes_prevalence"))
                      )
                  )
              )
          )
        ),

        # ==================== STEP 04: Analysis ====================
        tabPanel(
          title = "图表分析",
          value = "analysis",
          div(class = "workbench-grid",
              tags$aside(class = "input-panel",
                  div(class = "panel-section",
                      panel_head("01", "结果结构", "表格、图形、报告"),
                      div(class = "keyword-strip",
                          span(class = "keyword-pill", "Table 1"),
                          span(class = "keyword-pill", "EAPC 模型"),
                          span(class = "keyword-pill", "趋势图"),
                          span(class = "keyword-pill", "热力图"),
                          span(class = "keyword-pill", "SDI 梯度"),
                          span(class = "keyword-pill", "预测")),
                      div(class = "status-msg info", "每个图表下方均有下载按钮。所有图表为出版级 PNG（360 dpi）。")
                  ),
                  div(class = "panel-section",
                      panel_head("02", "图表说明", "各产出的使用方法"),
                      div(class = "table-scroll", tableOutput("figure_explainer_table"))
                  ),
                  div(class = "panel-section",
                      panel_head("03", "下载", "导出所有结果"),
                      resource_button("download_clean_data", "清洗后数据 CSV"),
                      resource_button("download_clean_report", "清洗报告 TXT"),
                      resource_button("download_analysis_report", "分析报告 TXT")
                  ),
                  div(class = "panel-section panel-section-action",
                      actionButton("go_write_analysis", "前往写作导出", class = "btn-generate")
                  )
              ),
              tags$section(class = "center-panel",
                  uiOutput("analysis_workspace_ui")
              )
          )
        ),

        # ==================== STEP 05: Writing ====================
        tabPanel(
          title = "写作导出",
          value = "writing",
          div(class = "workbench-grid",
              tags$aside(class = "input-panel",
                  div(class = "panel-section",
                      panel_head("01", "写作材料", "论文框架"),
                      resource_button("download_template", "论文模板 TXT"),
                      resource_button("download_checklist", "投稿清单 CSV"),
                      resource_button("download_analysis_code", "可复现 R 代码"),
                      resource_button("download_resources", "资源目录 CSV")
                  ),
                  div(class = "panel-section",
                      panel_head("02", "投稿提醒", "投稿前请检查"),
                      div(class = "status-msg warning",
                          "自动生成的内容为结构化初稿。投稿前请核实：变量定义、GBD 版本、缺失编码、不确定区间、图注和局限性。")
                  ),
                  div(class = "panel-section",
                      panel_head("03", "讨论框架", "段落结构"),
                      div(class = "table-scroll", tableOutput("discussion_table"))
                  )
              ),
              tags$section(class = "center-panel",
                  div(class = "overview-panel",
                      panel_head("总览", "写作与投稿材料", "以自动生成的内容为框架，投稿前对照代码簿和研究方案审查。"),
                      div(class = "result-dock",
                          div(class = "result-intro",
                              div(div(class = "section-title", "写作流水线"), p(class = "muted-line", "标题 -> 摘要 -> 方法 -> 结果 -> 讨论 -> 清单")),
                              span(class = "result-order", "T -> A -> M -> R -> D -> C")
                          )
                      )
                  ),
                  div(class = "analysis-card",
                      div(class = "analysis-head", h3("标题与摘要"), span("模板")),
                      uiOutput("abstract_template_ui")
                  ),
                  div(class = "analysis-card",
                      div(class = "analysis-head", h3("方法"), span("GBD + EAPC 方法学")),
                      uiOutput("methods_template_ui")
                  ),
                  div(class = "analysis-card",
                      div(class = "analysis-head", h3("结果"), span("趋势 + 地区差异")),
                      uiOutput("results_template_ui")
                  ),
                  div(class = "analysis-card",
                      div(class = "analysis-head", h3("局限性"), span("需人工审查")),
                      uiOutput("limitations_template_ui")
                  ),
                  div(class = "analysis-card",
                      div(class = "analysis-head", h3("投稿清单"), span("投稿前审查")),
                      div(class = "table-scroll", tableOutput("submission_checklist_table"))
                  ),
                  div(class = "analysis-card",
                      div(class = "analysis-head", h3("资源目录"), span("所有可下载资源")),
                      div(class = "table-scroll", tableOutput("resource_catalog_table"))
                  )
              )
          )
        )
      )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  raw_data <- reactiveVal(make_example_gbd())
  raw_label <- reactiveVal("内置演示数据")
  upload_status <- reactiveVal(list(state = "success", message = "已加载演示数据。可直接查看结果，或批量上传 GBD CSV/XLSX/ZIP/TIF 文件。"))
  upload_canceled <- reactiveVal(FALSE)
  analysis_error <- reactiveVal(NULL)

  # ---- Static tables ----
  output$official_steps_table <- compact_table(gbd_official_download_steps())
  output$figure_explainer_table <- compact_table(gbd_figure_explainer())
  output$discussion_table <- compact_table(discussion_scaffold())
  output$submission_checklist_table <- compact_table(manuscript_checklist())
  output$resource_catalog_table <- compact_table(gbd_download_resource_catalog())

  # ---- Tab navigation ----
  observeEvent(input$go_download_design, {
    updateTabsetPanel(session, "main_tab", selected = "download")
  }, ignoreInit = TRUE)
  observeEvent(input$go_upload_download, {
    updateTabsetPanel(session, "main_tab", selected = "upload")
  }, ignoreInit = TRUE)
  observeEvent(input$hero_start, {
    updateTabsetPanel(session, "main_tab", selected = "download")
  }, ignoreInit = TRUE)
  observeEvent(input$hero_demo, {
    updateTabsetPanel(session, "main_tab", selected = "analysis")
  }, ignoreInit = TRUE)
  observeEvent(input$hero_path, {
    route <- gbd_route_cards()
    showModal(modalDialog(
      title = "GBD 数据库工作流",
      div(class = "route-steps",
          lapply(seq_len(nrow(route)), function(i) {
            div(class = "route-step active",
                span(class = "route-num", route$step[[i]]),
                div(strong(route$title[[i]]), p(route$text[[i]])))
          })
      ),
      easyClose = TRUE, footer = modalButton("关闭"), size = "l"
    ))
  }, ignoreInit = TRUE)
  observeEvent(input$go_write_analysis, {
    updateTabsetPanel(session, "main_tab", selected = "writing")
  }, ignoreInit = TRUE)

  # ---- Update scope inputs ----
  update_scope_inputs <- function(data) {
    combo <- default_filter_combo(data)
    cause_vals <- sort(unique(data$cause))
    updateSelectizeInput(session, "measure", choices = available_choices(data, "measure", add_all = FALSE), selected = combo$measure, server = TRUE)
    updateSelectizeInput(session, "cause", choices = available_choices(data, "cause", add_all = length(cause_vals) > 1), selected = combo$cause, server = TRUE)
    updateSelectizeInput(session, "metric", choices = available_choices(data, "metric", add_all = FALSE), selected = combo$metric, server = TRUE)
    updateSelectizeInput(session, "age", choices = available_choices(data, "age", add_all = FALSE), selected = combo$age, server = TRUE)
    updateSelectizeInput(session, "sex", choices = available_choices(data, "sex", add_all = FALSE), selected = combo$sex, server = TRUE)
    slice <- gbd_filter_data(data, measure = combo$measure, cause = combo$cause, metric = combo$metric, age = combo$age, sex = combo$sex)
    locs <- sort(unique(slice$location))
    default_locs <- unique(c("Global", "China", "United States of America", "India", "Japan"))
    default_locs <- default_locs[default_locs %in% locs]
    if (length(default_locs) == 0) default_locs <- head(locs, 6)
    updateSelectizeInput(session, "locations", choices = locs, selected = default_locs, server = TRUE)
    focus <- if ("Global" %in% default_locs) "Global" else default_locs[[1]]
    updateSelectizeInput(session, "focus_location", choices = locs, selected = focus, server = TRUE)
    updateSliderInput(session, "year_range", min = min(data$year), max = max(data$year), value = c(min(data$year), max(data$year)))
    updateNumericInput(session, "forecast_year", value = max(data$year) + 7, min = max(data$year) + 1, max = max(data$year) + 30)
  }

  observeEvent(raw_data(), {
    update_scope_inputs(raw_data())
  }, ignoreInit = FALSE)

  observeEvent(list(input$measure, input$cause, input$metric, input$age, input$sex), {
    data <- raw_data()
    req(data)
    slice <- try(gbd_filter_data(data, measure = input$measure, cause = input$cause, metric = input$metric, age = input$age, sex = input$sex), silent = TRUE)
    if (inherits(slice, "try-error") || nrow(slice) == 0) return(invisible(NULL))
    locs <- sort(unique(slice$location))
    current <- input$locations %||% character(0)
    selected <- current[current %in% locs]
    if (length(selected) == 0) {
      preferred <- unique(c("Global", "China", "United States of America", "India", "Japan"))
      selected <- preferred[preferred %in% locs]
      if (length(selected) == 0) selected <- head(locs, 6)
    }
    focus <- input$focus_location %||% if ("Global" %in% selected) "Global" else selected[[1]]
    if (!focus %in% selected) focus <- if ("Global" %in% selected) "Global" else selected[[1]]
    updateSelectizeInput(session, "locations", choices = locs, selected = selected, server = TRUE)
    updateSelectizeInput(session, "focus_location", choices = locs, selected = focus, server = TRUE)
  }, ignoreInit = TRUE)

  # ---- Data source switching ----
  observeEvent(input$data_source, {
    if (identical(input$data_source, "demo")) {
      data <- make_example_gbd()
      raw_data(data)
      raw_label("内置演示数据")
      upload_canceled(FALSE)
      analysis_error(NULL)
      upload_status(list(state = "success", message = paste0("已加载演示数据：", nrow(data), " 行。分析已自动生成。")))
    }
  }, ignoreInit = TRUE)

  # ---- File upload ----
  observeEvent(input$user_file, {
    if (is.null(input$user_file)) return(invisible(NULL))
    tryCatch({
      data <- read_gbd_files(input$user_file$datapath, input$user_file$name)
      skipped <- length(attr(data, "gbd_import_errors") %||% character(0))
      raw_data(data)
      source_n <- length(unique(data$source_file))
      raw_label(if (source_n == 1) unique(data$source_file)[[1]] else paste0(source_n, " 个文件"))
      upload_canceled(FALSE)
      analysis_error(NULL)
      skip_msg <- if (skipped > 0) paste0("；已跳过 ", skipped, " 个非结果/不可分析文件") else ""
      upload_status(list(state = "success", message = paste0("已上传：", source_n, " 个数据文件；", format(nrow(data), big.mark = ","), " 行；", length(unique(data$cause)), " 个病因/风险因素", skip_msg, "。已自动分析。")))
      showNotification("批量上传成功，分析完成", type = "message", duration = 3)
    }, error = function(err) {
      upload_status(list(state = "error", message = paste0("上传失败：", conditionMessage(err))))
      analysis_error(conditionMessage(err))
      showNotification("上传失败", type = "error", duration = 4)
    })
  }, ignoreInit = TRUE)

  # ---- URL fetch ----
  observeEvent(input$fetch_url, {
    req(nzchar(input$gbd_url))
    tryCatch({
      data <- read_gbd_url(input$gbd_url)
      raw_data(data)
      raw_label("CSV 链接")
      upload_canceled(FALSE)
      analysis_error(NULL)
      upload_status(list(state = "success", message = paste0("URL 获取成功：", nrow(data), " 行。")))
      showNotification("URL 数据已加载", type = "message", duration = 3)
    }, error = function(err) {
      upload_status(list(state = "error", message = paste0("获取失败：", conditionMessage(err))))
      analysis_error(conditionMessage(err))
    })
  }, ignoreInit = TRUE)

  # ---- Cancel upload ----
  observeEvent(input$cancel_upload, {
    upload_canceled(TRUE)
    upload_status(list(state = "cancel", message = "已取消上传。请选择演示数据或上传新的 CSV/XLSX/ZIP/TIF。"))
    session$sendCustomMessage("clearFileInput", "user_file")
    showNotification("已取消上传", type = "message", duration = 2)
  }, ignoreInit = TRUE)

  # ---- Scope values ----
  scope_values <- reactive({
    data <- raw_data()
    combo <- default_filter_combo(data)
    measure <- input$measure %||% combo$measure
    cause <- input$cause %||% combo$cause
    metric <- input$metric %||% combo$metric
    age <- input$age %||% combo$age
    sex <- input$sex %||% combo$sex
    slice <- gbd_filter_data(data, measure = measure, cause = cause, metric = metric, age = age, sex = sex)
    valid_locs <- sort(unique(slice$location))
    if (length(valid_locs) == 0) valid_locs <- sort(unique(data$location))
    selected_locs <- input$locations %||% head(valid_locs, 6)
    selected_locs <- selected_locs[selected_locs %in% valid_locs]
    if (length(selected_locs) == 0) {
      preferred_locs <- unique(c("Global", "China", "United States of America", "India", "Japan"))
      selected_locs <- preferred_locs[preferred_locs %in% valid_locs]
      if (length(selected_locs) == 0) selected_locs <- head(valid_locs, 6)
    }
    focus <- input$focus_location %||% if ("Global" %in% valid_locs) "Global" else valid_locs[[1]]
    if (!focus %in% selected_locs) focus <- if ("Global" %in% selected_locs) "Global" else selected_locs[[1]]
    list(
      measure = measure,
      cause = cause,
      metric = metric,
      age = age,
      sex = sex,
      locations = selected_locs,
      focus_location = focus,
      year_range = input$year_range %||% c(min(data$year), max(data$year)),
      forecast_year = input$forecast_year %||% (max(data$year) + 7)
    )
  })

  # ---- Analysis result ----
  result <- reactive({
    req(!isTRUE(upload_canceled()))
    data <- raw_data()
    s <- scope_values()
    tryCatch({
      analysis_error(NULL)
      analyze_gbd(
        data,
        measure = s$measure, cause = s$cause, metric = s$metric,
        age = s$age, sex = s$sex, locations = s$locations,
        start_year = s$year_range[[1]], end_year = s$year_range[[2]],
        focus_location = s$focus_location, forecast_year = s$forecast_year
      )
    }, error = function(err) {
      analysis_error(conditionMessage(err))
      NULL
    })
  })

  observeEvent(input$run_analysis, {
    r <- result()
    if (is.null(r)) {
      showNotification("分析失败，请检查筛选条件。", type = "error", duration = 3)
    } else {
      upload_status(list(state = "success", message = paste0("已重新生成：", r$meta$cause, "；", r$meta$n_locations, " 个地区。")))
      showNotification("分析已重新生成", type = "message", duration = 3)
    }
  }, ignoreInit = TRUE)

  # ---- Download plan UI ----
  output$download_plan_ui <- renderUI({
    gbd_download_plan_panel(input$download_topic %||% "diabetes_prevalence")
  })
  output$official_cards_ui <- renderUI({
    gbd_official_cards(input$download_topic %||% "diabetes_prevalence")
  })

  # ---- Upload status ----
  output$data_error_ui <- renderUI({
    err <- analysis_error()
    if (is.null(err)) return(NULL)
    div(class = "error-box", paste("错误：", err))
  })

  output$upload_status_ui <- renderUI({
    st <- upload_status()
    div(class = paste("upload-status", st$state), st$message)
  })

  output$auto_mapping_ui <- renderUI({
    if (isTRUE(upload_canceled())) return(div(class = "upload-status idle", "未上传文件"))
    data <- raw_data()
    r <- result()
    tagList(
      div(class = "upload-status success",
          paste0("已识别：", nrow(data), " 行；年份 ", min(data$year), "-", max(data$year),
                 "；", length(unique(data$location)), " 个地区。")),
      div(class = "status-msg info",
          if (is.null(r)) "请调整筛选条件。" else paste0("当前：", r$meta$measure, " / ", r$meta$cause, " / ", r$meta$metric, " / ", r$meta$age, " / ", r$meta$sex))
    )
  })

  output$file_inventory_ui <- renderUI({
    data <- raw_data()
    fields <- gbd_field_summary(data)
    files <- gbd_source_file_summary(data)
    source_block <- NULL
    if (nrow(files) > 1 || !identical(files$source_file[[1]], "Not specified")) {
      source_block <- tagList(
        div(class = "section-title", "批量文件清单"),
        tags$table(class = "table shiny-table table-striped table-hover",
            tags$thead(tags$tr(tags$th("文件"), tags$th("行数"), tags$th("病因/风险因素"), tags$th("度量"), tags$th("年龄"), tags$th("年份"))),
            tags$tbody(lapply(seq_len(nrow(files)), function(i) {
              tags$tr(
                tags$td(files$source_file[[i]]),
                tags$td(format(files$rows[[i]], big.mark = ",")),
                tags$td(files$causes[[i]]),
                tags$td(files$metrics[[i]]),
                tags$td(files$ages[[i]]),
                tags$td(files$years[[i]])
              )
            }))
        ),
        div(class = "section-title section-title-spaced", "字段摘要")
      )
    }
    tagList(
      source_block,
      tags$table(class = "table shiny-table table-striped table-hover",
          tags$thead(tags$tr(tags$th("字段"), tags$th("数量"), tags$th("示例"))),
          tags$tbody(lapply(seq_len(nrow(fields)), function(i) {
            tags$tr(tags$td(fields$field[[i]]), tags$td(fields$n[[i]]), tags$td(fields$examples[[i]]))
          }))
      )
    )
  })

  output$field_summary_table <- compact_table(gbd_field_summary(raw_data()))
  output$data_preview <- compact_table(head(result()$selected, 12))

  # ---- Quality metrics ----
  output$quality_metrics <- renderUI({
    r <- result()
    if (is.null(r)) return(empty_state("等待数据", "上传 GBD CSV 以显示数据质量指标。"))
    div(class = "metric-grid",
        metric_card("原始记录", format(nrow(r$raw), big.mark = ","), raw_label()),
        metric_card("分析记录", format(nrow(r$selected), big.mark = ","), paste0(r$meta$measure, " / ", r$meta$metric)),
        metric_card("地区数", r$meta$n_locations, r$meta$year_range),
        metric_card("重点地区", r$meta$focus_location, paste0(r$meta$start_year, "-", r$meta$end_year))
    )
  })

  # ---- Cleaning report ----
  output$cleaning_report_ui <- renderUI({
    r <- result()
    if (is.null(r)) return(NULL)
    div(class = "report-panel",
        h4("清洗报告"),
        div(class = "report-grid",
            div(class = "report-pill", div(class = "k", "原始"), div(class = "v", format(nrow(r$raw), big.mark = ","))),
            div(class = "report-pill", div(class = "k", "已筛选"), div(class = "v", format(nrow(r$selected), big.mark = ","))),
            div(class = "report-pill", div(class = "k", "地区"), div(class = "v", r$meta$n_locations)),
            div(class = "report-pill", div(class = "k", "年份"), div(class = "v", paste0(r$meta$start_year, "-", r$meta$end_year)))
        ),
        div(class = "download-row",
            downloadButton("download_clean_data_step3", "清洗后数据 CSV", class = "download-btn"),
            downloadButton("download_clean_report_step3", "清洗报告 TXT", class = "download-btn"),
            actionButton("go_results", "查看图表与报告", class = "btn-generate")
        )
    )
  })

  observeEvent(input$go_results, {
    updateTabsetPanel(session, "main_tab", selected = "analysis")
  }, ignoreInit = TRUE)

  # ---- Analysis workspace ----
  output$analysis_workspace_ui <- renderUI({
    r <- result()
    if (is.null(r))
      return(empty_state("暂无结果", "请在「上传清洗」标签页上传 GBD CSV，系统将自动清洗并生成结果。"))
    div(class = "analysis-workspace",
        div(class = "download-row compact-result-downloads",
            downloadButton("download_clean_data", "清洗后数据 CSV", class = "download-btn"),
            downloadButton("download_clean_report", "清洗报告 TXT", class = "download-btn"),
            downloadButton("download_analysis_report", "分析报告 TXT", class = "download-btn")
        ),
        tabsetPanel(id = "analysis_tab", type = "pills",

            tabPanel("总览",
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("数据质量"), span("记录、地区、年份、范围")),
                    uiOutput("quality_metrics")
                ),
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("自动诊断"), span("数据类型、缺失维度与推荐策略")),
                    div(class = "table-scroll", tableOutput("diagnostic_table")),
                    br(), downloadButton("download_diagnostic", "下载诊断表 CSV", class = "download-btn")
                ),
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("处理流程"), span("图 1")),
                    div(class = "plot-wrap plot-wrap-flow", plotOutput("flow_plot", height = "620px")),
                    br(), div(class = "table-scroll", tableOutput("flow_table_analysis")),
                    br(), downloadButton("download_flow", "下载 图 1 PNG", class = "download-btn")
                )
            ),

            tabPanel("Table 1",
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("研究总览表"), span("用于论文正文或补充材料")),
                    div(class = "table-scroll", tableOutput("table1")),
                    br(), downloadButton("download_table1", "下载 Table 1 CSV", class = "download-btn")
                )
            ),

            tabPanel("模型",
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("EAPC 趋势模型"), span("log(value) ~ year")),
                    div(class = "table-scroll", tableOutput("model_table")),
                    br(), downloadButton("download_model", "下载 EAPC CSV", class = "download-btn")
                ),
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("起止变化"), span("基线 vs 最新")),
                    div(class = "plot-wrap", plotOutput("rank_plot", height = "620px")),
                    br(), downloadButton("download_rank_plot", "下载变化图 PNG", class = "download-btn")
                )
            ),

            tabPanel("图形",
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("综合情报看板"), span("总览、排名、构成和解释信号")),
                    div(class = "plot-wrap plot-wrap-flow", plotOutput("storyboard_plot", height = "720px")),
                    br(), downloadButton("download_storyboard_plot", "下载综合看板 PNG", class = "download-btn")
                ),
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("趋势图"), span("重点及高负担地区")),
                    div(class = "plot-wrap", plotOutput("trend_plot", height = "620px")),
                    br(), downloadButton("download_trend_plot", "下载趋势图 PNG", class = "download-btn")
                ),
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("趋势热力图"), span("各地区以基线=100 为索引")),
                    div(class = "plot-wrap", plotOutput("heatmap_plot", height = "620px")),
                    br(), downloadButton("download_heatmap_plot", "下载热力图 PNG", class = "download-btn")
                ),
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("EAPC 排名"), span("年均百分比变化及 95% CI")),
                    div(class = "plot-wrap", plotOutput("eapc_plot", height = "640px")),
                    br(), downloadButton("download_eapc_plot", "下载 EAPC 图 PNG", class = "download-btn")
                ),
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("不确定区间"), span("最新估计值及 UI")),
                    div(class = "plot-wrap", plotOutput("uncertainty_plot", height = "640px")),
                    br(), downloadButton("download_uncertainty_plot", "下载 UI 图 PNG", class = "download-btn")
                ),
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("不确定性扇形"), span("焦点地区 UI 随时间变化")),
                    div(class = "plot-wrap", plotOutput("uncertainty_fan_plot", height = "620px")),
                    br(), downloadButton("download_uncertainty_fan_plot", "下载扇形图 PNG", class = "download-btn")
                ),
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("构成贡献"), span("病因/年龄/文件来源贡献")),
                    div(class = "plot-wrap", plotOutput("contribution_plot", height = "640px")),
                    br(), downloadButton("download_contribution_plot", "下载构成图 PNG", class = "download-btn")
                ),
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("构成流图"), span("结构随时间变化")),
                    div(class = "plot-wrap", plotOutput("share_stream_plot", height = "640px")),
                    br(), downloadButton("download_share_stream_plot", "下载构成流图 PNG", class = "download-btn")
                ),
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("年龄谱"), span("最新年份年龄分层")),
                    div(class = "plot-wrap", plotOutput("age_pattern_plot", height = "640px")),
                    br(), downloadButton("download_age_pattern_plot", "下载年龄谱 PNG", class = "download-btn")
                ),
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("SDI / 地区梯度"), span("缺 SDI 时自动替代")),
                    div(class = "plot-wrap", plotOutput("equity_plot", height = "620px")),
                    br(), downloadButton("download_equity_plot", "下载梯度图 PNG", class = "download-btn")
                ),
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("负担-趋势象限图"), span("高负担 + 上升 = 优先关注")),
                    div(class = "plot-wrap", plotOutput("quadrant_plot", height = "620px")),
                    br(), downloadButton("download_quadrant_plot", "下载象限图 PNG", class = "download-btn")
                ),
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("变化瀑布图"), span("起止年份变化贡献")),
                    div(class = "plot-wrap", plotOutput("waterfall_plot", height = "640px")),
                    br(), downloadButton("download_waterfall_plot", "下载瀑布图 PNG", class = "download-btn")
                ),
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("排名迁移图"), span("高负担地区排序变化")),
                    div(class = "plot-wrap", plotOutput("bump_rank_plot", height = "640px")),
                    br(), downloadButton("download_bump_rank_plot", "下载排名迁移 PNG", class = "download-btn")
                ),
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("地区小多图"), span("多地区轨迹矩阵")),
                    div(class = "plot-wrap", plotOutput("small_multiples_plot", height = "760px")),
                    br(), downloadButton("download_small_multiples_plot", "下载小多图 PNG", class = "download-btn")
                ),
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("地区分布演变"), span("离散度与异常高值")),
                    div(class = "plot-wrap", plotOutput("distribution_plot", height = "620px")),
                    br(), downloadButton("download_distribution_plot", "下载分布图 PNG", class = "download-btn")
                ),
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("近期预测"), span("探索性趋势外推")),
                    div(class = "plot-wrap", plotOutput("forecast_plot", height = "560px")),
                    br(), downloadButton("download_forecast_plot", "下载预测图 PNG", class = "download-btn")
                )
            ),

            tabPanel("扩展",
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("优先级表"), span("高负担、上升趋势与解释动作")),
                    div(class = "table-scroll", tableOutput("priority_table")),
                    br(), downloadButton("download_priority", "下载优先级 CSV", class = "download-btn")
                ),
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("构成贡献表"), span("谁贡献了最新负担")),
                    div(class = "table-scroll", tableOutput("contribution_table")),
                    br(), downloadButton("download_contribution", "下载构成贡献 CSV", class = "download-btn")
                ),
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("年龄分层表"), span("最新年份年龄谱")),
                    div(class = "table-scroll", tableOutput("age_pattern_table")),
                    br(), downloadButton("download_age_pattern", "下载年龄谱 CSV", class = "download-btn")
                ),
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("敏感性分析"), span("用于稳健性声明")),
                    div(class = "table-scroll", tableOutput("sensitivity_table")),
                    br(), downloadButton("download_sensitivity", "下载敏感性分析 CSV", class = "download-btn")
                ),
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("地区排名"), span("按最新负担排序")),
                    div(class = "table-scroll", tableOutput("rank_table")),
                    br(), downloadButton("download_rank_table", "下载排名 CSV", class = "download-btn")
                )
            ),

            tabPanel("报告",
                div(class = "analysis-section",
                    div(class = "analysis-section-head", h4("结果解读"), span("改写为 Results 部分")),
                    uiOutput("interpretation_ui"),
                    br(), downloadButton("download_interpretation", "下载解读 TXT", class = "download-btn")
                )
            )
        )
    )
  })

  # ---- Analysis table outputs ----
  output$flow_table_analysis <- compact_table(gbd_flow_table(result()))
  output$table1 <- compact_table(gbd_table1(result()))
  output$model_table <- compact_table(gbd_model_table(result()))
  output$diagnostic_table <- compact_table(result()$diagnostic_table)
  output$priority_table <- compact_table(result()$priority_table)
  output$contribution_table <- compact_table(result()$contribution_table)
  output$age_pattern_table <- compact_table(result()$age_pattern_table)
  output$sensitivity_table <- compact_table(gbd_sensitivity_table(result()))
  output$rank_table <- compact_table(result()$rank_table)

  # ---- Plot outputs ----
  output$flow_plot <- renderPlot(draw_gbd_flow_plot(result()), res = 110, execOnResize = TRUE)
  output$storyboard_plot <- renderPlot(draw_storyboard_plot(result(), preview = TRUE), res = 110, execOnResize = TRUE)
  output$trend_plot <- renderPlot(draw_trend_plot(result(), preview = TRUE), res = 96, execOnResize = TRUE)
  output$rank_plot <- renderPlot(draw_rank_plot(result(), preview = TRUE), res = 96, execOnResize = TRUE)
  output$heatmap_plot <- renderPlot(draw_heatmap_plot(result(), preview = TRUE), res = 96, execOnResize = TRUE)
  output$eapc_plot <- renderPlot(draw_eapc_plot(result(), preview = TRUE), res = 96, execOnResize = TRUE)
  output$uncertainty_plot <- renderPlot(draw_uncertainty_plot(result(), preview = TRUE), res = 96, execOnResize = TRUE)
  output$uncertainty_fan_plot <- renderPlot(draw_uncertainty_fan_plot(result(), preview = TRUE), res = 96, execOnResize = TRUE)
  output$contribution_plot <- renderPlot(draw_contribution_plot(result(), preview = TRUE), res = 96, execOnResize = TRUE)
  output$share_stream_plot <- renderPlot(draw_share_stream_plot(result(), preview = TRUE), res = 96, execOnResize = TRUE)
  output$age_pattern_plot <- renderPlot(draw_age_pattern_plot(result(), preview = TRUE), res = 96, execOnResize = TRUE)
  output$equity_plot <- renderPlot(draw_equity_plot(result(), preview = TRUE), res = 96, execOnResize = TRUE)
  output$quadrant_plot <- renderPlot(draw_quadrant_plot(result(), preview = TRUE), res = 96, execOnResize = TRUE)
  output$waterfall_plot <- renderPlot(draw_waterfall_plot(result(), preview = TRUE), res = 96, execOnResize = TRUE)
  output$bump_rank_plot <- renderPlot(draw_bump_rank_plot(result(), preview = TRUE), res = 96, execOnResize = TRUE)
  output$small_multiples_plot <- renderPlot(draw_small_multiples_plot(result(), preview = TRUE), res = 96, execOnResize = TRUE)
  output$distribution_plot <- renderPlot(draw_distribution_plot(result(), preview = TRUE), res = 96, execOnResize = TRUE)
  output$forecast_plot <- renderPlot(draw_forecast_plot(result(), preview = TRUE), res = 96, execOnResize = TRUE)

  # ---- Interpretation ----
  output$interpretation_ui <- renderUI({
    div(class = "template-box", paste(gbd_interpretation_lines(result()), collapse = "\n\n"))
  })

  # ---- Writing templates ----
  output$abstract_template_ui <- renderUI({
    paper <- result()$paper
    tagList(
      template_card("标题模板", "模板", paper$title),
      template_card("摘要模板", "四段式", paste(paper$abstract, collapse = "\n\n"))
    )
  })
  output$methods_template_ui <- renderUI({
    template_card("方法", "GBD + EAPC", paste(result()$paper$methods, collapse = "\n\n"))
  })
  output$results_template_ui <- renderUI({
    template_card("结果", "趋势 + 地区差异", paste(result()$paper$results, collapse = "\n\n"))
  })
  output$limitations_template_ui <- renderUI({
    template_card("局限性", "需人工审查", paste(result()$paper$limitations, collapse = "\n\n"))
  })

  # ---- Resource downloads ----
  output$download_example_csv <- downloadHandler(
    filename = function() "gbd_clinical_demo.csv",
    content = function(file) write_csv_excel(make_example_gbd(), file)
  )
  output$download_official_manifest <- downloadHandler(
    filename = function() "gbd_official_download_manifest.csv",
    content = function(file) write_csv_excel(gbd_download_manifest(input$download_topic %||% "diabetes_prevalence"), file)
  )
  output$download_variable_template <- downloadHandler(
    filename = function() "gbd_variable_record_template.csv",
    content = function(file) write_csv_excel(gbd_variable_template(), file)
  )
  output$download_beginner_manual <- downloadHandler(
    filename = function() "gbd_beginner_workflow_manual.txt",
    content = function(file) writeLines(beginner_manual_lines(input$download_topic %||% "diabetes_prevalence"), file, useBytes = TRUE)
  )
  output$download_clean_data <- downloadHandler(
    filename = function() "gbd_selected_clean.csv",
    content = function(file) write_csv_excel(result()$selected, file)
  )
  output$download_clean_data_step3 <- downloadHandler(
    filename = function() "gbd_selected_clean.csv",
    content = function(file) write_csv_excel(result()$selected, file)
  )
  output$download_clean_report <- downloadHandler(
    filename = function() "gbd_cleaning_report.txt",
    content = function(file) writeLines(gbd_cleaning_report_lines(result()), file, useBytes = TRUE)
  )
  output$download_clean_report_step3 <- downloadHandler(
    filename = function() "gbd_cleaning_report.txt",
    content = function(file) writeLines(gbd_cleaning_report_lines(result()), file, useBytes = TRUE)
  )
  output$download_analysis_report <- downloadHandler(
    filename = function() "gbd_analysis_report.txt",
    content = function(file) writeLines(analysis_report_lines(result()), file, useBytes = TRUE)
  )
  output$download_table1 <- downloadHandler(
    filename = function() "gbd_table1_overview.csv",
    content = function(file) write_csv_excel(gbd_table1(result()), file)
  )
  output$download_model <- downloadHandler(
    filename = function() "gbd_eapc_model_table.csv",
    content = function(file) write_csv_excel(gbd_model_table(result()), file)
  )
  output$download_diagnostic <- downloadHandler(
    filename = function() "gbd_auto_diagnostic_table.csv",
    content = function(file) write_csv_excel(result()$diagnostic_table, file)
  )
  output$download_priority <- downloadHandler(
    filename = function() "gbd_priority_table.csv",
    content = function(file) write_csv_excel(result()$priority_table, file)
  )
  output$download_contribution <- downloadHandler(
    filename = function() "gbd_contribution_table.csv",
    content = function(file) write_csv_excel(result()$contribution_table, file)
  )
  output$download_age_pattern <- downloadHandler(
    filename = function() "gbd_age_pattern_table.csv",
    content = function(file) write_csv_excel(result()$age_pattern_table, file)
  )
  output$download_sensitivity <- downloadHandler(
    filename = function() "gbd_sensitivity_table.csv",
    content = function(file) write_csv_excel(gbd_sensitivity_table(result()), file)
  )
  output$download_rank_table <- downloadHandler(
    filename = function() "gbd_rank_table.csv",
    content = function(file) write_csv_excel(result()$rank_table, file)
  )
  output$download_interpretation <- downloadHandler(
    filename = function() "gbd_result_interpretation.txt",
    content = function(file) writeLines(gbd_interpretation_lines(result()), file, useBytes = TRUE)
  )
  output$download_analysis_code <- downloadHandler(
    filename = function() "gbd_reproducible_analysis.R",
    content = function(file) writeLines(make_download_code(input$download_topic %||% "diabetes_prevalence"), file, useBytes = TRUE)
  )
  output$download_template <- downloadHandler(
    filename = function() "gbd_manuscript_template.txt",
    content = function(file) writeLines(manuscript_text_lines(result()), file, useBytes = TRUE)
  )
  output$download_checklist <- downloadHandler(
    filename = function() "gbd_submission_checklist.csv",
    content = function(file) write_csv_excel(manuscript_checklist(), file)
  )
  output$download_resources <- downloadHandler(
    filename = function() "gbd_resource_catalog.csv",
    content = function(file) write_csv_excel(gbd_download_resource_catalog(), file)
  )

  # ---- Figure downloads ----
  make_fig_handler <- function(draw_fn, w = 3600, h = 2400, ...) {
    downloadHandler(
      filename = function() paste0("gbd_figure_", format(Sys.time(), "%H%M%S"), ".png"),
      content = function(file) {
        open_plot_device(file, width = w, height = h, res = 360)
        draw_fn(result(), ...)
        dev.off()
      }
    )
  }

  output$download_flow <- make_fig_handler(draw_gbd_flow_plot, 3600, 2500)
  output$download_storyboard_plot <- make_fig_handler(draw_storyboard_plot, 4200, 2800)
  output$download_trend_plot <- make_fig_handler(draw_trend_plot, 3900, 2500)
  output$download_rank_plot <- make_fig_handler(draw_rank_plot, 3800, 2700)
  output$download_heatmap_plot <- make_fig_handler(draw_heatmap_plot, 3900, 2600)
  output$download_eapc_plot <- make_fig_handler(draw_eapc_plot, 3800, 2800)
  output$download_uncertainty_plot <- make_fig_handler(draw_uncertainty_plot, 3900, 2800)
  output$download_uncertainty_fan_plot <- make_fig_handler(draw_uncertainty_fan_plot, 3900, 2600)
  output$download_contribution_plot <- make_fig_handler(draw_contribution_plot, 3900, 2800)
  output$download_share_stream_plot <- make_fig_handler(draw_share_stream_plot, 3900, 2800)
  output$download_age_pattern_plot <- make_fig_handler(draw_age_pattern_plot, 3900, 2800)
  output$download_equity_plot <- make_fig_handler(draw_equity_plot, 3700, 2600)
  output$download_quadrant_plot <- make_fig_handler(draw_quadrant_plot, 3700, 2600)
  output$download_waterfall_plot <- make_fig_handler(draw_waterfall_plot, 3900, 2800)
  output$download_bump_rank_plot <- make_fig_handler(draw_bump_rank_plot, 3900, 2800)
  output$download_small_multiples_plot <- make_fig_handler(draw_small_multiples_plot, 4200, 3200)
  output$download_distribution_plot <- make_fig_handler(draw_distribution_plot, 3900, 2600)
  output$download_forecast_plot <- make_fig_handler(draw_forecast_plot, 3700, 2400)
}

shinyApp(ui, server)
