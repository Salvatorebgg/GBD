# GBD 临床数据库一站式工作台 v3.0

面向 IHME Global Burden of Disease 导出数据的专业 Shiny 工作台：研究设计、下载清单、CSV 读取、标准化清洗、EAPC 趋势分析、出版级绘图和论文段落一次完成。

采用与 CDC Wonder Workbench、CHARLS Workbench 一致的全宽 workbench-grid 专业布局。

## 能做什么

- 带小白从 0 开始：确定临床问题、翻译成 GBD 字段、打开官网、下载 CSV、上传、分析、绘图、写作。
- 生成 GBD Results Tool 下载清单：Measure、Cause/Risk、Metric、Age、Sex、Year、Location。
- 读取 IHME/GBD Results Tool 导出的 CSV/TSV，也支持 CSV 直链。
- 自动标准化常见字段：`measure_name`、`location_name`、`cause_name`、`metric_name`、`year`、`val`、`lower`、`upper`。
- 输出趋势表、EAPC、最新负担排序、SDI 梯度、近端预测。
- 生成 8 张以上出版级图表（360 dpi PNG）：趋势图、热力图、EAPC 排名图、不确定区间图、SDI 梯度图、负担-趋势象限图、预测图、处理流程图。
- 自动生成 Methods、Results、Limitations 和投稿核对清单。

## 小白怎么从 0 开始

### 1. 先确定需求

先把想法写成一句话：

```text
我想研究 [疾病/风险因素] 在 [地区/人群] 中 [某个负担指标] 从 [开始年份] 到 [结束年份] 的变化。
```

最稳的第一篇：

```text
研究 Diabetes mellitus 在 Global、China 和 United States of America 中年龄标化患病率从 1990 年到最新年份的变化趋势。
```

对应 GBD 字段：

```text
Measure = Prevalence
Cause/Risk = Diabetes mellitus
Metric = Rate
Age = Age-standardized
Sex = Both
Year = 1990-latest
Location = Global + target countries
```

### 2. 打开 GBD 官网

优先打开：

```text
https://vizhub.healthdata.org/gbd-results/
```

如果页面说明或入口变化，查看：

```text
https://ghdx.healthdata.org/gbd-results-tool
```

### 3. 在官网筛选并下载

在 Results Tool 中按顺序选择：

```text
1. Measure
2. Cause / Risk / Impairment
3. Metric
4. Age
5. Sex
6. Years
7. Locations
8. Search / Apply
9. Download CSV
```

下载后不要手工改列名，不要删除 `lower` 和 `upper`，不要只复制网页表格。保留官网下载的原始 CSV。

### 4. 上传到本项目

运行 Shiny 后，在"上传清洗"标签页上传官网下载文件。系统会自动：

```text
读取字段 -> 标准化列名 -> 按筛选条件取数 -> 清洗 year/val -> 计算 EAPC -> 出图表 -> 生成写作草稿
```

### 5. 看结果和写作

推荐阅读顺序（在"图表分析"标签页）：

```text
总览 -> Table 1 -> 模型 -> 图形 -> 扩展 -> 报告
```

写作时优先使用：

- **趋势表**：latest value、uncertainty interval、percent change、EAPC。
- **趋势图**：主图，展示长期变化。
- **起止变化图**：展示哪些地区变化最大。
- **SDI 梯度图**：讨论社会发展水平相关差异，只做生态描述。
- **写作区**：复制 Methods/Results/Limitations 后人工润色。

## 目录结构

```text
GBD/
  app.R                    # Shiny 入口（v3.0 workbench-grid 布局）
  DESCRIPTION              # 包元数据
  R/
    data.R                 # 数据读写、标准化、字段识别
    analysis.R             # EAPC 建模、ggplot2 绘图、写作模板
  www/
    css/
      gbd-theme.css        # CSS 变量、重置、排版、动画
      gbd-layout.css       # 顶部导航、command-hero、workbench-grid、响应式
      gbd-components.css   # 按钮、卡片、表格、表单、badge、状态框
  scripts/
    run_shiny.R            # Shiny 服务启动脚本
    launch_shiny.ps1       # PowerShell 快捷启动
    run_pipeline.R         # 命令行：不启动 Shiny 直接跑完整流水线
  tests/
    smoke.R                # 集成冒烟测试
  docs/
    INTEGRATION.md         # 部署与嵌入说明
```

## 运行 Shiny

```powershell
Rscript scripts/run_shiny.R 3840 127.0.0.1
```

打开：

```text
http://127.0.0.1:3840
```

## 命令行一键跑完整产出

无参数时使用内置演示数据：

```powershell
Rscript scripts/run_pipeline.R
```

使用自己的 GBD CSV：

```powershell
Rscript scripts/run_pipeline.R path\to\gbd_export.csv outputs
```

输出包括：

```text
outputs/
  gbd_selected_clean.csv
  gbd_trend_eapc_table.csv
  gbd_analysis_report.txt
  gbd_manuscript_draft.txt
  figure_trend.png
  figure_rank.png
  figure_sdi_gradient.png
  figure_forecast.png
```

## 测试

```powershell
Rscript tests/smoke.R
```

## 设计说明

v3.0 界面采用与 CDC Wonder Workbench、CHARLS Workbench 一致的专业 workbench 风格：

- **顶部导航栏**：粘性顶栏，左侧项目名称，右侧 5 步流程指示线（设计 → 下载 → 上传 → 分析 → 写作）
- **Command Hero**：三栏布局（描述文字 → 操作按钮 → 统计卡片）
- **Workbench Grid**：每个标签页均采用侧边栏（输入控件，粘性定位）+ 中央面板（结果内容，全宽展示）
- **配色系统**：Teal/Navy 专业学术色系，通过 CSS 自定义属性统一管理
- **字体层级**：`Noto Sans SC` / `Microsoft YaHei`，清晰的字号层级（10px–22px）
- **响应式**：三级断点（1600px / 1180px / 860px），窄屏自动切换为单列

## 数据来源

官方下载入口优先使用：

- IHME GBD Results Tool：https://vizhub.healthdata.org/gbd-results/
- GHDx GBD Results Tool page：https://ghdx.healthdata.org/gbd-results-tool
- IHME API docs：https://api-docs.ihme.services/

注：Results Tool 的批量导出通常需要登录。这个项目不绕过官方权限；它读取你从官方页面导出的 CSV，并把后续清洗、绘图和写作流程自动化。

## 常见坑

- 不要只下载单一年份，否则无法做 EAPC。
- 跨地区比较优先用 `Age-standardized Rate`，不要直接用 `Number` 当主比较指标。
- `upper` 和 `lower` 是不确定区间，论文图表和结果段落要保留。
- GBD 是模型估计的群体层面数据，不能写成个体因果效应。
- Discussion 需要补充临床机制和已有文献，不能只复制自动生成段落。
