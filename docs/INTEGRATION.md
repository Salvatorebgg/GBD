# 网站接入说明

本项目是独立 Shiny 工具，入口为 `app.R`。部署方式和隔壁 CDC Wonder、CHARLS 项目保持一致。v3.0 采用 workbench-grid 全宽布局。

## 启动

```powershell
Rscript scripts/run_shiny.R 3840 0.0.0.0
```

反向代理可将内部端口映射到网站路径，例如 `/tools/gbd-workbench/`。

## 嵌入

```html
<iframe
  src="https://your-domain.example/tools/gbd-workbench/"
  title="GBD 临床数据库挖掘绘图写作工作台"
  style="width: 100%; min-height: 1400px; border: 0;"
></iframe>
```

## 运行依赖

```r
install.packages(c("shiny", "htmltools", "jsonlite", "ggplot2", "scales", "ragg"))
```

## 架构说明（v3.0）

```
GBD/
  app.R                    # UI + Server（workbench-grid 布局）
  R/
    data.R                 # 数据读写与字段标准化
    analysis.R             # EAPC 模型、ggplot2 绘图、写作模板
  www/css/
    gbd-theme.css          # 设计令牌、重置、排版
    gbd-layout.css         # 网格系统、响应式断点
    gbd-components.css     # 组件样式
```

CSS 分为三个层级：
- **Theme**：CSS 自定义属性、动画、滚动条、选中样式
- **Layout**：顶部导航栏、command-hero 三栏、workbench-grid 双栏、响应式媒体查询
- **Components**：按钮、卡片、表格、表单、badge、状态框、缩略图

## 部署注意

- 正式部署时建议在反向代理和 Shiny Server 中放开 CSV 上传大小限制
- IHME Results Tool 下载通常需要用户在官方页面登录后导出 CSV；本工具负责读取导出文件、清洗、分析和生成图文产出
- 所有图表通过 ggplot2 服务端渲染，无需客户端 JS 图表库
- 反向代理需支持 WebSocket 以保证 Shiny 正常通信
