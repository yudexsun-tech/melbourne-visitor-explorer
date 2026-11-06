# Melbourne Visitor Explorer｜墨尔本城市探索助手

**将景点、人流、公共交通和个性化路线整合到同一张互动地图。**

[English](README.md) · [下载完整项目](https://github.com/yudexsun-tech/melbourne-visitor-explorer/releases/latest) · [查看源码](app/)

使用 R Shiny、Mapbox、Leaflet 和 Tableau Public 构建。应用以墨尔本中央商务区及免费电车区域周边为研究范围，将空间数据与历史行人观测结合，帮助使用者比较景点、了解周边街道人流规律，并根据出行偏好安排游览路线。

## 主要功能

| 功能 | 内容 |
| --- | --- |
| 城市互动地图 | 展示 13 个景点，结合 Mapbox 自定义样式、时段光照和天气效果。 |
| 历史人流探索 | 按月份、天气、工作日／休息日及小时切换，支持一天内的动态播放。 |
| 街道人流热力图 | 汇总 2024—2025 年行人观测，结合估算步道宽度和本地街道几何展示活动强度。 |
| 公共交通查询 | 查看电车、公交和铁路站点，选择线路与方向，在 300、500、1,000 米范围间切换。 |
| 景点详情 | 查看开放时间、门票与预约信息、入口、周边设施、逐小时图表和星期／小时对照图。 |
| 游客评价 | 嵌入 Tableau Public 图表，随所选景点筛选评论统计。 |
| 个性化路线 | 根据天气偏好、安静／热闹氛围、便利设施、开放状态、起点和出发时间，生成 4—6 个停靠点的游览建议。 |

路线推荐采用明确的评分规则，并结合距离惩罚选择和排列景点，具体逻辑见 [路线推荐模块](app/R/route_planner.R)。

## 下载与运行

**请在 [Releases](https://github.com/yudexsun-tech/melbourne-visitor-explorer/releases/latest) 下载 `melbourne-visitor-explorer-v1.0.0.zip`。** 这是包含全部数据、照片、原有 Mapbox 配置及缓存的完整运行包，解压后约 741 MB。

仓库保留便于浏览的源码与较小参考文件；大文件清单见 [RELEASE_FILES.json](RELEASE_FILES.json)，文件本体随 Release 完整包提供。GitHub 的 **Code → Download ZIP** 及自动生成的 “Source code” 压缩包不包含这些大文件。

完整解压后安装 R，也可以使用 RStudio 操作。在 R 控制台安装依赖：

```r
install.packages(c(
  "shiny", "leaflet", "htmltools", "jsonlite",
  "later", "htmlwidgets", "mapgl"
))
```

将工作目录设为解压后的项目根目录，运行：

```r
shiny::runApp("app", launch.browser = TRUE)
```

也可以在 RStudio 打开 `app/app.R`，选择 **Run App → Run External**，使用外部浏览器查看 Tableau 图表。命令行启动方式为：

```sh
Rscript app/run_app.R
```

随后打开 **http://127.0.0.1:3840**。Mapbox 与 Tableau 需要联网；首次读取并计算历史数据可能较慢，`app/cache/project.rds` 是可重新生成的缓存。

### Mapbox 配置

**原有 Mapbox 公开令牌完整保留在 `app/.Renviron` 中**，应用会自动读取。移动或部署时请保留该文件。主地图还使用 `app/R/mapbox_main.R` 指定的自定义样式，令牌和样式都需继续有效。

## 数据概况

| 数据文件 | 记录数 |
| --- | ---: |
| 景点主表 | 13 |
| 公共交通站点记录 | 231 |
| 公共交通线路记录 | 116 |
| 公共设施记录 | 542 |
| 2024 年行人传感器元数据 | 63 |
| 2025 年行人传感器元数据 | 68 |
| 月度评论图表数据行 | 1,326 |

以上为文件记录数；实际显示会受研究范围、筛选和站台合并等规则影响，不能直接等同于独立车站数量。

13 个景点包括维多利亚州立图书馆、NGV 澳大利亚馆、ACMI、移民博物馆、圣保罗大教堂、维多利亚女王市场、皇家拱廊、布洛克拱廊、伯克街购物中心、唐人街、弗林德斯街车站外观、联邦广场和霍西尔巷。

## 方法与数据含义

人流概况合并 **2024 年与 2025 年**的有效观测，按月份、天气类别、日期类型和小时匹配。“Holiday” 包括周末和维多利亚州公共假日。缺失观测保持缺失，有效零值参与计算。

景点人流来自附近街道传感器。安静／适中／繁忙等级依据已知开放时段的均值，经估算有效面积标准化后，按景点分别设置固定三分位阈值。街道热力图和交通站点提示使用各自的步道宽度规则。这些历史街道活动指标不能直接换算为景点同时在场人数、排队人数或车厢载客量。

开放时间、交通快照和评论数据各有日期，来源及假设保留在元数据中。路线排序结合偏好评分与直线距离惩罚，并非基于道路网络的实时导航。

## 项目结构

| 路径 | 用途 |
| --- | --- |
| `app/app.R`、`app/run_app.R` | Shiny 入口和命令行启动脚本 |
| `app/.Renviron` | 保留的 Mapbox 公开令牌 |
| `app/R/` | 地图、人流、详情、交通及路线推荐模块 |
| `app/www/` | JavaScript、CSS、图标和照片 |
| `app/data/` | 运行数据与本地空间几何 |
| `app/metadata/`、`app/docs/` | 方法、来源及佐证材料 |
| `data_archive/` | 原始、处理后及参考数据归档 |
| `review_data/` | 评论原始表和 Tableau 月度输入 |
| `guides/` | 数据目录和文件来源记录 |

## 数据来源与素材署名

项目使用墨尔本市行人与空间数据、维多利亚州交通及公共假日数据、天气记录、OpenStreetMap 衍生要素、场馆信息及随附评论数据。详见 [数据目录](guides/DATA_CATALOG.csv)、[文件来源](guides/FILE_ORIGINS.csv)、`app/data/sources.csv` 与 `data_archive/metadata/sources.csv`。[照片署名](app/www/route-photos/CREDITS.html) 保留在项目中。

外部数据、照片、地图服务和图表保留各自权属与使用条件，仓库不对第三方素材统一授予额外许可。

## 贡献者

- **Codex（OpenAI）**：协助项目检查、文档整理与 GitHub 发布。

## 许可证

项目自行编写的应用源代码和文档采用 [MIT 许可证](LICENSE)。第三方数据、照片、软件依赖及外部服务继续遵循各自的许可或条款，详见[第三方授权说明](THIRD_PARTY_NOTICES.md)。
