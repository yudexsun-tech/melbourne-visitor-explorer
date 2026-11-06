# Third-party notices / 第三方授权说明

## Licence scope

The [MIT License](LICENSE) applies to the original application source code and original documentation in this project. It does not relicense third-party software, datasets, photographs, map content, hosted visualisations or other material owned by their respective rights holders. File-specific and upstream notices continue to apply.

## Photographs

The attraction photographs in `app/www/route-photos/` retain the licences listed for each image in [CREDITS.html](app/www/route-photos/CREDITS.html). That file records the image title, author, source page and licence, and notes that the images were resized and cropped to 640 × 360 pixels.

The recorded licences include [CC BY 2.0](https://creativecommons.org/licenses/by/2.0/), [CC BY 2.5](https://creativecommons.org/licenses/by/2.5/), [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/), [CC BY-SA 3.0](https://creativecommons.org/licenses/by-sa/3.0/), [CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/) and [CC0 1.0](https://creativecommons.org/publicdomain/zero/1.0/). Preserve the applicable attribution, licence links and modification notices. Adapted CC BY-SA photographs remain subject to the applicable ShareAlike terms; the project's MIT licence does not replace those terms.

## Datasets and derived data

Source-specific licence and attribution records are maintained in [app/data/sources.csv](app/data/sources.csv), [data_archive/metadata/sources.csv](data_archive/metadata/sources.csv), [the data catalogue](guides/DATA_CATALOG.csv) and [file-origin records](guides/FILE_ORIGINS.csv).

| Material | Recorded source terms and attribution |
| --- | --- |
| OpenStreetMap-derived features and databases | © OpenStreetMap contributors. The source records specify ODbL 1.0; retain the applicable database licence and attribution. See [OpenStreetMap copyright and licence information](https://www.openstreetmap.org/copyright). |
| City of Melbourne pedestrian and spatial data | Retain the publisher and dataset attribution and the CC BY / portal terms recorded for each source. |
| DTP / PTV transport records from the research archive | The archive is recorded as ODC-By. Retain the archive authors' attribution and applicable DTP / PTV source terms. |
| Open-Meteo weather data | The source records specify CC BY 4.0 and associated source attribution, including Copernicus / ERA5 where recorded. |
| Venue information, visitor reviews and other supplied reference material | Retain the applicable original source terms and provenance; no additional licence is granted over this material by the project's MIT licence. |

Transformed data, combined tables and cached data retain applicable upstream requirements. An MIT licence for the application code does not turn its input data or derived databases into MIT-licensed material.

## Software dependencies and online services

R and the packages used by the application remain governed by their own licences. Direct packages listed in the setup instructions are `shiny`, `leaflet`, `htmltools`, `jsonlite`, `later`, `htmlwidgets` and `mapgl`. Use the licence and notice files supplied with the installed versions, including those of their dependencies and any bundled JavaScript libraries.

Mapbox services, styles and map content, and Tableau Public services and hosted visualisations, remain subject to their respective providers' terms. Including a public Mapbox token in the configuration does not change those terms or grant additional rights to the service.

## 中文说明

项目自行编写的应用源代码和文档采用根目录 [LICENSE](LICENSE) 中的 MIT 许可证。第三方软件、数据、照片、地图内容及外部托管图表不因此改为 MIT 授权。

照片的作者、来源、许可和裁剪缩放说明见 [照片署名](app/www/route-photos/CREDITS.html)，应继续保留；采用 CC BY-SA 的照片改编版本仍需遵循相应的相同方式共享条款。数据的来源、许可与署名以各来源记录为准，OpenStreetMap 衍生数据库等继续保留适用的上游义务。运行时使用的 R 包，以及 Mapbox、Tableau Public 等服务，继续遵循各自的许可或服务条款。
