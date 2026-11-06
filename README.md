# Melbourne Visitor Explorer

**Explore Melbourne's CBD through attractions, pedestrian activity, public transport and personalised sightseeing routes.**

[简体中文](README.zh-CN.md) · [Download the complete project](https://github.com/yudexsun-tech/melbourne-visitor-explorer/releases/latest) · [Source code](app/)

Built with R Shiny, Mapbox, Leaflet and Tableau Public. It combines spatial data and historical pedestrian observations to help visitors compare places, understand when nearby streets tend to be busy and plan a day around their preferences.

## Features

| Feature | What it does |
| --- | --- |
| Interactive city map | Explore 13 attractions around Melbourne's Free Tram Zone with a custom Mapbox style, lighting and weather effects. |
| Pedestrian activity | Compare historical profiles by month, weather, weekday/holiday and hour, or animate a full day. |
| Street heatmap | View activity based on 2024–2025 pedestrian observations, adjusted for estimated footway width and local street geometry. |
| Public transport | Inspect nearby tram, bus and rail stops, select routes and directions, and use a 300, 500 or 1,000 metre radius. |
| Attraction details | Explore opening hours, admission and booking information, mapped entrances, nearby facilities and hourly/weekly charts. |
| Visitor reviews | View an embedded Tableau Public dashboard filtered to the selected attraction. |
| Smart routes | Generate a 4–6-stop itinerary using weather preferences, quiet/lively atmosphere, comfort facilities, opening status, a starting location and the selected hour. |

The route planner uses explicit scoring rules and a distance penalty to choose and order stops. See [the implementation](app/R/route_planner.R).

## Download and run

**Download `melbourne-visitor-explorer-v1.0.0.zip` from [Releases](https://github.com/yudexsun-tech/melbourne-visitor-explorer/releases/latest).** This is the complete runnable distribution, including all datasets, photos, the original Mapbox configuration and the cache. The expanded project is approximately 741 MB.

The repository contains the source and smaller reference files. Large files are listed in [RELEASE_FILES.json](RELEASE_FILES.json) and supplied in the complete Release ZIP. GitHub's **Code → Download ZIP** and automatic “Source code” release archives do not include those large files.

Fully extract the release, install R (and optionally RStudio), then install these packages in an R console:

```r
install.packages(c(
  "shiny", "leaflet", "htmltools", "jsonlite",
  "later", "htmlwidgets", "mapgl"
))
```

From the extracted project root:

```r
shiny::runApp("app", launch.browser = TRUE)
```

Alternatively, open `app/app.R` in RStudio and select **Run App → Run External**. Use an external browser for the embedded Tableau charts. For command-line use:

```sh
Rscript app/run_app.R
```

Then open **http://127.0.0.1:3840**. Mapbox and Tableau require internet access. The first run can take longer while historical profiles are calculated; `app/cache/project.rds` is a rebuildable cache.

### Mapbox configuration

**The existing public Mapbox token is preserved unchanged in `app/.Renviron`.** The app reads it automatically. Keep this file when moving or deploying the project. The main map also uses the custom style in `app/R/mapbox_main.R`; both the token and that style must remain available.

## Data at a glance

| Supplied dataset | Records |
| --- | ---: |
| Attractions | 13 |
| Transport stop records | 231 |
| Transport route records | 116 |
| Facility records | 542 |
| Pedestrian sensor metadata, 2024 | 63 |
| Pedestrian sensor metadata, 2025 | 68 |
| Monthly review chart rows | 1,326 |

These are file-level counts. Map views apply geographic and display filters; transport records can include platforms and station-related records.

Attractions include State Library Victoria, NGV Australia, ACMI, Immigration Museum, St Paul's Cathedral, Queen Victoria Market, Royal Arcade, The Block Arcade, Bourke Street Mall, Chinatown, Flinders Street Station's exterior, Federation Square and Hosier Lane.

## Methods and interpretation

Pedestrian profiles pool eligible **2024 and 2025** observations by month, weather class, day type and hour. “Holiday” groups weekends and Victorian public holidays. Missing observations remain missing; valid zero counts are retained.

Attraction activity is inferred from nearby street sensors. Quiet/Moderate/Busy thresholds use fixed, attraction-specific terciles of known-open profile means normalised by estimated effective area. Street heat and transport-stop indicators use their separate footway-width rules. Historical street passages do not measure simultaneous venue occupancy, queues or onboard passenger load.

Opening information, transport snapshots and reviews have their own dates and provenance. Route ordering uses preference scores and a straight-line distance penalty, rather than a street-network navigation service.

## Project structure

| Path | Purpose |
| --- | --- |
| `app/app.R`, `app/run_app.R` | Shiny entry point and command-line launcher |
| `app/.Renviron` | Preserved public Mapbox token |
| `app/R/` | Mapping, crowd analysis, details, transport and route planning |
| `app/www/` | JavaScript, CSS, icons and attraction photos |
| `app/data/` | Runtime tables and local geometry |
| `app/metadata/`, `app/docs/` | Methods, provenance and evidence |
| `data_archive/` | Archived source, processed and reference datasets |
| `review_data/` | Review source tables and monthly Tableau input |
| `guides/` | Data catalogue and file-origin records |

## Sources and credits

Sources include City of Melbourne pedestrian and spatial data, Victorian transport and holiday data, weather records, OpenStreetMap-derived features, venue information and the supplied review collection. See [the data catalogue](guides/DATA_CATALOG.csv), [file origins](guides/FILE_ORIGINS.csv), `app/data/sources.csv` and `data_archive/metadata/sources.csv`. [Photo attribution](app/www/route-photos/CREDITS.html) is retained.

External datasets, photos, map services and visualisations retain their original ownership and applicable terms; no blanket licence is granted over third-party materials.
