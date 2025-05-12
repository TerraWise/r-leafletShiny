# 🌱 Planfarm TerraWise Pick Your Boundary :shipit: :shipit: :shipit:

This Shiny application allows users to interactively select and submit farm boundaries for the Planfarm TerraWise project. Users can view agricultural property polygons on a map, select properties by shire (LGA), highlight and manage selections, and submit their chosen boundaries directly to Planfarm TerraWise.

---

## Features

:world_map: **Interactive Map**: View and search for properties using a Leaflet map with Esri World Imagery basemap.
:microscope: **LGA Filtering**: Filter properties by selecting one or more shires (LGAs).
:pushpin: **Polygon Selection**: Click polygons to highlight them, then add to your selection.
:hammer_and_wrench: **Selection Management**: Add, remove, or clear selected properties.
:bar_chart: **Data Table**: View selected properties in a table.
[x] **Submission**: Submit selected boundaries as GeoJSON to TerraWise (uploaded to AWS S3).
:notebook_with_decorative_cover: **Instructions**: Downloadable PDF instructions available from the app interface.

---

## Getting Started

### Prerequisites

- R (>= 4.0)
- [R packages](#r-packages)
- AWS S3 credentials (for uploading boundaries)
- WA State government defined landholder title shapefile data in `Data/CPES.zip`

### R Packages

The app uses the following R packages:

- shiny
- leaflet
- sf
- dplyr
- bslib
- DT
- leaflet.extras
- geojsonsf
- aws.s3

You can install them with:

```r
install.packages(c(
  "shiny", "leaflet", "sf", "dplyr", "bslib", "DT",
  "leaflet.extras", "geojsonsf", "aws.s3"
))

