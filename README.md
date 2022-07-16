# Capacity Limits Code Release

---

This repo contains code to perform a difference-to-difference policy analysis of Bay Area capacity limits to accompany the paper ["Science Translation During the COVID-19 Pandemic: An Academic-Public Health Partnership to Assess Capacity Limits in California"](https://ajph.aphapublications.org/doi/10.2105/AJPH.2021.306576). 

## Data

This project relies on the following data:
- SafeGraph Core Places (including metadata and square footage)
- SafeGraph Weekly Patterns (including home panel summaries)
- [NYTimes COVID-19 Data](https://github.com/nytimes/covid-19-data)

## Pipeline

To run this pipeline, you'll need to update the data paths to point to the appropriate data sources throughout and in the variables script. Code should be run in RStudio with the root directory of the code release repo as the working directory.

Scripts and notebooks should be run in the following order to reproduce results:

1. `capacity_paper_bay_area_patterns`: Takes raw SafeGraph data and produces long dataframes of normalized hourly and daily visits to Bay Area places of interest (POI).
Note: This script is incredibly compute heavy and will require significant time and resources._

2. `capacity_paper_preprocessing`: Computes occupancy from normalized visits for a common set of POIs. Cleans and merges case counts and social distancing metrics.

3. `capacity_paper_figures`: Generates figures from preprocessed hourly occupancy data.

4. `capacity_paper_did`: Performs difference-in-difference analysis and variations on preprocessed daily patterns and generates figures.
