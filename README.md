# Climate-driven forest change and comprehensive wealth

Replication code and inputs for the forest-area, vegetation-carbon, and noncarbon forest ecosystem-service results.

## Requirements

- R 4.3 or later
- R packages: `data.table`, `ggplot2`, `openxlsx`, `patchwork`, `scales`, `sf`

Install the packages once:

```r
install.packages(c("data.table", "ggplot2", "openxlsx", "patchwork", "scales", "sf"))
```

## Run

From the repository root:

```bash
Rscript run_all.R
```

The pipeline writes intermediate caches to `output/cache`, manuscript figures to `output/figures`, result tables to `output/tables`, and validation results to `output/validation_summary.csv`.

## Inputs

- `data/input/impact_regions.gpkg`: Climate Impact Lab impact regions, spatial allocation shares, 2022 forest area, and 2020 CWON marginal values.
- `data/input/coefficients/`: regional forest-area and vegetation-carbon coefficients for JULES-ES, LPJmL, and MC2.
- `data/input/fair_gmst_2022_2100.csv`: annual FaIR temperature trajectories.
- `data/input/ssp_country_socioeconomic_annual.rds`: annual country GDP and population trajectories.
- `data/input/service_elasticities.csv`: ecosystem-service income and population-density elasticities.

The monetary results include recreation, nonwood forest products, and water services. Habitat and carbon valuation are excluded from the reported aggregate.

## Author

Bernardo A. Bastien-Olvera  
Instituto de Ciencias de la Atmósfera y Cambio Climático, Universidad Nacional Autónoma de México  
bbastien@atmosfera.unam.mx

## Funding

Secretaría de Ciencia, Humanidades, Tecnología e Innovación, Mexico. Ciencia Básica y de Frontera 2025 project CBF-2025-I-345.
