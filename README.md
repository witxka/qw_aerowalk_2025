# qw_aerowalk_2025

# Aerowalk Duel Tournament Analysis (Playoff Div 1)

R scripts for processing and analyzing QuakeWorld duel match data from the [Aerowalk Duel Tournament](https://www.quakeworld.nu/wiki/Aerowalk_Duel_Tournament/Playoff_Div_1). 

## Files

### `qw_arm.R`
```R
# Converts JSON match data to CSV format
# Input:  Raw JSON match logs
# Output: Formatted CSV files (winners.csv, losers.csv)
```

### `summary.Rmd`
```R
# Generates an analysis report from processed data
# Input:  winners.csv, losers.csv
# Output: HTML/PDF report with key metrics and visualizations
```

### `AERO2025_DIV1.duels.json` 

Initial dataset provided by parsing mvd demos log ouputs with [mvd to JSON tools](https://github.com/witxka/mvdToJSON)

### `winners.csv` 

Subset of the data for winner games.

### `losers.csv` 

Subset of the data for loser games.

### `Aero_Duel_Tournament_2025-v1.pdf` 

Rmarkup analisys output in pdf form

## Usage
1. Run `qw_arm.R` to process JSON match logs:
   ```bash
   Rscript qw_arm.R 
   ```
2. Generate the report with:
   ```bash
   Rscript -e "rmarkdown::render('summary.Rmd')"
   ```

## Data Highlights
- **Source**: QuakeWorld.nu tournament logs
- **Format**: JSON → CSV → Analysis
- **Key Metrics**: Frags, damage, armor control, movement speed

## Dependencies
- R (≥ 4.0)
- Packages:
  ```r
  install.packages(c("jsonlite", "dplyr", "rmarkdown","ggplot2","corrplot","reshape2","gtExtras"))
  ```
