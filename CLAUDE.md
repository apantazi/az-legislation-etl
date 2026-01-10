# CLAUDE.md

## Project Overview

This is an **R-based ETL (Extract-Transform-Load) pipeline** for Arizona legislative data. It retrieves data from the LegiScan API, transforms it through multiple processing layers, and exports it to PostgreSQL and CSV formats for use in web applications.

**Primary outputs:**
- PostgreSQL database with three schemas (raw, processed, app)
- CSV exports in `/data-app/` for downstream applications
- Live dashboard at [Legislative Compass](http://andrewpantazi.com/interactives/legislative-compass)

## Quick Start Commands

```r
# Install dependencies (run once)
source("scripts/00_install_packages.R")

# Run full ETL pipeline (interactive prompts for settings)
source("scripts/etl_main.R")

# Run QA checks separately
source("scripts/qa_checks.R")
```

## Project Structure

```
az-legislation-etl/
├── scripts/           # R scripts executing the ETL pipeline
│   ├── etl_main.R     # Master orchestrator - sources all modules in sequence
│   ├── functions_database.R  # Database helper functions (connection, writing)
│   ├── 00_install_packages.R # Package dependency installer
│   ├── 01_request_api_legiscan.R  # LegiScan API data requests
│   ├── 02a_raw_parse_legiscan.R   # Parse JSONs to tables
│   ├── 02b_raw_read_csvs.R        # Read CSVs and Google Sheets
│   ├── 02z_raw_load.R             # Write raw layer to Postgres
│   ├── 03a_process.R              # Data transformation and calculations
│   ├── 03z_process_load.R         # Write processed layer to Postgres
│   ├── 04a_app_settings.R         # App configuration settings
│   ├── 04b_app_prep.R             # Prepare app-ready datasets
│   ├── 04c_app_bill_lookup.R      # Build bill lookup table
│   ├── 04z_app_load.R             # Export app data to Postgres and CSV
│   └── qa_checks.R                # Quality assurance checks
├── data-raw/          # Raw input data (LegiScan JSONs, CSVs)
├── data-app/          # Processed outputs (CSVs, RDS files)
├── qa/                # QA output and logs
├── docs/              # Project documentation and data dictionaries
└── config.yml         # Configuration (API keys, DB passwords) - gitignored
```

## Naming Conventions

### Data Frame Prefixes

| Prefix | Meaning | Layer | Example |
|--------|---------|-------|---------|
| `t_` | Raw **T**ables | raw | `t_bills`, `t_legislator_votes` |
| `user_` | **User**-entered data | raw | `user_bill_categories` |
| `hist_` | **Hist**orical processed data | proc | `hist_leg_sessions` |
| `p_` | **P**rocessed (latest record) | proc | `p_legislators`, `p_bills` |
| `jct_` | **J**unction tables (many-to-many) | proc | `jct_bill_categories` |
| `calc_` | **Calc**ulated intermediates (not persisted) | temp | `calc_party_loyalty` |
| `qry_` | **Query** (foundational views for apps) | app | `qry_legislators_incumbent` |
| `app_` | **App**lication-specific data | app | `app01_vote_patterns` |
| `qa_` | **QA** queries and anomalies | app | `qa_loyalty_ranks` |

### Database Schemas

| Schema | Purpose |
|--------|---------|
| `raw_legiscan` | Raw LegiScan data parsed from JSON |
| `raw_misc_csvs` | Raw CSV/Sheets data (demographics, user data) |
| `proc` | Processed and cleaned data with calculations |
| `app_shiny` | Application-ready data for visualizations |

### Code Style

- Use **snake_case** for all variables and functions
- Single-purpose, modular scripts (one script = one logical step)
- Clear comment blocks at script headers and section breaks
- Inline comments for complex logic

## Key Patterns

### Script Header Pattern
```r
#################################
#                               #
# SCRIPT_NAME.R                 #
#                               #
#################################
# Brief description of script purpose
# Author, Date

##########################
# 1) SECTION HEADER     #
##########################
```

### Safe Value Extraction
```r
safe_get <- function(x, default = NA) ifelse(is.null(x), default, x)
```

### Database Write Pattern
```r
con <- attempt_connection()
write_tables_in_list(con, schema_name, list_of_tables, primary_keys)
dbDisconnect(con)
```

### Progress Bar for Long Operations
```r
pb <- progress::progress_bar$new(
  format = "  processing [:bar] :percent",
  total = n, clear = FALSE
)
for (i in seq_len(n)) {
  # work here
  pb$tick()
}
```

## Configuration

### Environment Settings
The pipeline prompts interactively for:
- **Environment**: `staging` (port 5433) or `production` (port 5432)
- **Docker**: Whether to use Docker containers for PostgreSQL
- **Parse Years**: Date range for data (default: 2023-2025)
- **Demographic Source**: CVAP, ACS, etc.
- **Election Weights**: Which elections to use for partisan lean

### Required Secrets (in config.yml)
```yaml
default:
  api_key_legiscan: "your-legiscan-api-key"
  postgres_pwd: "your-postgres-password"
```

**Never commit config.yml to version control.**

## Common Tasks

### Adding a New ETL Step
1. Create a new script in `/scripts/` following naming convention (e.g., `03b_new_step.R`)
2. Add `source("03b_new_step.R")` to `etl_main.R` in the correct sequence
3. Update documentation in `/docs/etl.md`

### Adding a New Table
1. Follow naming prefix conventions based on layer (raw/proc/app)
2. Define primary key(s) in the appropriate `*_load.R` script
3. Add to the write list in the corresponding load script
4. Update data dictionary in `/docs/data_dictionaries/`

### Running QA Checks
```r
source("scripts/qa_checks.R")
# Check qa/qa_checks.log for issues
# Review qa/*.csv for specific anomalies
```

### Exporting New Data to CSV
Add to the export list in `04z_app_load.R`:
```r
write.csv(new_dataframe, "../data-app/new_export.csv", row.names = FALSE)
```

## Data Sources

| Source | Access | Files/Location |
|--------|--------|----------------|
| LegiScan API | Requires API key in config.yml | `/data-raw/legiscan/AZ/` |
| Dave's Redistricting | CSV download | `/data-raw/daves/t_daves_districts.csv` |
| Google Sheets | Public (no auth needed) | Read via `googlesheets4::read_sheet()` |

## Testing and Validation

- **QA Script**: `scripts/qa_checks.R` validates row counts, PK uniqueness, missing values
- **QA Output**: Check `/qa/qa_checks.log` and CSV files in `/qa/`
- **Manual Verification**: Run `verify_table(con, schema, table)` after writes

## Dependencies

**Key R packages:**
- `tidyverse` - Data manipulation
- `legiscanrr` - LegiScan API interface (install from GitHub)
- `RPostgres` / `DBI` - Database connectivity
- `jsonlite` - JSON parsing
- `googlesheets4` - Google Sheets access
- `progress` - Progress bars
- `qs` - Fast R object serialization

**Install GitHub packages:**
```r
devtools::install_github("fanghuiz/legiscanrr")
```

## Important Files Reference

| File | Purpose |
|------|---------|
| `docs/db_architecture.md` | Database schema details, table definitions |
| `docs/etl.md` | ETL process documentation |
| `docs/app_dev_guide.md` | Web app development guide |
| `docs/dev_workplan.md` | Project roadmap and tasks |
| `docs/data_dictionaries/*.csv` | Field-level documentation |

## Do's and Don'ts

### Do
- Use existing helper functions from `functions_database.R`
- Follow the three-layer architecture (raw → processed → app)
- Add primary key constraints at processed/app layers
- Run QA checks after significant data changes
- Keep scripts single-purpose and modular
- Document new tables in data dictionaries

### Don't
- Hardcode credentials (use config.yml)
- Skip the processed layer when building app queries
- Create new functions if equivalent exists in `functions_database.R`
- Modify raw layer data (transform in processed layer instead)
- Push without running QA checks

## Troubleshooting

### Database Connection Issues
```r
# Check container status
system("docker ps")

# Manual connection test
con <- attempt_connection()
dbGetQuery(con, "SELECT 1")
```

### Package Installation Issues
```r
# Re-run package installer
source("scripts/00_install_packages.R")

# For archived packages (basicspace)
install.packages("path/to/basicspace_0.24.tar.gz", repos = NULL, type = "source")
```

### Empty Data Frames
The `write_table()` function safely handles empty data frames by skipping the write operation. Check upstream data sources if tables are unexpectedly empty.
