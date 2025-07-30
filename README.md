# Arizona Legislation ETL

This repository houses the scripts and data pipeline that retrieve and transform Arizona legislative data from [LegiScan](https://legiscan.com/). The processed data feeds downstream applications such as the [az-legislation-app](https://github.com/apantazi/az-legislation-app) and the live dashboard at [Legislative Compass](http://andrewpantazi.com/interactives/legislative-compass).

## Quick start
1. Install required R packages
   ```r
   source("scripts/00_install_packages.R")
   ```
2. Run the ETL pipeline
   ```r
   source("scripts/etl_main.R")
   ```
   This downloads new LegiScan datasets (if available), parses them, reads manual CSVs, loads data into Postgres, and exports CSVs to `data-app/`.
   The console will ask questions for you to answer like whether you want to Dockerize your data, what password you want to set, what elections you want to analyze and what metrics you want to use.
   
4. Review QA output
   ```r
   source("scripts/qa_checks.R")
   ```
   Check `qa/qa_checks.log` and the CSVs in `qa/` for potential issues.

## Data overview
The pipeline pulls legislative data using the LegiScan API and merges it with additional demographic and manually curated data. Key sources include:

| Source | Files | Description |
|-------|-------|-------------|
| LegiScan API | JSON datasets per session | Bills, legislators, and roll call votes. Each session folder contains `bill`, `people`, and `vote` subdirectories. |
| Dave's Redistricting | `t_daves_districts.csv` | District demographics and election results |
| Google Sheets | user_legislator_events, user_bill_categories | Manually maintained sheets for special events and bill categories |

The ETL produces three database layers (raw, processed, app) and exports selected tables as CSVs to `data-app/`.

## LegiScan data format
LegiScan publishes JSON archives that follow a consistent structure. The README included in each dataset explains that archives unpack to:
```
STATE/
STATE/SESSION/
STATE/SESSION/bill/
STATE/SESSION/people/
STATE/SESSION/vote/
```
Individual files are named after their IDs or bill numbers (e.g., `HB1.json` for a bill, `1357.json` for a legislator, `135791.json` for a roll call). These JSONs mirror the objects returned by the LegiScan API hooks.

## Manual CSVs and Google Sheets
Besides LegiScan, the pipeline ingests demographic and contextual information:
* `data-raw/daves/t_daves_districts.csv` – district-level demographics and election results.
* `user_legislator_events` – Google Sheet tracking notable legislator events.
* `user_bill_categories` – Google Sheet with bill category tags.

These datasets are parsed in `scripts/02b_raw_read_csvs.R` before being merged with LegiScan data.

## ETL scripts
`etl_main.R` orchestrates the workflow and calls individual modules in sequence:

| Script | Purpose |
|-------|---------|
| `functions_database.R` | Connect to Postgres and write tables |
| `01_request_api_legiscan.R` | Download new LegiScan datasets |
| `02a_raw_parse_legiscan.R` | Parse JSON to tables |
| `02b_raw_read_csvs.R` | Read manual CSVs and Google Sheets |
| `02z_raw_load.R` | Load raw data to Postgres |
| `03a_process.R` | Clean and calculate processed tables |
| `03z_process_load.R` | Write processed data to Postgres |
| `04a_app_settings.R` | Apply app settings |
| `04b_app_prep.R` | Prepare data for apps |
| `04c_app_bill_lookup.R` | Build bill lookup table |
| `qa_checks.R` | Quality assurance checks |
| `04z_app_load.R` | Export app-ready tables |

For details on each step, see [docs/etl.md](docs/etl.md).

## Database architecture
The database layers and naming conventions are documented in [docs/db_architecture.md](docs/db_architecture.md). Tables in the **raw** schema mirror LegiScan JSONs and manual CSVs. The **processed** schema cleans and integrates this data, while the **app** schema organizes it for web applications.

## Outputs
Processed CSVs are written to `data-app/`, for example:
* `app01_vote_patterns.csv` – voting pattern metrics
* `app03_district_context.csv` – district demographics and partisan lean
* `qry_leg_votes.csv` – full legislator vote records

These files can be used directly or loaded into Postgres for further analysis.

## Example application
The companion repository [az-legislation-app](https://github.com/reliablerascal/az-legislation-app) demonstrates how this data feeds a Shiny dashboard. A live example is hosted at [Legislative Compass](http://andrewpantazi.com/interactives/legislative-compass).

---
For more information, explore the `docs/` directory which includes a detailed workplan, data dictionaries, and design diagrams.
