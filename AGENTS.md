# AGENTS.md

## Project Agents.md Guide for OpenAI Codex

Welcome! This **AGENTS.md** file provides comprehensive guidance for OpenAI Codex and other AI agents working in this repository. Follow these rules to ensure high code quality, fast onboarding, and consistent output.

---
  
  ## Project Structure
  
  Key folders and files for Codex and developers:
  
  - `/scripts`: All R scripts for ETL (extract, transform, load), QA, and data exports.
- `etl_main.R`: Orchestrates the complete ETL pipeline.
- `functions_database.R`: Helper functions for database operations.
- `00_install_packages.R`: Manages package dependencies.
- `01_request_api_legiscan.R`: API request logic.
- `02a_raw_parse_legiscan.R` – `04z_app_load.R`: Core ETL steps, each modular.
- `qa_checks.R`: Data quality assurance scripts.
- `/data-raw`: Raw input data (JSONs, CSVs) as received from LegiScan and other sources.
- `/data-app`: Cleaned, processed data exports for downstream apps and developers.
- `/qa`: Data QA output (CSV logs and checks).
- `/docs`: Project documentation, data dictionaries, and design documents.
- `/tests`: (Optional) Test scripts or notebooks for validating pipeline logic.

---
  
  ## Coding Conventions for OpenAI Codex
  
  ### General
  
  - Use **R** for all ETL and data pipeline code.
- Name variables and functions descriptively (`snake_case` preferred).
- Modularize: keep scripts small, single-purpose, and easy to test.
- Always add inline comments for complex logic and at the top of each script with a summary.
- All config (API keys, DB passwords) must be loaded from environment variables or `config.yaml`. Never hardcode secrets.

### Database

- Primary key and data integrity rules are enforced at each stage. Use helper functions in `functions_database.R`.
- Follow schema prefixes (`t_`, `hist_`, `p_`, `qry_`, `app_`, `user_`, etc.) as documented in `/docs/db_architecture.md`.

### Data Exports

- All tables must be exported both to the Postgres database and as CSVs to `/data-app/`.
- For every new table or export, generate or update the corresponding data dictionary in `/docs/`.

---
  
  ## Testing and Quality Assurance
  
  - Data quality checks are performed using `qa_checks.R`.
- Before merging code or pushing new data, always run QA scripts and review output in `/qa/`.
- When Codex generates new code, include at least one test or QA function to check expected row counts, key uniqueness, and NA values.
- If adding a new ETL step, update `etl_main.R` and add an integration test or describe manual QA steps in a comment block.

**Sample QA command:**
  ```r
source("qa_checks.R")```

# Review /qa/ output for anomalies

## Pull Request (PR) Guidelines

When Codex or a developer creates a PR:
  
  1. **Description:** Clearly summarize the change, referencing related issues if any.
2. **Testing:** State how you tested the code (including any new or updated QA scripts).
3. **Scope:** Limit PRs to a single feature, fix, or refactor for easy review.
4. **Documentation:** Update this AGENTS.md and other docs as needed for new conventions or processes.
5. **Checklist:**
  - [ ] All scripts run without error
- [ ] QA scripts produce clean output
- [ ] Data exports are up-to-date
- [ ] No secrets or credentials in code or logs

---
  
  ## How to Run & Validate
  
  **Install dependencies:**  
  ```r
source("00_install_packages.R")```

**Run the pipeline:**
  ```r
source("etl_main.R")```

**Validate with QA:**
  ```r
source("qa_checks.R")```

**Check outputs:**  
  - `/data-app/` for exports  
- `/qa/` for logs and QA results  

---
  
  ## Additional Guidance for AI Agents
  
  - When unsure, default to the safest and most documented choice (e.g., check if a function already exists before creating a new one).
- Favor readability, maintainability, and transparency.
- All changes should be reproducible and compatible with both Docker and local execution.
- If generating a new ETL step or QA script, ensure it is discoverable by referencing it in `etl_main.R` and updating AGENTS.md.
- If the prompt requests multiple changes, propose an order of execution and ask for confirmation if ambiguous.

---
  
  ## Resources
  
  - `dev_workplan.md`: Project vision and major tasks
- `db_architecture.md`: Database layers, naming, and primary key conventions
- `app_dev_guide.md`: For app and data visualization developers

---
  
  *This AGENTS.md is a living document. Please update as workflows, conventions, or technologies evolve.*
  
  