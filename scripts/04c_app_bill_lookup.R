#################################
#                               #
# 04C_APP_BILL_LOOKUP.R         #
#                               #
#################################
# Build a dataset summarizing roll-call votes for a bill lookup tab.
# This merges bill metadata with roll call statistics and labels
# votes as Unanimous, Bipartisan, Partisan, or Failed.
# 2025-06-04

library(dplyr)

app04_bill_lookup <- qry_roll_calls %>%
  left_join(qry_bills %>% select(bill_id, bill_desc, state_link), by = "bill_id") %>%
  mutate(
    vote_category = case_when(
      passed == 0 ~ "Failed",
      D_pct_of_present == 1 & R_pct_of_present == 1 ~ "Unanimous",
      D_pct_of_present >= 0.6 & R_pct_of_present >= 0.6 ~ "Bipartisan",
      TRUE ~ "Partisan"
    ),
    roll_call_date = as.Date(roll_call_date)
  ) %>%
  select(
    bill_id, bill_number, bill_title, bill_desc, session_year,
    bill_url, state_link,
    roll_call_id, roll_call_date, roll_call_desc, roll_call_chamber,
    yea, nay, nv, absent, n_total, n_present, passed, vote_category,
    final_vote, D_pct_of_present, R_pct_of_present,
    rc_unity_R, rc_unity_D, rc_mean_partisanship
  ) %>%
  arrange(session_year, bill_number, roll_call_date)
