#################################
#                               #  
# 03A_TRANSFORM.R               #
#                               #
#################################

##########################################
#                                        #  
# 1a) construct initial processed frames #
#                                        #
##########################################
# hist_leg_sessions, jct_bill_categories, p_bills, pl_legislator_votes, p_legislators, p_roll_calls, p_sessions, 

p_bills <- t_bills %>%
  rename(
    bill_desc = description,
    bill_number = number,
    bill_title = title,
    bill_url = url
  )

p_sessions <- p_bills %>%
  select(session_id,session_name,session, two_year_period) %>%
  distinct() %>%
  mutate(
    session_year = as.numeric(str_extract(session_name, "\\d{4}")),
    session_biennium = paste(
      if_else(session_year %% 2 == 0, session_year - 1, session_year),
      if_else(session_year %% 2 == 0, session_year, session_year + 1),
      sep = "-"
    )
  )

# roll_call_id should remain as an integer- see ls_bill_vote at https://api.legiscan.com/dl/Database_ERD.png
p_roll_calls <- t_roll_calls %>%
  left_join(p_bills %>% select(bill_id, bill_title, bill_number, session_year, bill_url), by = "bill_id") %>%
  rename(
    roll_call_date = date,
    roll_call_desc = desc,
    roll_call_chamber = chamber,
    n_total = total,
  ) %>%
  mutate(
    pct_of_total = yea/n_total,
    n_present = yea+nay,
    pct_of_present = yea/n_present,
    final_vote = ifelse(grepl("third", roll_call_desc, ignore.case = TRUE), "Y", "N")
  ) %>%
  select(-chamber_id)

# remove all non-legislators
hist_leg_sessions <- t_legislator_sessions %>%
  filter (party =='D' | party =='R') %>%
  rename(
    legislator_name = name
  ) %>%
  mutate(
    ballotpedia = paste0("http://ballotpedia.org/",ballotpedia),
    district_number = as.integer(str_extract(district, "\\d+")),
    chamber = case_when(
      role == "Sen" ~ "Senate",
      role == "Rep" ~ "House",
      TRUE ~ role
    ))

# for user-entered info on legislator termination, see https://docs.google.com/spreadsheets/d/1woSZBU5bOfTGFKtuaYg2xT8jCo314RVlSpMrSARWl1c/edit?gid=0#gid=0
calc_leg_terminated <- 
  user_legislator_events %>% 
  filter (event =="terminated") %>%
  left_join(hist_leg_sessions,by = c('chamber','district_number','last_name')) %>%
  mutate (termination_date = date, temp_name = last_name) %>%
  select (people_id, termination_date, temp_name) %>%
  group_by(people_id) %>%
  summarize(
    termination_date = max(termination_date, na.rm = TRUE),  # or min(termination_date) depending on your need
    temp_name = first(temp_name)  # or any other method to select a name
  ) %>%
  ungroup()

p_legislators <- hist_leg_sessions %>%
  left_join(p_sessions %>% select(session, session_id), by = "session") %>%  #so we can arrange by session
  arrange(people_id, desc(session_id)) %>% 
  group_by(people_id) %>%  # Group by the stable people_id
  slice(1) %>%           # Take the first row for each person (now the latest session's record)
  ungroup() %>%
  left_join(calc_leg_terminated, by="people_id") %>%
  select(-role,-role_id,-party_id,-district, -committee_id, -committee_sponsor, -state_federal, -session, -temp_name) #%>%
#  left_join(t_myfloridahouse %>% select(district_number, last_name, mfh_member_id), 
#            by = c("district_number", "last_name")) %>%
#  mutate(mfh_member_id = ifelse(chamber == 'House', mfh_member_id, NA))

p_legislator_votes <- t_legislator_votes %>%
  inner_join(hist_leg_sessions %>%
               select(people_id, session, party, legislator_name), by = c("people_id", "session")) %>%
  inner_join(p_roll_calls, by = c("roll_call_id", "session"))  %>%
  inner_join(p_legislators %>%
               select (people_id, termination_date), by = "people_id")

jct_bill_categories <- user_bill_categories %>%
  inner_join(p_sessions %>% select(session_year,session_id), by = 'session_year') %>%
  inner_join(p_bills %>% select(bill_number,session_id,bill_id), by=c('bill_number','session_id'))

##########################################
#                                        #  
# 1b) construct history of demographics  #
#                                        #
##########################################
# prepare demographics history table to enable choice of demographic snapshots based on setting_party_loyalty

# combine Dave's Redistricting raw data tables (https://davesredistricting.org/) into one table for all congressional districts
t_daves_districts_house$chamber= 'House'
t_daves_districts_senate$chamber= 'Senate'
calc_daves_districts_combined <- rbind(t_daves_districts_house, t_daves_districts_senate)

#define function to pull demographic group data based on field name prefix
get_demographics <- function(prefix, source_label, year) {
  calc_daves_districts_combined %>%
    rename(
      district_number = ID
    ) %>%
    mutate(
      n_white = !!sym(paste0(prefix, "White")),
      n_hispanic = !!sym(paste0(prefix, "Hispanic")),
      n_black = !!sym(paste0(prefix, "Black")),
      n_asian = !!sym(paste0(prefix, "Asian")),
      n_native = !!sym(paste0(prefix, "Native")),
      n_pacific = !!sym(paste0(prefix, "Pacific")),
      n_total_demo = !!sym(paste0(prefix, "Total")),
      pct_white = n_white / n_total_demo,
      pct_hispanic = n_hispanic / n_total_demo,
      pct_black = n_black / n_total_demo,
      pct_asian = n_asian / n_total_demo,
      pct_napi = (n_native + n_pacific) / n_total_demo,
      source_demo = source_label,
      year_demo = year
    ) %>%
    select(chamber, district_number, n_white, n_hispanic, n_black, n_asian, n_native, n_pacific, n_total_demo, pct_white, pct_hispanic, pct_black, pct_asian, pct_napi, source_demo, year_demo)
}

# get 2022 CVAP and 2022 ACS, then bind into one table
demo_prefixes <- grep("^(T|V)_\\d{2}_(ACS|CVAP|CENS)_Total$", names(calc_daves_districts_combined), value = TRUE)
# Now extract just the prefix (everything up to and including the last underscore before Total)
demo_prefixes <- gsub("Total$", "", demo_prefixes)  # removes "Total" but leaves trailing "_"

parse_source_label_year <- function(prefix) {
  # e.g., prefix: "T_22_ACS_" or "V_22_CVAP_"
  parts <- unlist(strsplit(gsub("_$", "", prefix), "_"))
  year <- as.integer(parts[2])
  source_label <- parts[3]
  list(year = year, source_label = source_label)
}
list_df_demos <- lapply(demo_prefixes, function(prefix) {
  info <- parse_source_label_year(prefix)
  get_demographics(prefix, info$source_label, info$year)
})
hist_district_demo <- do.call(rbind, list_df_demos)

##############################################
#                                            #  
# 1c) construct history of election results  #
#                                            #
##############################################
#define function to pull demographic group data based on field name prefix
get_election_results <- function(prefix, source_label, is_include) {
  calc_daves_districts_combined %>%
    rename(
      district_number = ID
    ) %>%
    mutate(
      n_Dem  = !!sym(paste0(prefix, "Dem")),
      n_Rep = !!sym(paste0(prefix, "Rep")),
      n_Total_Elec = !!sym(paste0(prefix, "Total")),
      pct_D = n_Dem / n_Total_Elec,
      pct_R = n_Rep / n_Total_Elec,
      source_elec = source_label
    ) %>%
    select(chamber,district_number,n_Dem, n_Rep, n_Total_Elec,pct_D,pct_R,source_elec)
}

# get election results, then bind into one table
election_prefixes <- grep("^E_.*_Dem$", names(calc_daves_districts_combined), value = TRUE)
list_str_elections <- gsub("^E_(.*)_Dem$", "\\1", election_prefixes)
list_df_elections <- lapply(list_str_elections, function(type_year) {
  prefix <- paste0("E_", type_year, "_")
  get_election_results(prefix, type_year)
})
hist_district_elections <- do.call(rbind, list_df_elections)

#######################################
#                                     #  
# 2a) roll call partisanship analysis #
#                                     #
#######################################
# The following two partisanship analysis sections are adapted from Andrew's work.
# 7/16/24 I purged code and fields (e.g. partisan_desc, bill_alignment, priority_bills) which aren't used anywhere in the current app
# These could be added back later, for data throughput efficiency

# primary key is roll_call_id, party
calc_rc01_by_party <- p_legislator_votes %>%
  group_by(party,roll_call_id, vote_text) %>%
  summarize(n=n()) %>% arrange(desc(n)) %>% 
  pivot_wider(values_from = n,names_from = vote_text,values_fill = 0) %>% 
  mutate(
    n_total=sum(Yea,Nay,NV,Absent,na.rm = TRUE),
    n_present=sum(Yea,Nay)
  ) %>%
  mutate(
    party_pct_of_present = Yea/(n_present),
  ) %>%
  select(party,roll_call_id,party_pct_of_present, n_present)

# primary key is roll_call_id
# roll up party counts to get one row per roll call with partisanship descriptors
calc_rc02_partisan_pivot <- calc_rc01_by_party %>%
  pivot_wider(names_from = party,values_from=party_pct_of_present,values_fill = NA,id_cols = c(roll_call_id)) %>% 
  mutate(
    dem_majority = case_when(
      D > 0.5 ~ "Y",
      D < 0.5 ~ "N",
      D == 0.5 ~ "Equal",
      TRUE ~ NA_character_
    ),
    gop_majority = case_when(
      R > 0.5 ~ "Y",
      R < 0.5 ~ "N",
      R == 0.5 ~ "Equal",
      TRUE ~ NA_character_
    )
  )

#############################################
#                                           #  
# 2b) legislator-vote partisanship analysis #
#                                           #
#############################################
# join partisanship analysis to p_legislator_votes to contextualize individual votes within party-level analysis
# remove roll calls with no date or no vote total, but in my test case the # of records is identically 213,203
calc_votes01_both_parties_present <- p_legislator_votes %>%
  left_join(
    calc_rc02_partisan_pivot,
    by = 'roll_call_id'
  ) %>%
  filter(!is.na(D) & !is.na(R))

# for individual votes, classify relationship between individual votes and party-level majorities
# RR 7/16/24 added vote_with_both and vote_with_same
calc_votes02_w_partisan_stats <- calc_votes01_both_parties_present %>%
  mutate(
    vote_with_dem_majority = ifelse((dem_majority == "Y" & vote_text == "Yea")|dem_majority=="N" & vote_text=="Nay", 1, 0),
    vote_with_gop_majority = ifelse((gop_majority == "Y" & vote_text == "Yea")|gop_majority=="N" & vote_text=="Nay", 1, 0),
    vote_against_both = ifelse(
      (dem_majority == "Y" & gop_majority == "Y" & vote_text == "Nay") | (dem_majority == "N" & gop_majority == "N" & vote_text == "Yea"), 1, 0),
    voted_at_all = (vote_with_dem_majority+vote_with_gop_majority+vote_against_both)>=1,
    vote_cross_party=ifelse(
      (party=="D" & vote_text=="Yea" & dem_majority=="N" & gop_majority=="Y") |
        (party=="D" & vote_text=="Nay" & dem_majority=="Y" & gop_majority=="N") |
        (party=="R" & vote_text=="Yea" & gop_majority=="N" & dem_majority=="Y") |
        (party=="R" & vote_text=="Nay" & gop_majority=="Y" & dem_majority=="N"),
      1,0 ),
    ## RR added 7/16/24, vote with same party's majority regardless of oppo party majority 
    vote_party_line = ifelse(
      (vote_with_dem_majority & party == "D")|
        (vote_with_gop_majority & party == "R")
      , 1, 0)
  )

##################################################
#                                                #  
# 3) label each legislator-vote by partisanship  #
#                                                #
##################################################
# prepare legislator-votes data table for calculating party loyalty based on setting_demo_src and setting_demo_year

# partisan_vote_type is calculated here as an intermediate categorical variable
# partisan_vote_weight can then be calculated later, dependent on setting_party_loyalty
# (Andrew Pantazi's work creating calc_leg_votes_partisan can be found at
#      https://github.com/reliablerascal/fl-legislation-etl/blob/main/scripts/03a_process.R


# consolidate vote pattern into a single variable partisan_vote_type
# "Other" includes absent, no vote, same party vote split equally, or nay vote when oppo party split equally (see qa_checks.R)
calc_votes03_categorized <- calc_votes02_w_partisan_stats %>%
  mutate(
    partisan_vote_type = case_when(
      vote_against_both == 1 ~ "Against Both Parties",
      vote_cross_party == 1 ~ "Cross Party",
      party=="D" & vote_with_dem_majority == 1 & vote_with_gop_majority == 0 ~ "Party Line Partisan",
      party=="R" & vote_with_dem_majority == 0 & vote_with_gop_majority == 1 ~ "Party Line Partisan",
      vote_party_line == 1 ~ "Party Line Bipartisan",
      vote_text == "NV" ~ "Absent/NV",
      vote_text == "Absent" ~ "Absent/NV",
      TRUE ~ "Other"
    ) %>% 
      factor(levels = c("Against Both Parties", "Cross Party", "Party Line Partisan", "Party Line Bipartisan", "Absent/NV", "Other"))
  )

#7/26/24 RR can get rid of this section when we confirm the new partisan_vote_type calculation is good
# calc_votes03_categorized <- calc_votes03_categorized %>%
#   mutate(
#     partisan_vote_type_OLD = case_when(
#       vote_with_neither == 1 ~ "Against Both Parties",
#       maverick_votes == 1 ~ "Cross Party",
#       vote_with_same == 1 ~ "Party Line",
#       TRUE ~ "Other"
#     ) %>%
#       factor(levels = c("Against Both Parties", "Cross Party", "Party Line", "Other"))
#   )

# fold calculated partisan_vote_type into p_legislator_votes data frame
p_legislator_votes <- p_legislator_votes %>%
  left_join(calc_votes03_categorized %>%
              select(people_id,roll_call_id,partisan_vote_type, vote_against_both, vote_with_dem_majority, vote_with_gop_majority, vote_cross_party, vote_party_line, voted_at_all),
            by = c('people_id','roll_call_id')
  ) %>% distinct()

# roll call summaries, 6271
p_roll_calls <- p_roll_calls %>%
  left_join(calc_rc02_partisan_pivot %>%
              select(roll_call_id,R,D),
            by = 'roll_call_id'
  ) %>%
  rename (
    R_pct_of_present = R,
    D_pct_of_present = D
  )
