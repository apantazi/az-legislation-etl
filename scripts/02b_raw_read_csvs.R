# READ_CSVS.R
# 7/6/24 RR
# this script retrieves user-entered data and reads downloaded csvs

#reset working directory in case this script is run independently from etl main
setwd(script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path))

########################################
#                                      #  
# 1) read Google Sheets user-entered   #
#                                      #
########################################
gs4_deauth() # de-authorize to connect to publicly shared sheets

user_legislator_events <- read_sheet("https://docs.google.com/spreadsheets/d/1uKgB97Qg-zzsKxoPU9UAWXgrKCjdHdrFDgZMsdOY0J4/edit?usp=drive_link", col_types = "Dciccc")
user_bill_categories <- read_sheet("https://docs.google.com/spreadsheets/d/10QzH8OzjqOkPUbviChdtUkq7M98SOwtlsYKlXA30qhY/edit?usp=drive_link",col_types="cdc")

########################################
#                                      #  
# 2) read downloaded csvs              #
#                                      #
########################################

t_daves_districts_house <- read.csv("../data-raw/daves/t_daves_districts.csv")
t_daves_districts_senate <- read.csv("../data-raw/daves/t_daves_districts.csv")

########################################
#                                      #  
# 3) read scraped_data                 #
#                                      #
########################################

#t_myfloridahouse <- read.csv("../data-raw/myfloridahouse/t_myfloridahouse.csv")
