setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Install packages --------------------------------------------------------
library(agricolae)
library(Rmisc)
library(gdata)
library(rlist)
library(stringr)
library(ggplot2)
library(grid)
library(fBasics)
library(reshape2)
library(gdata)
library(rlist)
library(stringr)
library(readxl)
library(knitr)
library(devtools)
library(Hmisc)
library(fuzzyjoin)
library(rmarkdown)
library(kableExtra)
library(here)
library(naniar)
library(lubridate)
library(rstudioapi)
library(data.table)
library(readr)
library(textclean)
library(janitor)
library(tidyverse)
library(dplyr)
library(zoo)
library(padr)

# REDCap Upload and Tidy --------------------------------------------------
#Upload REDCap Report
REDCap <-  read_xlsx("Data/REDCap.xlsx",
                     col_types = c("text",    "text",    "date",    "text",    "text",
                                   "text",    "text",    "date",    "text",    "numeric",
                                   "text",    "text",    "text",    "logical", "logical", 
                                   "date",    "text",    "text",    "text",    "numeric",
                                   "numeric", "text",    "text",    "numeric", "text",
                                   "text",    "text",    "text",    "logical", "date",
                                   "logical", "logical", "logical", "logical", "logical",
                                   "logical", "logical", "logical", "logical", "logical",
                                   "logical", "logical", "logical", "text",    "logical",
                                   "logical", "logical", "logical", "logical", "logical",
                                   "logical", "text",    "text",    "logical", "logical",
                                   "numeric", "numeric", "logical", "logical", "logical",
                                   "logical", "numeric", "numeric", "numeric", "numeric",
                                   "numeric", "text",    "numeric", "logical", "logical", 
                                   "logical", "text",    "logical", "logical", "numeric", 
                                   "date",    "numeric", "numeric", "date",    "numeric"),
                     na = c("[not completed]", "empty", "null", "", "NA")) 
redcap_colnames <- read_csv("Data/Reference/redcap_colnames2.csv",
                            col_types = cols(.default = "c"))
names(REDCap) = redcap_colnames$Names_tidy[match(names(REDCap), redcap_colnames$Names_raw)]

#Add in duplicates, canceled, and sent elsewhere
dcs <- read_csv("Data/dcs.csv", 
                col_types = cols(.default = "c")) %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  mutate(status_comment = tolower(status_comment)) %>% 
  distinct(record_id, .keep_all = T) %>% 
  filter(status_comment %in% c("c", "se")) %>% 
  mutate(status_comment = case_when(
    status_comment == "c" ~ "cancelled",
    status_comment == "se" ~ "sent elsewhere")) 

#Tidy REDCap
`%notin%` <- Negate(`%in%`)
tREDCap <- full_join(REDCap, dcs, by = "record_id") %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  mutate_at(vars(date_birth, date_collected, date_onset), 
            ~as_date(.)) %>% 
  mutate_at(vars(covid19_timestamp), ~as_datetime(.)) %>% 
  mutate_if(is.character, ~tolower(.), ~trimws(.)) %>% 
  mutate(record_id = str_replace_all(record_id, c("^wy" = "500000", "^wmci" = "100000"))) %>%  
  mutate_at(vars(record_id), ~as.numeric(.)) %>% 
  mutate(mrn = str_replace_all(mrn, c("^xxx-xx-" = "","^xx+" = "","^mrn: "="","^0+"=""))) %>% 
  mutate_at(vars(mrn, last_name, first_name), ~str_replace_all(.,c("[:punct:]" = " ", "  " = " "))) %>% 
  mutate(last_name = str_replace_all(last_name, c("^chair" = "c hair", "^cbearing" = "c bearing", "^stclair" = "st clair"))) %>% 
  mutate(status_comment = case_when(
    is.na(`last_name`) & is.na(`first_name`) & is.na(`date_birth`) ~ "entry error",
    !(is.na(`last_name`) & is.na(`first_name`) & is.na(`date_birth`)) ~ status_comment)) 
rm(REDCap, dcs, redcap_colnames)

# LIMS Upload and Tidy ----------------------------------------------------
LIMS <- read_excel("Data/LIMS.xlsx", 
                  col_types = c("date", "text", "numeric","text", "text", 
                                "text", "text", "text",   "text", "text",
                                "text", "text", "text",   "text", "text",
                                "text", "date", "text",   "text", "date",
                                "text", "text", "date",   "text", "text", 
                                "text", "text", "text", "text"),  na = "/  /") 
lims_colnames <- read_csv("Data/Reference/lims_colnames.csv",
                          col_types = cols(.default = "c"))
names(LIMS) = lims_colnames$names_tidy[match(names(LIMS), lims_colnames$names_raw)]

#Trim and tidy LIMS
`%notin%` <- Negate(`%in%`)
tLIMS <- LIMS %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  mutate_if(is.character, ~tolower(.)) %>% 
  mutate_at(vars(date_collected, birth_date, date_approved, date_received), 
            ~as_date(.)) %>%  
  mutate(analyte_name = str_replace(analyte_name, "2019-ncov rrt-pcr", "sars-cov-2")) %>% 
  mutate_at(vars(mrn, last_name, first_name), ~str_replace_all(.,c("[:punct:]" = " ", "  " = " "))) %>% 
  mutate(mrn = str_replace_all(mrn, c("^xx+" = "","^mrn "="","^0+"=""))) %>% 
  mutate(last_name = str_replace_all(last_name, c("^chair" = "c hair", "^cbearing" = "c bearing", "^stclair" = "st clair"))) %>% 
  mutate(test_result = str_replace_all(test_result,c("[*]"="", 
                                                     "^presumptive positive 2019-ncov$"="positive 2019-ncov", 
                                                     "^positive$"="positive 2019-ncov"))) %>% 
  mutate(turnaround_time = as.duration(date_received %--% date_approved)/ddays(1)) %>% 
  mutate(last_name = ifelse(
    is.na(last_name) & is.na(first_name) & is.na(birth_date) & !is.na(test_name) & !is.na(submitter_name),
    "wastewater", last_name)) %>% 
  mutate(first_name = ifelse(
    last_name == "wastewater" & is.na(first_name) & is.na(birth_date) & !is.na(test_name) & !is.na(submitter_name),
    "wastewater", first_name)) %>% 
  mutate(run_number = case_when(
    str_detect(lims_subsample_id,"[0-9]$") ~ "1",
    str_detect(lims_subsample_id,"[0-9]a$") ~ "2",
    str_detect(lims_subsample_id,"[0-9]aa$") ~ "3",
    str_detect(lims_subsample_id,"[0-9]b$") ~ "3",
    str_detect(lims_subsample_id,"[0-9]ab$") ~ "4",
    str_detect(lims_subsample_id,"[0-9]aaa$") ~ "4",
    str_detect(lims_subsample_id,"[0-9]ba$") ~ "4",
    str_detect(lims_subsample_id,"[0-9]c$") ~ "5",
    str_detect(lims_subsample_id,"[0-9]ac$") ~ "5",
    str_detect(lims_subsample_id,"[0-9]aaaa$") ~ "5",   
    str_detect(lims_subsample_id,"[0-9]d$") ~ "6",
    str_detect(lims_subsample_id,"[0-9]ad$") ~ "6")) %>% 
  mutate(run_number2 = str_remove_all(lims_subsample_id, "[0-9]")) %>% 
  mutate(run_number2 = ifelse(run_number2 == "", "1", run_number2)) %>% 
    arrange(lims_id, run_number) %>% 
  mutate(county = ifelse(county == "out of state", county, paste0(county ," county")))
rm(LIMS, lims_colnames)

# Wide LIMS conversion----------------------------------------------------
cLIMS <- tLIMS %>% 
  pivot_wider(id_cols = `lims_subsample_id`,
              names_from = `analyte_name`,
              values_from = `test_result`) %>% 
  rename_all(~str_replace_all(.,"2019-ncov","")) %>% 
  clean_names(.) %>% 
  mutate(`sars_cov_2` = if_else(
    rnase_p_2 != "invalid" | is.na(rnase_p_2), `sars_cov_2`, rnase_p_2)) %>% 
  mutate(`influenza_virus_a` = if_else(
    rnase_p_2 != "invalid" | is.na(rnase_p_2), `influenza_virus_a`, rnase_p_2)) %>% 
  mutate(`influenza_virus_b` = if_else(
    rnase_p_2 != "invalid" | is.na(rnase_p_2), `influenza_virus_a`, rnase_p_2)) 

dLIMS <- tLIMS %>% 
  distinct(`lims_subsample_id`,.keep_all = T) %>% 
  pivot_wider(
    id_cols = `lims_id`,
    names_from = `run_number`,
    values_from = `lims_subsample_id`,
    names_prefix = "run_") %>% 
  mutate_if(is.list, as.character)

w1LIMS <- 
  left_join(dLIMS, cLIMS, 
            by = c("run_1"="lims_subsample_id"))
w2LIMS <-
  left_join(dLIMS, cLIMS, 
            by = c("run_2"="lims_subsample_id"))
w3LIMS <-
  left_join(dLIMS, cLIMS, 
            by = c("run_3"="lims_subsample_id"))
w4LIMS <-
  left_join(dLIMS, cLIMS, 
            by = c("run_4"="lims_subsample_id"))
w5LIMS <-
  left_join(dLIMS, cLIMS, 
            by = c("run_5"="lims_subsample_id"))
w6LIMS <-
  left_join(dLIMS, cLIMS, 
            by = c("run_6"="lims_subsample_id"))

waLIMS<-
  left_join(w1LIMS, w2LIMS, by = c("lims_id", "run_1", "run_2", "run_3", "run_4", "run_5", "run_6"), 
            suffix = c("_1", "_2"))
wbLIMS<-
  left_join(w3LIMS, w4LIMS, by = c("lims_id", "run_1", "run_2", "run_3", "run_4", "run_5", "run_6"), 
            suffix = c("_3", "_4"))
wcLIMS<-
  left_join(w5LIMS, w6LIMS, by = c("lims_id", "run_1", "run_2", "run_3", "run_4", "run_5", "run_6"), 
            suffix = c("_5", "_6"))
wdLIMS<-
  left_join(waLIMS, wbLIMS, by = c("lims_id", "run_1", "run_2", "run_3", "run_4", "run_5", "run_6")) 
weLIMS<-
  left_join(wdLIMS, wcLIMS, by = c("lims_id", "run_1", "run_2", "run_3", "run_4", "run_5", "run_6")) 

wLIMS <- 
  left_join(weLIMS, tLIMS, by = "lims_id") %>% 
  distinct(`lims_id`, .keep_all = T) %>% 
  mutate(`test_result_cov`= case_when(
    sars_cov_2_6 = str_detect(sars_cov_2_6,"[a-z]") ~ sars_cov_2_6,
    sars_cov_2_5 = str_detect(sars_cov_2_5,"[a-z]") ~ sars_cov_2_5,
    sars_cov_2_4 = str_detect(sars_cov_2_4,"[a-z]") ~ sars_cov_2_4,
    sars_cov_2_3 = str_detect(sars_cov_2_3,"[a-z]") ~ sars_cov_2_3,
    sars_cov_2_2 = str_detect(sars_cov_2_2,"[a-z]") ~ sars_cov_2_2,
    sars_cov_2_1 = str_detect(sars_cov_2_1,"[a-z]") ~ sars_cov_2_1)) %>% 
  mutate(test_result_cov = str_replace_all(test_result_cov, c("positive 2019-ncov" = "sars-cov2 positive", 
                                                              "^detected$" = "sars-cov2 positive",
                                                              "presumptive positive" = "sars-cov2 positive", 
                                                              "negative" = "sars-cov2 negative", 
                                                              "^not detected$" = "sars-cov2 negative",
                                                              "inconclusive" = "sars-cov2 inconclusive",
                                                              "invalid" = "invalid sample", 
                                                              "not tested" = "sample not tested"))) %>% 
  mutate(`test_result_flu_a`= case_when(
    influenza_virus_a_6 = str_detect(influenza_virus_a_6,"[a-z]") ~ influenza_virus_a_6,
    influenza_virus_a_5 = str_detect(influenza_virus_a_5,"[a-z]") ~ influenza_virus_a_5,
    influenza_virus_a_4 = str_detect(influenza_virus_a_4,"[a-z]") ~ influenza_virus_a_4,
    influenza_virus_a_3 = str_detect(influenza_virus_a_3,"[a-z]") ~ influenza_virus_a_3,
    influenza_virus_a_2 = str_detect(influenza_virus_a_2,"[a-z]") ~ influenza_virus_a_2,
    influenza_virus_a_1 = str_detect(influenza_virus_a_1,"[a-z]") ~ influenza_virus_a_1)) %>% 
  mutate(test_result_flu_a = str_replace_all(test_result_flu_a, c("^detected$" = "influenza a positive",
                                                              "^not detected$" = "influenza a negative",
                                                              "^invalid$" = "invalid sample", 
                                                              "^not tested$" = "sample not tested"))) %>% 
  mutate(`test_result_flu_b`= case_when(
    influenza_virus_b_6 = str_detect(influenza_virus_b_6,"[a-z]") ~ influenza_virus_b_6,
    influenza_virus_b_5 = str_detect(influenza_virus_b_5,"[a-z]") ~ influenza_virus_b_5,
    influenza_virus_b_4 = str_detect(influenza_virus_b_4,"[a-z]") ~ influenza_virus_b_4,
    influenza_virus_b_3 = str_detect(influenza_virus_b_3,"[a-z]") ~ influenza_virus_b_3,
    influenza_virus_b_2 = str_detect(influenza_virus_b_2,"[a-z]") ~ influenza_virus_b_2,
    influenza_virus_b_1 = str_detect(influenza_virus_b_1,"[a-z]") ~ influenza_virus_b_1)) %>% 
  mutate(test_result_flu_b = str_replace_all(test_result_flu_b, c("^detected$" = "influenza b positive",
                                                                  "^not detected$" = "influenza b negative",
                                                                  "^invalid$" = "invalid sample", 
                                                                  "^not tested$" = "sample not tested"))) %>% 
    select(lims_id, date_collected, mrn, birth_date, last_name, first_name,
         status, test_result_cov, test_result_flu_a, test_result_flu_b, date_received, date_approved,
         run_1, sars_cov_2_1, influenza_virus_a_1, influenza_virus_b_1, 
         run_2, sars_cov_2_2, influenza_virus_a_2, influenza_virus_b_2, 
         run_3, sars_cov_2_3, influenza_virus_a_3, influenza_virus_b_3, 
         run_4, sars_cov_2_4, influenza_virus_a_4, influenza_virus_b_4, 
         run_5, sars_cov_2_5, influenza_virus_a_5, influenza_virus_b_5,
         run_6, sars_cov_2_6, influenza_virus_a_6, influenza_virus_b_6, everything())

rm(cLIMS,dLIMS,tLIMS,w1LIMS, w2LIMS, w3LIMS, w4LIMS, w5LIMS, w6LIMS, waLIMS, wbLIMS, wcLIMS, wdLIMS, weLIMS)
write.csv(wLIMS, file = "Data/wLIMS.csv", row.names = F)

# LWP upload and tidy ----------------------------------------------------
LWP <- read_excel("Data/LWP.xlsx", col_types = c("text", "numeric", "text",    "date", "text", 
                                                 "text", "date",    "date",    "text", "text", 
                                                 "text", "date",    "numeric", "date", "text",
                                                 "text", "text",    "text",    "text", "text", 
                                                 "text", "text",    "text",    "text", "text"))

lwp_colnames <- read_csv("Data/Reference/lwp_colnames.csv",
                         col_types = cols(.default = "c"))
names(LWP) = lwp_colnames$Names_tidy[match(names(LWP), lwp_colnames$Names_raw)]

#Tidy LWP
tLWP <-  LWP %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  mutate_if(is.character, tolower) %>% 
  separate(name, into = c("last_name", "first_name"), sep = ", ", remove = T, extra = "merge") %>% 
  mutate_at(vars(mrn, last_name, first_name), ~str_replace_all(.,c("[:punct:]" = " ", "  " = " "))) %>% 
  mutate(mrn = str_replace_all(mrn, c("^xx+" = "","^mrn "="","^0+"=""))) %>% 
  mutate_if(is.character, trimws) %>% 
  mutate(status = str_replace(status,"^in","in ")) %>% 
  mutate(record_id = as.numeric(str_replace(record_id, "^oidwy", ""))) %>% 
  mutate_at(vars(dead, healthcare_work, closecontact, caretaker, epirequest, rapidtest), 
            ~case_when(
              . == "no" ~ "0",
              . == "yes" ~ "1",
              . == "unknown" ~ "2")) %>% 
  mutate_at(vars(dead, healthcare_work, closecontact, caretaker, epirequest, rapidtest), ~as.logical(.))
write.csv(tLWP, file = "Data/tLWP.csv", row.names = F)
rm(LWP, lwp_colnames)

# Master Creation ---------------------------------------------------------
#LWP
confidence_lwp <- 
  right_join(tLWP, wLIMS,
             by = "lims_id",
             suffix = c(".lwp",".lims")) %>% 
  mutate(confidence0 = "lwp") %>% 
  select(`record_id`, `lims_id`, `confidence0`, status.lwp, lwp_timestamp) %>% 
  filter(!is.na(`record_id`),!is.na(`lims_id`)) 

#High confidence match
confidence_1 <- 
  right_join(tREDCap, wLIMS, 
             by = c("date_collected" = "date_collected",
                    "mrn",
                    "date_birth" = "birth_date",
                    "last_name", 
                    "first_name"),
             suffix = c(".r",".l")) %>% 
  filter(status_comment != "entry error" | is.na(status_comment)) %>% 
  mutate(confidence1 = "1") %>% 
  select(`record_id`, `lims_id`, `confidence1`, status_comment) %>% 
  filter(!is.na(`record_id`),!is.na(`lims_id`)) 

master <- 
  full_join(confidence_lwp, confidence_1, 
            by = c("record_id",  "lims_id")) %>% 
  select(`record_id`, `lims_id`, confidence0, confidence1, status.lwp, lwp_timestamp)

#High confidence matchs pt2
confidence_1.1 <- 
  right_join(tREDCap, wLIMS, 
             by = c("date_collected" = "date_collected",
                    "mrn",
                    "date_birth" = "birth_date",
                    "last_name" = "first_name", 
                    "first_name" = "last_name"),
             suffix = c(".r",".l")) %>% 
  filter(status_comment != "entry error" | is.na(status_comment)) %>% 
  mutate(confidence1.1 = "1") %>% 
  select(`record_id`, `lims_id`, `confidence1.1`) %>% 
  filter(!is.na(`record_id`),!is.na(`lims_id`)) 

master <- 
  full_join(master, confidence_1.1, 
            by = c("record_id",  "lims_id")) %>% 
  select(`record_id`, confidence0, `lims_id`, confidence1, `confidence1.1`, status.lwp, lwp_timestamp)

#Medium-high confidence matchs
confidence_2 <- 
  right_join(tREDCap, wLIMS, 
             by = c("date_collected",
                    "mrn",
                    "date_birth" = "birth_date",
                    "last_name"),
             suffix = c(".r",".l")) %>% 
  mutate(confidence2 = "2") %>% 
  filter(status_comment != "entry error" | is.na(status_comment)) %>% 
  select(`record_id`, `lims_id`, `confidence2`) %>% 
  filter(!is.na(`record_id`),!is.na(`lims_id`)) 

master <- 
  full_join(master, confidence_2, 
            by = c("record_id",  "lims_id")) %>% 
  select(`record_id`, confidence0, `lims_id`, confidence1, confidence1.1, `confidence2`, status.lwp, lwp_timestamp)

#Medium confidence matchs
confidence_3 <- 
  full_join(tREDCap, wLIMS, 
            by = c("date_collected" = "date_collected",
                   "mrn",
                   "date_birth" = "birth_date"),
            suffix = c(".r",".l")) %>% 
  filter(status_comment != "entry error" | is.na(status_comment)) %>% 
  mutate(confidence3 = "3") %>% 
  select(`record_id`, `lims_id`, `last_name.r`, `last_name.l`,  `confidence3`) %>% 
  filter(!is.na(`record_id`),!is.na(`lims_id`)) %>% 
  filter(substr(`last_name.r`,1,4) == substr(`last_name.l`,1,4))

master <- 
  full_join(master, confidence_3, 
            by = c("record_id",  "lims_id")) %>% 
  select(`record_id`, confidence0, `lims_id`, confidence1, confidence1.1,`confidence2`, confidence3, status.lwp, lwp_timestamp)

#Medium pt2 confidence matchs
confidence_3.1 <- 
  right_join(tREDCap, wLIMS, 
             by = c("date_collected" = "date_collected",
                    "date_birth" = "birth_date",
                    "last_name" = "first_name",
                    "first_name" = "last_name"),
             suffix = c(".r",".l")) %>% 
  filter(status_comment != "entry error" | is.na(status_comment)) %>% 
  mutate(confidence3.1 = "3") %>% 
  select(`record_id`, `lims_id`, `confidence3.1`) %>% 
  filter(!is.na(`record_id`),!is.na(`lims_id`))

master <- 
  full_join(master, confidence_3.1, 
            by = c("record_id",  "lims_id")) %>% 
  select(`record_id`, confidence0, `lims_id`, confidence1, confidence1.1, confidence2, confidence3, confidence3.1, status.lwp, lwp_timestamp)

#Medium-low confidence matchs
confidence_4 <- 
  full_join(tREDCap, wLIMS, 
            by = c("date_collected" = "date_collected",
                   "mrn"),
            suffix = c(".r",".l")) %>% 
  filter(status_comment != "entry error" | is.na(status_comment)) %>% 
  filter(!is.na(mrn)) %>% 
  mutate(confidence4 = "4") %>% 
  select(`record_id`, `lims_id`, last_name.r, last_name.l, first_name.r, first_name.l, `confidence4`) %>% 
  filter(!is.na(`record_id`),!is.na(`lims_id`)) %>% 
  filter(substr(`last_name.r`,1,4) == substr(`last_name.l`,1,4))%>% 
  filter(substr(`first_name.r`,1,4) == substr(`first_name.l`,1,4))

master <- 
  full_join(master, confidence_4, 
            by = c("record_id",  "lims_id")) %>% 
  select(`record_id`, `lims_id`, confidence0, confidence1, confidence1.1, `confidence2`, confidence3, 
         `confidence3.1`, confidence4, status.lwp, lwp_timestamp)

#Low confidence matchs
confidence_5 <- 
  full_join(tREDCap, wLIMS, 
            by = c("date_collected" = "date_collected",
                   "date_birth" = "birth_date",
                   "last_name",
                   "first_name"),
            suffix = c(".r",".l")) %>% 
  filter(status_comment != "entry error" | is.na(status_comment)) %>% 
  mutate(confidence5 = "5") %>% 
  select(`record_id`, `lims_id`, `confidence5`) %>% 
  filter(!is.na(`record_id`),!is.na(`lims_id`))

master <- 
  full_join(master, confidence_5, 
            by = c("record_id",  "lims_id")) %>% 
  select(`record_id`, `lims_id`, confidence0, confidence1, confidence1.1,`confidence2`, confidence3, 
         `confidence3.1`, confidence4, confidence5, status.lwp, lwp_timestamp)

unmatch_redcap <- anti_join(tREDCap,master, by = "record_id")

unmatch_lims<- anti_join(wLIMS, master, by = "lims_id") %>% 
  select(lims_id, date_received, status,            test_result_cov,  test_result_flu_a,  test_result_flu_b,  date_collected, 
         mrn,     birth_date,    last_name,         first_name,   specimen_source,
         county,  city,          submitter_name,    lims_submitter_id) %>% 
  filter(!(last_name == "wastewater" & first_name == "wastewater")) %>% 
  filter(is.na(`mrn`) | !(str_detect(`mrn`, "validation [0-9]"))) %>% 
  filter(!(is.na(`last_name`) & is.na(`first_name`) & is.na(`birth_date`)))

#Handling known misfits, match within date range of +/- 4 days
confidence_6 <- 
  full_join(tREDCap,unmatch_lims, 
            by = c("mrn",
                   "date_birth" = "birth_date",
                   "last_name",
                   "first_name"),
            suffix = c(".r",".l")) %>% 
  filter(status_comment != "entry error" | is.na(status_comment)) %>% 
  mutate(confidence6 = "6") %>% 
  select(`record_id`, `lims_id`, date_collected.r, date_collected.l, `confidence6`) %>% 
  filter(date_collected.l <= date_collected.r + days(4) &
           date_collected.l >= date_collected.r - days(4)) 

master <- 
  full_join(master, confidence_6, 
            by = c("record_id",  "lims_id")) %>% 
  select(`record_id`, `lims_id`, confidence0, confidence1, confidence1.1, `confidence2`, confidence3, 
         `confidence3.1`, confidence4, confidence5, confidence6, status.lwp, lwp_timestamp) %>% 
  mutate(merge_timestamp = Sys.time())

#True data misfits
unmatch_lims<- anti_join(wLIMS, master, by = "lims_id") %>% 
  select(lims_id, date_received, status,            test_result_cov, test_result_flu_a, test_result_flu_b,  date_collected, 
         mrn,     birth_date,    last_name,         first_name,   specimen_source,
         county,  city,          submitter_name,    lims_submitter_id) %>% 
  filter(!(last_name == "wastewater" & first_name == "wastewater")) %>% 
  filter(!(last_name %in% c("pt","verification") & birth_date == "2001-01-01")) %>% 
  filter(is.na(`mrn`) | !(str_detect(`mrn`, "validation [0-9]"))) %>% 
  filter(!(is.na(`last_name`) & is.na(`first_name`) & is.na(`birth_date`)))

unmatch_redcap <- 
  anti_join(tREDCap,master, by = "record_id") %>% 
  filter(is.na(status_comment)| `status_comment` == "")

write.csv(unmatch_lims, file = "Data/unmatch_lims.csv", row.names = F)
write.csv(unmatch_redcap, file = "Data/unmatch_redcap.csv", row.names = F)

rm(confidence_lwp, confidence_1, confidence_1.1, confidence_2, confidence_3, confidence_3.1, confidence_4, 
   confidence_5, confidence_6)

# Fill in remainder ------------------------------------------------
omaster <- 
  full_join(master, tREDCap, by = "record_id", suffix = c(".m",".r")) 

nmaster <- 
  full_join(omaster, wLIMS, by = "lims_id", suffix = c(".m",".l")) %>% 
  unite(covid19_timestamp, c(covid19_timestamp, lwp_timestamp)) %>%  
  mutate(covid19_timestamp = str_remove(covid19_timestamp, "NA_")) %>% 
  mutate(covid19_timestamp = str_remove(covid19_timestamp, "_NA")) %>% 
  mutate(merge_confidence = case_when(
    `confidence0`=="lwp"~"lwp",
    `confidence1`=="1"~"1",
    `confidence1.1`=="1"~"1",
    `confidence2`=="2"~"2",
    `confidence3`=="3"~"3",
    `confidence3.1`=="3"~"3",
    `confidence4`=="4"~"4",
    `confidence5`=="5"~"5",
    `confidence6`=="6"~"6" )) %>% 
  mutate(status = ifelse(is.na(status), status.lwp, status)) %>% 
  mutate(status_comment.l = ifelse(`test_result_cov` == "not tested", "rejected", "")) %>% 
  unite(status_comment, c(status_comment, status_comment.l), 
        sep = " / ", remove = T, na.rm = T) %>% 
  mutate(status_comment = str_replace(`status_comment`, 
                                      "duplicate / rejected / rejected", "duplicate / rejected")) %>% 
  mutate(status_comment = str_replace(`status_comment`, 
                                      "duplicate / cancelled / ", "duplicate / cancelled")) %>% 
  mutate(status_comment = str_replace(`status_comment`, 
                                      "^rejected / $", "rejected"))%>% 
  mutate(status_comment = str_replace(`status_comment`, 
                                      " / $", "")) %>% 
  mutate(status_comment = str_replace(`status_comment`, 
                                      "^ / rejected$", "rejected"))%>% 
  mutate(status_comment = str_replace(`status_comment`, 
                                      "^rejected / rejected$", "rejected"))
rm(master, omaster)
# Provider call list ------------------------------------------------------
call_list_lwp <- tLWP %>% 
  filter(is.na(date_approved)) %>% 
  filter(date_collected <= as_date(today()-days(3))) %>% 
  mutate_at(vars(dob, date_collected), ~as_date(.))

call_list <- full_join(unmatch_redcap, call_list_lwp, 
                       by = c("record_id", "last_name", "first_name", "mrn", "date_birth" = "dob", 
                              "date_collected", "submitter", "dead", "rapidtest", 
                              "healthcare_work", "closecontact", "caretaker","preexist", "epirequest")) %>% 
  select(record_id, submitter, submitter_phone, date_collected, mrn, date_birth, first_name, last_name, orderer) %>% 
  arrange(submitter, date_collected, mrn, date_birth, last_name, first_name) %>% 
  filter(date_collected <= as_date(today()-days(3))) %>% 
  mutate(call_list_timestamp = Sys.time()) 
write.csv(call_list, file = "Data/call_list.csv", row.names = F) 
rm(call_list_lwp)

# Export falseneg master data ---------------------------------------------------------
#Tidy master for falseneg
falseneg_master <- nmaster %>% 
  mutate(date_collected = if_else(is.na(date_collected.l),
                                  date_collected.m,
                                  date_collected.l)) %>% 
  mutate(mrn = if_else(is.na(mrn.l),
                       mrn.m,
                       mrn.l)) %>% 
  mutate(birth_date = if_else(is.na(birth_date),
                              date_birth,
                              birth_date)) %>% 
  mutate(last_name = if_else(is.na(last_name.l),
                             last_name.m,
                             last_name.l)) %>% 
  mutate(first_name = if_else(is.na(first_name.l),
                              first_name.m,
                              first_name.l)) %>% 
  mutate(submitter_name = if_else(is.na(submitter_name),
                                  submitter,
                                  submitter_name)) %>% 
  select(merge_timestamp, merge_confidence, record_id, covid19_timestamp, lims_id, date_received, 
         status, status_comment, test_result_cov, test_result_flu_a, test_result_flu_b, 
         date_collected, specimen_source, mrn, birth_date, last_name, first_name, 
         date_onset, `symptoms___fever_>100.4`, symptoms___fever_subjective, symptoms___cough, symptoms___shortness_of_breath, symptoms___muscle_aches, 
         symptoms___sore_throat, symptoms___runny_nose, symptoms___nausea_vomiting, symptoms___headache, symptoms___abdominal_pain, 
         symptoms___diarrhea, symptoms___asymptomatic, symptoms___other, other_sx, 
         clinical___pneumonia, clinical___acute_respiratory_distress_syndrome, clinical___abnormal_xray, clinical___hospitalized, clinical___mechanical_ventilation,
         clinical___ECMO, clinical___none_of_above, other_etiology, illness_etiology, travel, travel_hx,
         healthcare_work, healthsymp, closecontact, caretaker, preexist, everything()) %>% 
  mutate(record_id = str_replace_all(record_id, c("^500000" = "WY", "^100000" = "WMCI")))

write.csv(falseneg_master, file = "Data/falseneg_master.csv", row.names = F) 

rm(falseneg_master)

# Tidy and Export master -----------------------------------------------------------
#Tidy master 
tmaster <- nmaster %>% 
  select(merge_timestamp, merge_confidence, record_id, covid19_timestamp, lims_id, date_received,  
         test_name, status, status_comment, test_result_cov, test_result_flu_a, test_result_flu_b, date_approved,
         date_collected.l, date_collected.m, specimen_source, mrn.l, mrn.m, birth_date, date_birth, last_name.l, last_name.m, first_name.l, first_name.m, 
         street_address, city.l, city.m, county.l, county.m, state, zip, sex, gender,
         orderer, submitter, submitter_name, submitter_phone, lims_submitter_id) %>% 
  mutate(county.m = case_when(
    county.m == "1"~	"albany county",
    county.m == "2"~	"big horn county",
    county.m == "3"~	"campbell county",
    county.m == "4"~	"carbon county",
    county.m == "5"~	"converse county",
    county.m == "6"~	"crook county",
    county.m == "7"~	"fremont county",
    county.m == "8"~	"goshen county",
    county.m == "9"~	"hot springs county",
    county.m == "10"~	"johnson county",
    county.m == "11"~	"laramie county",
    county.m == "12"~	"lincoln county",
    county.m == "13"~	"natrona county",
    county.m == "14"~	"niobrara county",
    county.m == "15"~	"park county",
    county.m == "16"~	"platte county",
    county.m == "17"~	"sheridan county",
    county.m == "18"~	"sublette county",
    county.m == "19"~	"sweetwater county",
    county.m == "20"~	"teton county",
    county.m == "21"~	"uinta county",
    county.m == "22"~	"washakie county",
    county.m == "23"~	"weston county",
    county.m == "24"~ "out of state")) %>% 
  mutate(state = case_when(
    state ==  "1"~	"AL",
    state ==  "2"~	"AK",
    state ==  "3"~	"AZ",
    state ==  "4"~	"AR",
    state ==  "5"~	"CA",
    state ==  "6"~	"CO",
    state ==  "7"~	"CT",
    state ==  "8"~	"DE",
    state ==  "9"~	"FL",
    state == "10"~	"GA",
    state == "11"~	"HI",
    state == "12"~	"ID",
    state == "13"~	"IL",
    state == "14"~	"IN",
    state == "15"~	"IA",
    state == "16"~	"KS",
    state == "17"~	"KY",
    state == "18"~	"LA",
    state == "19"~	"ME",
    state == "20"~	"MD",
    state == "21"~	"MA",
    state == "22"~	"MI",
    state == "23"~	"MN",
    state == "24"~	"MS",
    state == "25"~	"MO",
    state == "26"~	"MT",
    state == "27"~	"NE",
    state == "28"~	"NV",
    state == "29"~	"NH",
    state == "30"~	"NJ",
    state == "31"~	"NM",
    state == "32"~	"NY",
    state == "33"~	"NC",
    state == "34"~	"ND",
    state == "35"~	"OH",
    state == "36"~	"OK",
    state == "37"~	"OR",
    state == "38"~	"PA",
    state == "39"~	"RI",
    state == "40"~	"SC",
    state == "41"~	"SD",
    state == "42"~	"TN",
    state == "43"~	"TX",
    state == "44"~	"UT",
    state == "45"~	"VT",
    state == "46"~	"VA",
    state == "47"~	"WA",
    state == "48"~	"WV",
    state == "49"~	"WI",
    state == "50"~	"WY")) %>% 
  mutate(gender = case_when(
    gender == "1"~	"male",
    gender == "2"~	"female",
    gender == "3"~	"other")) %>% 
  mutate(covid_id = str_pad(group_indices(., birth_date, last_name.l, first_name.l), 5, "left", pad = "0")) %>% 
  mutate(date_collected = if_else(is.na(date_collected.l),
                                  date_collected.m,
                                  date_collected.l)) %>% 
  mutate(mrn = if_else(is.na(mrn.l),
                       mrn.m,
                       mrn.l)) %>% 
  mutate(birth_date = if_else(is.na(birth_date),
                              date_birth,
                              birth_date)) %>% 
  mutate(last_name = if_else(is.na(last_name.l),
                             last_name.m,
                             last_name.l)) %>% 
  mutate(first_name = if_else(is.na(first_name.l),
                              first_name.m,
                              first_name.l)) %>% 
  mutate(sex = if_else(is.na(sex),
                              gender,
                              sex)) %>% 
  mutate(submitter_name = if_else(is.na(submitter_name),
                                  submitter,
                                  submitter_name)) %>% 
  mutate(county = if_else(is.na(county.m),
                                  county.l,
                                  county.m)) %>% 
  mutate(city = if_else(is.na(city.m),
                            city.l,
                            city.m)) %>% 
  mutate(lims_id = case_when(
    test_name == "xpert xpress sars-cov-2" ~ paste0(lims_id ,"X"),
    test_name == "2019-ncov rrt-pcr" ~ paste0(lims_id, ""),
    test_name == "flu sc2 multiplex assay" ~ paste0(lims_id, ""),
    is.na(test_name) ~ paste0(lims_id, ""))) %>% 
  mutate(status = case_when(
    status_comment != "" ~ status_comment,
    test_result_cov == "sample not tested" ~ "Not Tested",
    status == "in transit" ~ "In Transit",
    status == "receivedinlab" ~ "Received In Lab",
    status == "in process" ~ "In Process",
    status == "released" ~ "Testing Completed")) %>% 
  select(merge_timestamp, merge_confidence,  record_id, lims_id, 
         status, date_received, test_result_cov, test_result_flu_a, test_result_flu_b, covid19_timestamp, 
         date_collected, mrn, birth_date, first_name, last_name, 
         county, state, zip, 
         orderer, submitter_name, submitter_phone, covid_id) %>% 
  mutate(record_id = str_replace_all(record_id, c("^500000" = "wy", "^100000" = "wmci"))) %>% 
  arrange(lims_id, record_id) %>% 
  mutate_at(vars(merge_timestamp, date_collected, birth_date, date_received, covid19_timestamp), 
            ~as.character(.)) %>% 
  replace_na(list(merge_timestamp = "", merge_confidence = "", record_id = "", lims_id = "", status = "", date_received = "", 
                  test_result_cov = "", test_result_flu_a = "", test_result_flu_b = "", covid19_timestamp = "[not completed]",
                  date_collected = "", mrn = "", birth_date = "", mrn = "", first_name = "", last_name = "", 
                  county = "", state = "", zip = "", 
                  orderer = "", submitter_name = "", submitter_phone = ""))  %>% 
  mutate(covid19_timestamp = str_replace(covid19_timestamp, "NA", "[not completed]")) %>% 
  mutate_if(is.character,  ~toupper(.)) 
write.csv(tmaster, file = "Data/tmaster.csv", row.names = F) 

#Export version
emaster <- tmaster %>% 
  select(merge_timestamp, merge_confidence,  record_id, lims_id, 
         status, date_received, test_result_cov, test_result_flu_a, test_result_flu_b, covid19_timestamp, 
         date_collected, mrn, birth_date, first_name, last_name, 
         county, state, zip, 
         orderer, submitter_name, submitter_phone)
emaster_colnames <- read_csv("Data/Reference/emaster_colnames.csv",
                            col_types = cols(.default = "c"))
names(emaster) = emaster_colnames$Names_tidy[match(names(emaster), emaster_colnames$Names_raw)]
write.csv(emaster, file = "Data/emaster.csv", row.names = F) 
rm(nmaster, emaster_colnames, emaster)

# New matchs --------------------------------------------------------------
#Manual master export new rows
newmatchs_lwp <- tmaster %>% 
  filter(merge_confidence == "LWP") %>% 
  mutate(covid19_timestamp = as_date(covid19_timestamp)) %>% 
  filter(covid19_timestamp >= as_date(today()-days(1))) %>% 
  mutate(covid19_timestamp = as.character(covid19_timestamp)) %>% 
  select(merge_timestamp, merge_confidence,  record_id, lims_id, 
         status, date_received, test_result_cov, test_result_flu_a, test_result_flu_b, covid19_timestamp, 
         date_collected, mrn, birth_date, first_name, last_name, 
         county, state, zip, 
         orderer, submitter_name, submitter_phone)

newmatchs_redcap <- tmaster %>% 
  filter(date_received == as_date(today())) %>% 
  filter(merge_confidence != "LWP") %>% 
  mutate(lims_id2 = as.numeric(str_replace(lims_id, "X", ""))) %>% 
  arrange(lims_id2) %>% 
  mutate(lims_id2 = as.character(lims_id2)) %>% 
  mutate(lims_id = if_else(is.na(lims_id),
                           lims_id,
                           lims_id2)) %>% 
  select(merge_timestamp, merge_confidence,  record_id, lims_id, 
         status, date_received, test_result_cov, test_result_flu_a, test_result_flu_b, covid19_timestamp, 
         date_collected, mrn, birth_date, first_name, last_name, 
         county, state, zip, 
         orderer, submitter_name, submitter_phone)

newmatchs <- union(newmatchs_redcap, newmatchs_lwp) %>% 
  filter(!(last_name == "WASTEWATER" & first_name == "WASTEWATER")) %>% 
  filter(!(last_name %in% c("PT","VERIFICATION") & birth_date == "2001-01-01")) %>% 
  arrange(lims_id)
 
newmatchs_colnames <- read_csv("Data/Reference/emaster_colnames.csv",
                             col_types = cols(.default = "c"))
names(newmatchs) = newmatchs_colnames$Names_tidy[match(names(newmatchs), newmatchs_colnames$Names_raw)]
write.csv(newmatchs, file = "Data/newmatchs.csv", row.names = F) 
rm(newmatchs_colnames, newmatchs_lwp)

