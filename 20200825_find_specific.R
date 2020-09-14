# Load Master -------------------------------------------------------------
tmaster <- read_csv("Data/tmaster.csv", 
                    col_types = cols(.default = "c")) %>% 
  mutate(lims_id2 = as.numeric(str_replace(lims_id, "X", ""))) %>% 
  mutate(record_id = as.numeric(str_replace_all(record_id, c("^WY" = "500000", "^WMCI" = "100000")))) 

# New Matches - find specific ---------------------------------------------
newmatchs1 <- tmaster %>% 
  filter(lims_id2 %in% c(1, 2)
         |
         record_id %in% c(1, 2)         
         |
         between(lims_id2, 1, 2)) %>% 
  select(merge_timestamp, merge_confidence,  record_id, lims_id, 
         status, date_received, test_result_cov, test_result_flu_a, test_result_flu_b, covid19_timestamp, 
         date_collected, mrn, birth_date, first_name, last_name, 
         county, state, zip, 
         orderer, submitter_name, submitter_phone) %>% 
  mutate_at(vars(merge_timestamp, date_collected, birth_date, date_received, covid19_timestamp), 
            ~as.character(.)) %>% 
  replace_na(list(merge_timestamp = "", merge_confidence = "", record_id = "", lims_id = "", status = "", date_received = "", 
                  test_result_cov = "",  test_result_flu_a = "",  test_result_flu_b = "",  covid19_timestamp = "[not completed]",
                  date_collected = "", mrn = "", birth_date = "", mrn = "", first_name = "", last_name = "", 
                  county = "", state = "", zip = "", 
                  orderer = "", submitter_name = "", submitter_phone = ""))  %>% 
  mutate(covid19_timestamp = str_replace(covid19_timestamp, "NA", "[not completed]")) %>% 
  mutate(record_id = str_replace_all(record_id, c("^500000" = "WY", "^100000" = "WMCI"))) 
newmatchs_colnames <- read_csv("Data/Reference/emaster_colnames.csv",
                               col_types = cols(.default = "c"))
names(newmatchs1) = newmatchs_colnames$Names_tidy[match(names(newmatchs1), newmatchs_colnames$Names_raw)]
write.csv(newmatchs1, file = "Data/1.csv", row.names = F) 
rm(newmatchs_colnames)


##system("TASKKILL /FI \"WINDOWTITLE eq 1 - Excel\" /f")##