# Load wLIMS ---------------------------------------------------------------
wLIMS <- 
  read_csv("Data/wLIMS.csv", 
           col_types = cols(.default = "c")) %>% 
  filter(!is.na(date_received)) %>%
  filter(!(last_name == "wastewater" & first_name == "wastewater" & is.na(birth_date))) %>% 
  filter(!(last_name %in% c("pt","verification") & birth_date == "2001-01-01")) %>% 
  mutate_at(vars(date_collected, birth_date, date_approved, date_received), 
            ~as.Date(.))

# Load tLWP ---------------------------------------------------------------
tLWP <- 
  read_csv("Data/tLWP.csv", 
           col_types = cols(.default = "c")) %>% 
  mutate_at(vars(date_collected, dob, date_received), 
            ~as.Date(.))

# Load Master -------------------------------------------------------------
tmaster <- read_csv("Data/tmaster.csv", 
                    col_types = cols(.default = "c")) %>% 
  filter(!(last_name == "WASTEWATER" & first_name == "WASTEWATER" & is.na(birth_date))) %>% 
  filter(!(last_name %in% c("PT","VERIFICATION") & birth_date == "2001-01-01")) 

# Specimen Tracking -------------------------------------------------------
# Total data
t1 <- wLIMS %>% 
  filter(!test_result_cov == "sample not tested" | is.na(test_result_cov)) %>% 
  count(status) %>% 
  replace(is.na(.),"In-house Untested") %>% 
  mutate(status = str_replace(status, "released", "Total Completed Tests"))

t2 <- wLIMS %>% 
  filter(date_received >= as_date(today()-days(1))) %>% 
  transmute(status = paste("Received", date_received)) %>% 
  count(status) 

spec_track_total <- bind_rows(t1,t2)
rm(t1, t2)

#Lab Web Portal
spec_track_lwp <- tLWP %>% 
  filter(!(str_detect(status, "released"))) %>% 
  count(status) %>% 
  slice(2,3,1)

#REDCap
r1 <- tmaster %>% 
  filter(is.na(status) & is.na(lims_id)) %>% 
  mutate(date_collected = as_date(date_collected)) %>% 
  filter(is.na(date_collected) | between(date_collected, as_date(today()-days(3)), as_date(today()))) %>% 
  count(status) %>% 
  replace(is.na(.),"In transit") 

r2 <- tmaster %>% 
  filter(is.na(status) & !is.na(lims_id) & !is.na(record_id)) %>% 
  distinct(lims_id, .keep_all = T) %>% 
  count(status) %>% 
  replace(is.na(.),"Received/In testing process") 

spec_track_redcap <- bind_rows(r1,r2)
rm(r1, r2)

#Detailed Information
test_results <- wLIMS %>% 
  filter(status == "released") %>% 
  count(test_result_cov) %>% 
  mutate(total = sum(n))

in_transit <- tmaster %>% 
  filter(is.na(date_received)) %>% 
  filter(status == "IN TRANSIT" | is.na(status)) %>% 
  mutate(date_collected = as_date(date_collected)) %>% 
  filter(is.na(date_collected) | between(date_collected, as_date(today()-days(3)), as_date(today()))) %>% 
  count(submitter_name) %>% 
  arrange(desc(n)) %>% 
  mutate(total = sum(n))

# Follow-up ---------------------------------------------------------------
#Follow-up required
follow_up <- wLIMS %>% 
  filter(is.na(date_approved)) %>% 
  filter(date_received <= as_date(today()-days(1))) %>% 
  select(lims_id, date_received, status,            test_result_cov,  test_result_flu_a,  test_result_flu_b,  date_collected, 
         mrn,     birth_date,    last_name,         first_name,   specimen_source,
         county,  city,          submitter_name,    lims_submitter_id) 
write.csv(follow_up, file = "Data/follow_up.csv", row.names = F) 
