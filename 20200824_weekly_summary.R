`%notin%` <- Negate(`%in%`)
# Load wLIMS ---------------------------------------------------------------
wLIMS <- 
  read_csv("Data/wLIMS.csv", 
           col_types = cols(.default = "c")) %>% 
  filter(!is.na(date_received)) %>%
  filter(!(last_name == "wastewater" & first_name == "wastewater" & is.na(birth_date))) %>% 
  filter(!(last_name %in% c("pt","verification") & birth_date == "2001-01-01")) %>% 
  mutate_at(vars(date_collected, birth_date, date_approved, date_received), 
            ~as.Date(.)) %>% 
  mutate(turnaround_time = as.double(turnaround_time)) 

# Load Master -------------------------------------------------------------
tmaster <- read_csv("Data/tmaster.csv", 
                    col_types = cols(.default = "c")) %>% 
  filter(!(last_name == "WASTEWATER" & first_name == "WASTEWATER" & is.na(birth_date))) %>% 
  filter(!(last_name %in% c("PT","VERIFICATION") & birth_date == "2001-01-01")) %>% 
  mutate_at(vars(date_collected, birth_date, date_received), 
            ~as.Date(.))

# Weekly ---------------------------------------------------------------
receivedLIMS <- wLIMS %>% 
  filter(between(date_received, as_date(today()-days(7)),as_date(today()-days(1))))

#Weekly counts
weekly_received <- receivedLIMS %>% 
  count(date_received) %>% 
  mutate(test_result_cov = "RECEIVED")
weekly_results <- receivedLIMS %>% 
  filter(!(is.na(last_name) & is.na(first_name) & is.na(birth_date))) %>% 
  count(date_received, test_result_cov) %>%  
  replace(is.na(.),"UNTESTED") 
weekly_rerun <- receivedLIMS %>% 
  count(date_received, run_2) %>% 
  filter(!is.na(run_2)) %>% 
  count(date_received, n) %>% 
  mutate(n = str_replace(n, "1", "RERUN")) %>% 
  replace_na(list(n = "RERUN")) %>% 
  rename(test_result_cov = n) %>% 
  rename(n = nn)
weekly_pending <- receivedLIMS %>% 
  select(date_received, turnaround_time) %>% 
  mutate(turnaround_time = case_when(
    turnaround_time > "0" ~ "PENDING")) %>% 
  count(date_received, turnaround_time) %>% 
  filter(!is.na(turnaround_time)) %>% 
  rename(test_result_cov = turnaround_time) 
  
weekly_summary <- 
  bind_rows(weekly_results, weekly_rerun, weekly_received, weekly_pending) %>% 
  pad(interval = "day", start_val = today()-days(7),end_val = today()-days(1), group = "test_result_cov") %>% 
  replace_na(list(n = 0)) %>% 
  mutate_if(is.character, ~toupper(.)) %>% 
  pivot_wider(id_cols = `date_received`,
              names_from = `test_result_cov`,
              values_from = `n`) %>% 
 select(date_received, RECEIVED, `SAMPLE NOT TESTED`, `SARS-COV2 POSITIVE`, `SARS-COV2 NEGATIVE`, RERUN, PENDING, everything()) %>% 
  rename("DATE RECEIVED" = date_received) 

weekly_released <- receivedLIMS %>% 
  count(date_approved, date_received)

weekly_test_used <- receivedLIMS %>% 
  count(date_approved, test_name)

weekly_testresults <- count(receivedLIMS, date_received, test_result_cov) %>%  
  replace(is.na(.),"UNRUN") %>% 
  pad(interval = "day",end_val = today(), group = "test_result_cov") %>% 
  replace_na(list(n = 0)) %>% 
  arrange(desc(test_result_cov))

weekly_newpositive <- tmaster %>% 
  filter(test_result_cov == "SARS-COV2 DETECTED") %>% 
  arrange(covid_id, date_collected) %>% 
  distinct(covid_id, .keep_all = T) %>% 
  filter(between(date_received, as_date(today()-days(7)),as_date(today()-days(1)))) %>%      
  count(date_received, test_result_cov)

weekly_counties <-  tmaster %>% 
  filter(between(date_received, as_date(today()-days(7)),as_date(today()-days(1)))) %>%     
  filter(test_result_cov == "SARS-COV2 DETECTED") %>% 
  count(county)

weekly_counties_new <-  tmaster %>% 
  filter(test_result_cov == "SARS-COV2 DETECTED") %>% 
  arrange(covid_id, date_collected) %>% 
  distinct(covid_id, .keep_all = T) %>% 
  filter(between(date_received, as_date(today()-days(7)),as_date(today()-days(1)))) %>%      
  count(county)

#Weekly list of results
list_positive <- receivedLIMS %>% 
  filter(test_result_cov == "SARS-COV2 DETECTED")

list_newpositive <- tmaster %>% 
  filter(test_result_cov == "SARS-COV2 DETECTED") %>% 
  arrange(covid_id, date_collected) %>% 
  distinct(covid_id, .keep_all = T) %>% 
  filter(between(date_received, as_date(today()-days(7)),as_date(today()-days(1))))   

list_inconclusive <- receivedLIMS %>% 
  filter(test_result_cov == "SARS-COV2 INCONCLUSIVE")

list_invalid <- receivedLIMS %>% 
  filter(test_result_cov == "INVALID SAMPLE")

list_rejected <- receivedLIMS %>% 
  filter(test_result_cov == "SAMPLE NOT TESTED")

list_rerun <- receivedLIMS %>% 
  filter(!is.na(`run_2`)|!is.na(`run_3`)|!is.na(`run_4`)|!is.na(`run_5`)|!is.na(`run_6`))

# Cumulative --------------------------------------------------------------
cumulative_released <- wLIMS %>% 
  filter(!is.na(date_received)) %>%
  count(`status`) %>%  
  replace(is.na(.),"unrun")

cumulative_testresults <- wLIMS %>% 
  filter(!is.na(date_received)) %>%
  count(`test_result_cov`)%>%  
  replace(is.na(.),"UNRUN") %>% 
  slice(5,3,1,2,4,6)

cumulative_turnaround <- wLIMS %>% 
  filter(!is.na(turnaround_time)) %>% 
  count(turnaround_time) %>% 
  mutate(percent = round(n/sum(n)*100,1))

# Graphs ------------------------------------------------------------------
z1 <- wLIMS %>% 
  filter(!is.na(date_received)) %>%
  count(date_received) %>% 
  mutate(sevendayavg = rollmean(n, 7, fill = 0, align = "right")) %>% 
  mutate(cum_total = cumsum(n))
z1.1 <- z1 %>% 
  filter(between(date_received, as_date(today()-days(7)),as_date(today())))

z2 <- wLIMS %>% 
  filter(!is.na(date_received)) %>%
  count(date_received, date_approved) %>% 
  mutate(turnaround = as.duration(date_received %--% date_approved)/ddays(1)) %>% 
  mutate(date_diff = case_when(
    turnaround >= 3 ~ "3+",
    turnaround == 2 ~ "2",
    turnaround == 1 ~ "1",
    turnaround == 0 ~ "0",
    is.na(turnaround) ~ "Unreleased"))
z2.1 <- z2 %>% 
  filter((date_received >= as_date(today()-days(7))))

z3 <- wLIMS %>% 
  filter(!is.na(date_received)) %>%
  count(date_received, test_result_cov) %>% 
  filter(date_received <= today()) %>% 
  pad(interval = "day", start_val = as.POSIXct('2020-03-04', tz = 'MST'),end_val = today(), group = "test_result_cov") %>% 
  replace_na(list(n = 0)) %>% 
  group_by(test_result_cov) %>% 
  mutate(sevendayavg = case_when(
    test_result_cov == "SARS-COV2 INCONCLUSIVE"~rollmean(n, 7, fill = 0, align = "right"),
    test_result_cov == "INVALID SAMPLE"~rollmean(n, 7, fill = 0, align = "right"),
    test_result_cov == "SARS-COV2 NOT DETECTED"~rollmean(n, 7, fill = 0, align = "right"),
    test_result_cov == "SAMPLE NOT TESTED"~rollmean(n, 7, fill = 0, align = "right"),
    test_result_cov == "SARS-COV2 DETECTED"~rollmean(n, 7, fill = 0, align = "right"))) %>% 
  mutate(cum_result = case_when(
    test_result_cov == "SARS-COV2 INCONCLUSIVE" ~ cumsum(n),
    test_result_cov == "INVALID SAMPLE" ~ cumsum(n),
    test_result_cov == "SARS-COV2 NOT DETECTED" ~ cumsum(n),
    test_result_cov == "SAMPLE NOT TESTED" ~ cumsum(n),
    test_result_cov == "SARS-COV2 DETECTED" ~ cumsum(n))) %>% 
  arrange(desc(test_result_cov))
z3.1 <- z3 %>% 
  filter((date_received >= as_date(today()-days(7))))

z4 <- tmaster %>% 
  filter(test_result_cov == "SARS-COV2 DETECTED") %>% 
  count(date_received, county)%>% 
  filter(date_received < today())
z4 <- z4 %>% 
  mutate(date_received = as_date(date_received)) %>% 
  pad(interval = "day", start_val = as.POSIXct('2020-03-04', tz = 'MST'), end_val = today(), group = "county") %>% 
  replace_na(list(n = 0)) %>% 
  group_by(county) %>% 
  mutate(cum_county = cumsum(n))
z4.1 <- z4 %>% 
  filter((date_received >= as_date(today()-days(7))))

graph_rollmean <- ggplot(z1, aes(x=date_received, y=c(n, sevendayavg))) + 
  geom_col(aes(x=date_received, y=n)) +
  geom_line(aes(y=sevendayavg)) +  
  labs(x = "Date", y = "Samples Received") + 
  theme_bw()
graph_rollmean

graph_turnaround <- ggplot(z2, aes(x=date_received, y=n)) + 
  geom_col(aes(x=date_received, y=n, fill = date_diff),position = position_stack(reverse = T)) + 
  labs(x = "Date", y = "Samples Received") + 
  theme_bw() 
graph_turnaround

graph_results <- ggplot(z3, aes(x=date_received, y= c(n, sevendayavg))) + 
  geom_col(aes(x=date_received, y=n, fill = test_result_cov), position = position_stack(reverse = T)) + 
  geom_line(aes(y=sevendayavg, color = test_result_cov)) + 
  labs(x = "Date", y = "Sample Result") + 
  theme_bw() 
graph_results

graph_cum <- ggplot(z3, aes(x=date_received, y= c(n, cum_result))) + 
  geom_col(aes(x=date_received, y=cum_result, fill = test_result_cov), position = position_stack(reverse = T)) + 
  labs(x = "Date", y = "Sample Result") + 
  theme_bw() 
graph_cum

graph_pos <- ggplot(z4, aes(x=date_received, y= c(n, county))) + 
  geom_col(aes(y=n, color = county)) + 
  facet_wrap(vars(county)) + 
  theme_bw() 
graph_pos

#Week graphs
wk_graph_rollmean <- ggplot(z1.1, aes(x=date_received, y=c(n, sevendayavg))) + 
  geom_col(aes(x=date_received, y=n)) +
  geom_line(aes(y=sevendayavg)) +  
  labs(x = "Date", y = "Samples Received") + 
  theme_bw()
wk_graph_rollmean

wk_graph_turnaround <- ggplot(z2.1, aes(x=date_received, y=n)) + 
  geom_col(aes(x=date_received, y=n, fill = date_diff),position = position_stack(reverse = T)) + 
  labs(x = "Date", y = "Samples Received") + 
  theme_bw() 
wk_graph_turnaround

wk_graph_results <- ggplot(z3.1, aes(x=date_received, y= c(n, sevendayavg))) + 
  geom_col(aes(x=date_received, y=n, fill = test_result_cov), position = position_stack(reverse = F)) +
  labs(x = "Date", y = "Sample Result") + 
  theme_bw() 
wk_graph_results

wk_graph_cum <- ggplot(z3.1, aes(x=date_received, y= c(n, cum_result))) + 
  geom_col(aes(x=date_received, y=cum_result, fill = test_result_cov), position = position_stack(reverse = F)) + 
  labs(x = "Date", y = "Sample Result") + 
  theme_bw() 
wk_graph_cum

wk_graph_pos <- ggplot(z4.1, aes(x=date_received, y= c(n, county))) + 
  geom_col(aes(y=n, color = county)) + facet_wrap(vars(county)) +
  theme_bw() 
wk_graph_pos


#working
#w <- map_data("county", "wyoming")%>% 
#select(lon = long, lat, group, id = subregion)

#w <- ggplot(w, aes(lon, lat, group = group)) +
  geom_polygon(fill = "white", colour = "grey50") + 
  coord_quickmap()
#w

#y <- map_data()
##How to add data on top? +geom_point(data = z4)

