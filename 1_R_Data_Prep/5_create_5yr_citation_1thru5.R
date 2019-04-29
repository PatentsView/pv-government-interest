source("requirements.R")


#read in table
uspatentcitation <- fread(file = str_c(input_folder,"uspatentcitation.tsv"), header=TRUE, sep="\t", quote = "")
government_interest <- read.csv(file = str_c(input_folder, "government_interest.tsv"), header=TRUE, sep="\t")

# patent counts and patent merged table
patent = fread(file=str_c(input_folder,"temp_patent_counts_patent_merged.csv"), header=TRUE, sep=",", verbose=TRUE)

patent = patent %>% rename(patent_id = id)

## table with each government interest patent and any citations within 5 years -- for each year 1 thru 5 (changed by ska)
## patent_20180528.temp_updated_gi is the table with all the government interest and government assignee patents
distinct_patent_id <- government_interest %>% distinct(patent_id)
a <- uspatentcitation %>%  
      filter(citation_id %in% distinct_patent_id$patent_id) %>% 
      select(patent_id, citation_id)
b <- a %>% 
      left_join(patent, by=c("patent_id" = "patent_id")) %>% 
      select(citation_id, patent_id, date, num_times_cited_by_us_patents) %>% 
      rename(citing_patent_date = date)

c <- b %>% 
        left_join(patent, by = c("citation_id" = "patent_id")) %>% 
        rename(cited_patent_date = date, num_times_cited_by_us_patents = num_times_cited_by_us_patents.x) %>% 
        select(citation_id, patent_id, cited_patent_date, citing_patent_date, num_times_cited_by_us_patents)

fwrite(c, file = str_c(output_folder,"temp_joined_gi_uspatcit_pat.csv"))

## year 5
temp_5yr_citations_by_cite_yr5 <- c %>% 
          mutate(date_diff = as.integer(difftime(citing_patent_date,cited_patent_date))) %>% 
          filter(date_diff <= 365 * 5 & date_diff >365 * 4) %>% 
          select(citation_id, patent_id, cited_patent_date, 
                 citing_patent_date, num_times_cited_by_us_patents)

temp_5yr_citations_yr5_1 <- temp_5yr_citations_by_cite_yr5 %>% 
                           group_by(citation_id) %>% 
                          summarize(num_citations_5 = n()) %>%
                           rename(patent_id = citation_id)

temp_5yr_citations_yr5_2 <- temp_5yr_citations_by_cite_yr5 %>% 
                                group_by(citation_id) %>% 
                                summarize(weighted_cites_5yrs = sum(num_times_cited_by_us_patents, na.rm = TRUE)) %>% 
                                rename(patent_id = citation_id)

temp_5yr_citations_yr5 <- temp_5yr_citations_yr5_1 %>% 
                            inner_join(temp_5yr_citations_yr5_2, by = "patent_id")

## year 4
temp_5yr_citations_by_cite_yr4 <- c %>% 
  mutate(date_diff = as.integer(difftime(citing_patent_date,cited_patent_date))) %>% 
  filter(date_diff <= 365 * 4 & date_diff > 365 * 3) %>% 
  select(citation_id, patent_id, cited_patent_date, 
         citing_patent_date, num_times_cited_by_us_patents)

temp_5yr_citations_yr4_1 <- temp_5yr_citations_by_cite_yr4 %>% 
  group_by(citation_id) %>% 
  summarize(num_citations_4 = n()) %>%
  rename(patent_id = citation_id)

temp_5yr_citations_yr4_2 <- temp_5yr_citations_by_cite_yr4 %>% 
  group_by(citation_id) %>% 
  summarize(weighted_cites_5yrs = sum(num_times_cited_by_us_patents, na.rm = TRUE)) %>% 
  rename(patent_id = citation_id)

temp_5yr_citations_yr4 <- temp_5yr_citations_yr4_1 %>% 
  inner_join(temp_5yr_citations_yr4_2, by = "patent_id")

## year 3
temp_5yr_citations_by_cite_yr3 <- c %>% 
  mutate(date_diff = as.integer(difftime(citing_patent_date,cited_patent_date))) %>% 
  filter(date_diff <= 365 * 3 & date_diff > 365 * 2) %>% 
  select(citation_id, patent_id, cited_patent_date, 
         citing_patent_date, num_times_cited_by_us_patents)

temp_5yr_citations_yr3_1 <- temp_5yr_citations_by_cite_yr3 %>% 
  group_by(citation_id) %>% 
  summarize(num_citations_3 = n()) %>% 
  rename(patent_id = citation_id)

temp_5yr_citations_yr3_2 <- temp_5yr_citations_by_cite_yr3 %>% 
  group_by(citation_id) %>% 
  summarize(weighted_cites_5yrs = sum(num_times_cited_by_us_patents, na.rm = TRUE)) %>% 
  rename(patent_id = citation_id)

temp_5yr_citations_yr3 <- temp_5yr_citations_yr3_1 %>% 
  inner_join(temp_5yr_citations_yr3_2, by = "patent_id")

## year 2
temp_5yr_citations_by_cite_yr2 <- c %>% 
  mutate(date_diff = as.integer(difftime(citing_patent_date,cited_patent_date))) %>% 
  filter(date_diff <= 365 * 2 & date_diff >365 * 1) %>% 
  select(citation_id, patent_id, cited_patent_date, 
         citing_patent_date, num_times_cited_by_us_patents)

temp_5yr_citations_yr2_1 <- temp_5yr_citations_by_cite_yr2 %>% 
  group_by(citation_id) %>% 
  summarize(num_citations_2 = n()) %>% 
  rename(patent_id = citation_id)

temp_5yr_citations_yr2_2 <- temp_5yr_citations_by_cite_yr2 %>% 
  group_by(citation_id) %>% 
  summarize(weighted_cites_5yrs = sum(num_times_cited_by_us_patents, na.rm=TRUE)) %>% 
  rename(patent_id = citation_id)

temp_5yr_citations_yr2 <- temp_5yr_citations_yr2_1 %>% 
  inner_join(temp_5yr_citations_yr2_2, by = "patent_id")

## year 1
temp_5yr_citations_by_cite_yr1 <- c %>% 
  mutate(date_diff = as.integer(difftime(citing_patent_date,cited_patent_date))) %>% 
  filter(date_diff < 365 * 2) %>% 
  select(citation_id, patent_id, cited_patent_date, 
         citing_patent_date, num_times_cited_by_us_patents)

temp_5yr_citations_yr1_1 <- temp_5yr_citations_by_cite_yr1 %>% 
  group_by(citation_id) %>% 
  summarize(num_citations_1 = n()) %>% 
  rename(patent_id = citation_id)

temp_5yr_citations_yr1_2 <- temp_5yr_citations_by_cite_yr1 %>% 
  group_by(citation_id) %>% 
  summarize(weighted_cites_5yrs = sum(num_times_cited_by_us_patents, na.rm=TRUE)) %>% 
  rename(patent_id = citation_id) %>% 
  select(patent_id, weighted_cites_5yrs)

temp_5yr_citations_yr1 <- temp_5yr_citations_yr1_1 %>% 
  inner_join(temp_5yr_citations_yr1_2, by = "patent_id")

write.csv(temp_5yr_citations_yr5, file = str_c(output_folder,"temp_5yr_citations_yr5.csv"))
write.csv(temp_5yr_citations_yr4, file = str_c(output_folder,"temp_5yr_citations_yr4.csv"))
write.csv(temp_5yr_citations_yr3, file = str_c(output_folder,"temp_5yr_citations_yr3.csv"))
write.csv(temp_5yr_citations_yr2, file = str_c(output_folder,"temp_5yr_citations_yr2.csv"))
write.csv(temp_5yr_citations_yr1, file = str_c(output_folder,"temp_5yr_citations_yr1.csv"))