library(dplyr)
library(tidyr)

#read in table
uspatentcitation <- fread(file = "uspatentcitation.tsv", header=TRUE, sep="\t")
government_interest <- read.csv(file = "government_interest.tsv", header=TRUE, sep="\t")
patent <- fread(file = "patent.tsv", header=TRUE, sep="\t")
# file with num_times_cited_by_us_patents column
patent_counts <- fread(file = "temp_patent_counts_fac_vfinal.csv", header = TRUE, sep = ",", verbose=TRUE )

# combine patent and patent_counts together
# outer join with patent counts to get num_times_cited_by_us_patents field
patent_combined = patent_counts %>% merge(., patent, by.x = "patent_id", by.y = "id", all=TRUE) 
patent = patent_combined

## table with each government interest patent and any citations within 5 years -- for each year 1 thru 5 (changed by ska)
## patent_20180528.temp_updated_gi is the table with all the government interest and government assignee patents
distinct_patent_id <- government_interest %>% distinct(patent_id)
a <- uspatentcitation %>%  
      filter(citation_id %in% distinct_patent_id$patent_id) %>% 
      select(patent_id, citation_id)
b <- a %>% 
      left_join(patent, by =c("patent_id" = "patent_id")) %>% 
      select(citation_id, patent_id, date, num_times_cited_by_us_patents) %>% 
      rename(citing_patent_date = date)

c <- b %>% 
        left_join(patent, by = c("citation_id" = "patent_id")) %>% 
        rename(cited_patent_date = date, num_times_cited_by_us_patents = num_times_cited_by_us_patents.x) %>% 
        select(citation_id, patent_id, cited_patent_date, citing_patent_date, num_times_cited_by_us_patents)

## year 5
temp_5yr_citations_by_cite_yr5 <- c %>% 
          mutate(date_diff = difftime(citing_patent_date,cited_patent_date)) %>% 
          filter(date_diff <= 365 * 5 && date_diff >365 * 4) %>% 
          select(citation_id, patent_id, cited_patent_date, 
                 citing_patent_date, num_times_cited_by_us_patents)

temp_5yr_citations_yr5_1 <- temp_5yr_citations_by_cite_yr5 %>% 
                           group_by(citation_id) %>% 
                           count(patent_id) %>% 
                           rename(patent_id_orig = patent_id, num_citations_5 = n, patent_id = citation_id) %>% 
                           select(patent_id, num_citations_5)

temp_5yr_citations_yr5_2 <- temp_5yr_citations_by_cite_yr5 %>% 
                                group_by(citation_id) %>% 
                                summarize(weighted_cites_5yrs = sum(num_times_cited_by_us_patents)) %>% 
                                rename(patent_id = citation_id) %>% 
                                select(patent_id, weighted_cites_5yrs)

temp_5yr_citations_yr5 <- temp_5yr_citations_yr5_1 %>% 
                            inner_join(temp_5yr_citations_yr5_2, by = "patent_id")

## year 4
temp_5yr_citations_by_cite_yr4 <- c %>% 
  mutate(date_diff = difftime(citing_patent_date,cited_patent_date)) %>% 
  filter(date_diff <= 365 * 4 && date_diff >365 * 3) %>% 
  select(citation_id, patent_id, cited_patent_date, 
         citing_patent_date, num_times_cited_by_us_patents)

temp_5yr_citations_yr4_1 <- temp_5yr_citations_by_cite_yr4 %>% 
  group_by(citation_id) %>% 
  count(patent_id) %>% 
  rename(patent_id_orig = patent_id, num_citations_4 = n, patent_id = citation_id) %>% 
  select(patent_id, num_citations_4)

temp_5yr_citations_yr4_2 <- temp_5yr_citations_by_cite_yr4 %>% 
  group_by(citation_id) %>% 
  summarize(weighted_cites_5yrs = sum(num_times_cited_by_us_patents)) %>% 
  rename(patent_id = citation_id) %>% 
  select(patent_id, weighted_cites_5yrs)

temp_5yr_citations_yr4 <- temp_5yr_citations_yr4_1 %>% 
  inner_join(temp_5yr_citations_yr4_2, by = "patent_id")

## year 3
temp_5yr_citations_by_cite_yr3 <- c %>% 
  mutate(date_diff = difftime(citing_patent_date,cited_patent_date)) %>% 
  filter(date_diff <= 365 * 3 && date_diff >365 * 2) %>% 
  select(citation_id, patent_id, cited_patent_date, 
         citing_patent_date, num_times_cited_by_us_patents)

temp_5yr_citations_yr3_1 <- temp_5yr_citations_by_cite_yr3 %>% 
  group_by(citation_id) %>% 
  count(patent_id) %>% 
  rename(patent_id_orig = patent_id, num_citations_3 = n, patent_id = citation_id) %>% 
  select(patent_id, num_citations_3)

temp_5yr_citations_yr3_2 <- temp_5yr_citations_by_cite_yr3 %>% 
  group_by(citation_id) %>% 
  summarize(weighted_cites_5yrs = sum(num_times_cited_by_us_patents)) %>% 
  rename(patent_id = citation_id) %>% 
  select(patent_id, weighted_cites_5yrs)

temp_5yr_citations_yr3 <- temp_5yr_citations_yr3_1 %>% 
  inner_join(temp_5yr_citations_yr3_2, by = "patent_id")

## year 2
temp_5yr_citations_by_cite_yr2 <- c %>% 
  mutate(date_diff = difftime(citing_patent_date,cited_patent_date)) %>% 
  filter(date_diff <= 365 * 2 && date_diff >365 * 1) %>% 
  select(citation_id, patent_id, cited_patent_date, 
         citing_patent_date, num_times_cited_by_us_patents)

temp_5yr_citations_yr2_1 <- temp_5yr_citations_by_cite_yr2 %>% 
  group_by(citation_id) %>% 
  count(patent_id) %>% 
  rename(patent_id_orig = patent_id, num_citations_2 = n, patent_id = citation_id) %>% 
  select(patent_id, num_citations_2)

temp_5yr_citations_yr2_2 <- temp_5yr_citations_by_cite_yr2 %>% 
  group_by(citation_id) %>% 
  summarize(weighted_cites_5yrs = sum(num_times_cited_by_us_patents)) %>% 
  rename(patent_id = citation_id) %>% 
  select(patent_id, weighted_cites_5yrs)

temp_5yr_citations_yr2 <- temp_5yr_citations_yr2_1 %>% 
  inner_join(temp_5yr_citations_yr2_2, by = "patent_id")

## year 1
temp_5yr_citations_by_cite_yr1 <- c %>% 
  mutate(date_diff = difftime(citing_patent_date,cited_patent_date)) %>% 
  filter(date_diff < 365 * 2) %>% 
  select(citation_id, patent_id, cited_patent_date, 
         citing_patent_date, num_times_cited_by_us_patents)

temp_5yr_citations_yr1_1 <- temp_5yr_citations_by_cite_yr1 %>% 
  group_by(citation_id) %>% 
  count(patent_id) %>% 
  rename(patent_id_orig = patent_id, num_citations_1 = n, patent_id = citation_id) %>% 
  select(patent_id, num_citations_1)

temp_5yr_citations_yr1_2 <- temp_5yr_citations_by_cite_yr1 %>% 
  group_by(citation_id) %>% 
  summarize(weighted_cites_5yrs = sum(num_times_cited_by_us_patents)) %>% 
  rename(patent_id = citation_id) %>% 
  select(patent_id, weighted_cites_5yrs)

temp_5yr_citations_yr1 <- temp_5yr_citations_yr1_1 %>% 
  inner_join(temp_5yr_citations_yr1_2, by = "patent_id")

write.csv(temp_5yr_citations_yr5, file = "temp_5yr_citations_yr5.csv")
write.csv(temp_5yr_citations_yr4, file = "temp_5yr_citations_yr4.csv")
write.csv(temp_5yr_citations_yr3, file = "temp_5yr_citations_yr3.csv")
write.csv(temp_5yr_citations_yr2, file = "temp_5yr_citations_yr2.csv")
write.csv(temp_5yr_citations_yr1, file = "temp_5yr_citations_yr1.csv")