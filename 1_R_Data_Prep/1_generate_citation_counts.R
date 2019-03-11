library(dplyr)
library(tidyr)
library(data.table)
# read in relevant data
uspatentcitation <- fread(file = "uspatentcitation.tsv", header=TRUE, sep="\t", quote="")
patent <- fread(file = "patent.tsv", header=TRUE, sep="\t", quote = "")
patent_govintorg <- fread(file = "patent_govintorg.tsv", header=TRUE, sep="\t")

#For each patent, create the 5 year citation counts and weighted citation counts
#Uses only the government relationships in the government interest table (not government assignees)
#Does not require any other new tables to be pre-generated

############################################################################
##  All Patents
############################################################################
  
## table with all patents and any citations within 5 years
## table has the id and date of both cited and citing patent ids
uspat_subset = uspatentcitation %>% select(patent_id, citation_id, date) %>%
  rename(citing_patent_date = date)

temp_5yr_citations_by_cite_all <-  left_join(uspat_subset, patent, by = c("patent_id" = "id"))
  
  
      left_join(patent, by=(c("patent_id" = "id"))) %>% 
      select(patent_id, citation_id, date, num_claims) %>% 
      rename(citing_patent_date = date) %>%  
      left_join(patent, by=(c("patent_id" = "patent_id"))) %>% 
      select(patent_id, citation, citing_patent_date, num_claims.x, date) %>% 
      rename(cited_patent_date = date, num_claims = num_claims.x) %>% 
      mutate(date_diff = difftime(citing_patent_date, citation_date)) %>% 
      filter(date_diff <= 365*5) %>% 
      select(patent_id, citation_id, citing_patent_date, cited_patent_date, num_claims)

write.csv(temp_5yr_citations_by_cite_all, file = "temp_5yr_citations_by_cite_all.csv")



## derivative table with 5 year citation counts and weighted citation count
temp_5yr_citations_all_1 <- temp_5yr_citations_by_cite_all %>% 
                              group_by(cited_patent_id) %>% 
                              summarize(weighted_cites_5yrs = sum(num_times_cited_by_us_patents))

temp_5yr_citations_all_2 <- temp_5yr_citations_by_cite_all %>% 
                              group_by(cited_patent_id) %>%
                              count(citing_patent_id) %>% 
                              rename(num_citations_in_5yrs = n) %>%
                              select(cited_patent_id, num_citations_in_5yrs)

temp_5yr_citations_all <- temp_5yr_citations_all_1 %>% 
                            inner_join(temp_5yr_citations_all_2, by = "cited_patent_id")

write.csv(temp_5yr_citations_all, file = "temp_5yr_citations_all.csv")
############################################################################
##  Government Interest Patents
############################################################################

  
## create citation tables for only government interest patents
## table has the id and date of both cited and citing patent ids
                            
distinct_patent_id <- patent_govintorg %>% distinct(patent_id)
temp_5yr_citations_by_cite <- temp_5yr_citations_by_cite_all %>% 
                              filter(cited_patent_id %in% distinct_patent_id$patent_id)

write.csv(temp_5yr_citations_by_cite, file = "temp_5yr_citations_by_cite.csv")

## derivative table with 5 year citation counts and weighted citation count
temp_5yr_citations_1 <- temp_5yr_citations_by_cite %>% 
                              group_by(cited_patent_id) %>% 
                              summarize(weighted_cites_5yrs = sum(num_times_cited_by_us_patents))

temp_5yr_citations_2 <- temp_5yr_citations_by_cite %>% 
                              group_by(cited_patent_id) %>%
                              count(citing_patent_id) %>% 
                              rename(num_citations_in_5yrs = n) %>%
                              select(cited_patent_id, num_citations_in_5yrs)

temp_5yr_citations <- temp_5yr_citations_1 %>% 
                              inner_join(temp_5yr_citations_2, by = "cited_patent_id")

write.csv(temp_5yr_citations_all, file = "temp_5yr_citations.csv")


###############################################
#"government_interest_patents_1980-2018_returned.csv"
govint = read.csv("government_interest.tsv", sep="\t")
patgovint_ret <- merge(patent, govint, by.x = "id", by.y= "patent_id")
patgovint_ret_subset <- patgovint_ret[,c(1,3,5,12)]
patgovint_ret_subset <- patgovint_ret_subset %>% as_tibble()
patgovint_ret_subset$date <- patgovint_ret_subset$date %>% as.Date()

patgovint_ret_subset_final <- patgovint_ret_subset %>% filter(year(date) >= 1980) %>% select(id)
write.csv(patgovint_ret_subset_final, "government_interest_patents_1980-2018_returned.csv")
