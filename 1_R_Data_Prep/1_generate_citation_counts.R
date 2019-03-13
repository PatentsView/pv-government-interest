library(dplyr)
library(tidyr)
library(data.table)
library(chunked)

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

############################################################################
patent_full = patent
patent = patent_full %>% select(id, date, num_claims)
patent_full = patent
patent = patent_full %>% select(id, date, num_claims)
uspat = read_csv_chunkwise("uspatentcitation.tsv", chunk_size=100000, header=TRUE, sep = "\t") %>% select(patent_id, citation_id) %>%
  # join 1 - for citing patents
  left_join (patent, by = c("patent_id" = "id")) %>% rename(citing_patent_date = date) %>% 
  rename(num_times_cited_by_us_patents = num_claims) %>%
  select(patent_id, citation_id, citing_patent_date, num_times_cited_by_us_patents) %>%
  
   # join 2 for cited patents
  left_join(patent, by=c("citation_id" = "id")) %>%
  rename(cited_patent_date = date) %>%
  
  mutate(date_diff = difftime(citing_patent_date, cited_patent_date)) %>%
  filter(date_diff <= 365*5) %>%
  select(patent_id, citation_id, citing_patent_date, cited_patent_date, num_times_cited_by_us_patents) %>%
  write_csv_chunkwise("temp_5yr_citations_by_cite_subset.csv")

temp_all = read.csv("temp_5yr_citations_by_cite_subset.csv")
temp_5yr_citations_by_cite_all = temp_all %>% select(patent_id, citation_id, citing_patent_date,
                                                     cited_patent_date,  num_times_cited_by_us_patents)
                                                      
############################################################################


# patent_full = patent
# patent = patent_full %>% select(id, date, num_claims)
# uspat = read_csv_chunkwise("uspatentcitation.tsv", chunk_size=100000, header=TRUE, sep = "\t") %>% select(patent_id, citation_id) %>%
#   # join 1 - for citing patents
#   left_join (patent, by = c("patent_id" = "id")) %>% rename(citing_patent_date = date) %>% 
#   rename(num_claims_citing_patent = num_claims) %>%
#   select(patent_id, citation_id, citing_patent_date, num_claims_citing_patent) %>%
#   # join 2 for cited patents
#   left_join(patent, by=c("citation_id" = "id")) %>%
#   rename(cited_patent_date = date) %>%
#   
#   mutate(date_diff = difftime(citing_patent_date, cited_patent_date)) %>%
#   filter(date_diff <= 365*5) %>%
#   select(patent_id, citation_id, citing_patent_date, cited_patent_date, num_claims_citing_patent, num_claims) %>%
#   write_csv_chunkwise("temp_5yr_citations_by_cite_all.csv")
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
                              group_by(citation_id) %>% 
                              summarize(weighted_cites_5yrs = sum(num_times_cited_by_us_patents))

temp_5yr_citations_all_2 <- temp_5yr_citations_by_cite_all %>% 
                              group_by(citation_id) %>%
                              count(patent_id) %>% 
                              rename(num_citations_in_5yrs = n) %>%
                              select(citation_id, num_citations_in_5yrs)

temp_5yr_citations_all <- temp_5yr_citations_all_1 %>% 
                            inner_join(temp_5yr_citations_all_2, by = "citation_id")

write.csv(temp_5yr_citations_all, file = "temp_5yr_citations_all_subset.csv")
############################################################################
##  Government Interest Patents
############################################################################

  
## create citation tables for only government interest patents
## table has the id and date of both cited and citing patent ids
                            
distinct_patent_id <- patent_govintorg %>% distinct(patent_id)
temp_5yr_citations_by_cite <- temp_5yr_citations_by_cite_all %>% 
                              filter(citation_id %in% distinct_patent_id$patent_id)

write.csv(temp_5yr_citations_by_cite, file = "temp_5yr_citations_by_cite_subset.csv")

## derivative table with 5 year citation counts and weighted citation count
temp_5yr_citations_1 <- temp_5yr_citations_by_cite %>% 
                              group_by(citation_id) %>% 
                              summarize(weighted_cites_5yrs = sum(num_times_cited_by_us_patents))

temp_5yr_citations_2 <- temp_5yr_citations_by_cite %>% 
                              group_by(citation_id) %>%
                              count(patent_id) %>% 
                              rename(num_citations_in_5yrs = n) %>%
                              select(citation_id, num_citations_in_5yrs)

temp_5yr_citations <- temp_5yr_citations_1 %>% 
                              inner_join(temp_5yr_citations_2, by = "citation_id")

write.csv(temp_5yr_citations_all, file = "temp_5yr_citations_subset.csv")

