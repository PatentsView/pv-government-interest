library(dplyr)
library(tidyr)
library(data.table)
library(chunked)

# read in relevant data
uspatentcitation <- fread(file = "uspatentcitation.tsv", header=TRUE, sep="\t", quote="")
patent <- fread(file = "patent.tsv", header=TRUE, sep="\t", quote = "")
patent_govintorg <- fread(file = "patent_govintorg.tsv", header=TRUE, sep="\t")

# file with num_times_cited_by_us_patents column
patent_counts <- fread(file="temp_patents_counts.csv", header = TRUE, sep = ",")
#For each patent, create the 5 year citation counts and weighted citation counts
#Uses only the government relationships in the government interest table (not government assignees)
#Does not require any other new tables to be pre-generated

############################################################################
##  All Patents
############################################################################

############################################################################
patent_df_full = patent
patent = patent_df_full %>% select(id, date)

# outer join with patent counts to get num_times_cited_by_us_patents field
patent_combined = patent_counts %>% select(patent_id, num_us_patents_cited) %>% merge(., patent, by.x = "patent_id", by.y = "id", all=TRUE) %>%
  rename(num_times_cited_by_us_patents = num_us_patents_cited) %>% rename(id = patent_id)

patent_before_merge = patent
patent = patent_combined

temp_5yr_citations_by_cite_all = #read_csv_chunkwise("uspatentcitation.tsv", chunk_size=500000, header=TRUE, sep = "\t") %>% 
   uspatentcitation %>%
  select(patent_id, citation_id) %>%
  
  # join 1 - for citing patents
  left_join (patent, by = c("patent_id" = "id")) %>% rename(citing_patent_date = date) %>% 
   select(patent_id, citation_id, citing_patent_date, num_times_cited_by_us_patents) %>%
  
   # join 2 for cited patents
  left_join(patent, by=c("citation_id" = "id")) %>%
   rename(cited_patent_date = date) %>%
  
  mutate(date_diff = difftime(citing_patent_date, cited_patent_date)) %>%
  filter(date_diff <= 365*5) %>%
  select(patent_id, citation_id, citing_patent_date, cited_patent_date, num_times_cited_by_us_patents.x) %>%
  rename(num_times_cited_by_us_patents = num_times_cited_by_us_patents.x) #%>% write_csv_chunkwise("temp_5yr_citations_by_cite_masterv2.csv")

#write.csv(temp_5yr_citations_by_cite_all, "temp_5yr_citations_by_cite_subset.csv")
write.csv(temp_5yr_citations_by_cite_all, "temp_5yr_citations_by_cite_all.csv")


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

write.csv(temp_5yr_citations, file = "temp_5yr_citations.csv")

