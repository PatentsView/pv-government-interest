# load required packages and set folder paths
source("requirements.R")

# read in relevant data files
patent_govintorg <- fread(file = str_c(input_folder,"patent_govintorg.tsv"), header=TRUE, sep="\t")
patent_df_full <- fread(file=str_c(input_folder, "patent.tsv"), header=TRUE, sep = "\t", quote = "")

# file with num_times_cited_by_us_patents column
patent_counts <- fread(file=str_c(input_folder,"temp_patent_counts_fac_vfinal.csv"), header = TRUE, sep = ",")

#For each patent, create the 5 year citation counts and weighted citation counts
#Uses only the government relationships in the government interest table (not government assignees)
#Does not require any other new tables to be pre-generated

############################################################################
##  All Patents
############################################################################

############################################################################

# outer join with patent counts to get num_times_cited_by_us_patents field, add year column
patent_combined = patent_df_full %>% merge(., patent_counts, by.x = "id", by.y = "patent_id", all=TRUE) %>%
    mutate(year = year(date))


fwrite(patent_combined, str_c(output_folder, "temp_patent_counts_patent_merged.csv"))

patent = patent_combined %>% select(id, date, num_times_cited_by_us_patents)

read_csv_chunkwise(str_c(input_folder,"uspatentcitation.tsv"), chunk_size=500000, header=TRUE, sep = "\t") %>% 
  
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
  rename(num_times_cited_by_us_patents = num_times_cited_by_us_patents.x) %>% 
  
  write_csv_chunkwise(str_c(output_folder, "temp_5yr_citations_by_cite.csv"))


temp_5yr_citations_by_cite_all = fread(str_c(input_folder, "temp_5yr_citations_by_cite.csv"), sep = ",",na.strings=getOption("datatable.na.strings","NA"), verbose=TRUE)

## derivative table with 5 year citation counts and weighted citation counts
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

fwrite(temp_5yr_citations_all, file = str_c(output_folder, "temp_5yr_citations_all.csv"), sep = ",")
############################################################################
##  Government Interest Patents
############################################################################

## create citation tables for only government interest patents
## table has the id and date of both cited and citing patent ids
                            
distinct_patent_id <- patent_govintorg %>% distinct(patent_id)
temp_5yr_citations_by_cite <- temp_5yr_citations_by_cite_all %>% 
                              filter(citation_id %in% distinct_patent_id$patent_id)

fwrite(temp_5yr_citations_by_cite, file = str_c(output_folder, "temp_5yr_citations_by_cite_gi.csv"), sep=",")

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

write.csv(temp_5yr_citations, file = str_c(output_folder, "temp_5yr_citations_gi.csv", sep = ""))

