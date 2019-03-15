library(dplyr)
library(tidyr)
library(data.table)

#read in table
patent_inventor <- fread(file = "patent_inventor.tsv", header=TRUE, sep="\t")
patent_assignee <- fread(file = "patent_assignee.tsv", header=TRUE, sep="\t")
nber <- fread(file = "nber.tsv", header=TRUE, sep="\t")
wipo <- fread(file = "wipo.tsv", header=TRUE, sep="\t")
wipo_field <- read.csv(file = "wipo_field.tsv", header=TRUE, sep="\t")

patent <- fread(file = "patent.tsv", header=TRUE, sep="\t", col.names = c("id", "type", "number", "country", 
                              "date", "abstract", "title", "kind", "num_claims", "filename", "withdrawn"))
temp_5yr_citations <- read.csv(file = "temp_5yr_citations.csv", header=TRUE, sep=",")
patent_govintorg <- fread(file = "patent_govintorg.tsv", header=TRUE, sep="\t")
government_organization <- read.csv(file = "government_organization.tsv", header=TRUE, sep="\t")

# file with num_times_cited_by_us_patents column
patent_counts <- fread(file = "temp_patent_counts_fac_vfinal.csv", header = TRUE, sep = ",", verbose=TRUE )

## Create the main Patent Level and Government Interest Level tables
## These tables have a lot of details around the patents including information from the database
## and  also from the citation count tables generated in the previous steps

############################################################################
##  All Patents
############################################################################
c <- patent_inventor %>% 
        group_by(patent_id) %>% 
        count(inventor_id) %>% 
        rename(num_inventors = n) %>% 
        select(patent_id,inventor_id, num_inventors)

d <- patent_assignee %>% 
        group_by(patent_id) %>% 
        count(assignee_id) %>% 
        rename(num_assignees = n) %>% 
        select(patent_id, num_assignees)

e <- c %>% left_join(d, by = "patent_id")

w <- wipo %>% 
      filter(sequence == 0) %>% 
      right_join(e, by = "patent_id") %>% 
      rename(id = field_id)

wipo_field$id = wipo_field$id %>% as.character()

wf <- w %>% 
          left_join(wipo_field, by = "id") %>% 
          rename(wipo_sector = sector_title, wipo_field = field_title)

# combine patent and patent_counts together
# outer join with patent counts to get num_times_cited_by_us_patents field
patent_combined = patent_counts %>% merge(., patent, by.x = "patent_id", by.y = "id", all=TRUE) 
  

p <- wf %>% 
        left_join(patent_combined, by = "patent_id") %>% 
        select(patent_id, inventor_id, date, num_us_patents_cited, num_us_applications_cited, num_foreign_documents_cited, kind, type,
               num_inventors, num_assignees, wipo_sector, wipo_field)

n <- nber %>% 
      select(patent_id, category_id, subcategory_id) %>% 
      rename(nber_category = category_id, nber_subcategory = subcategory_id) %>%  
      right_join(p, by = "patent_id")

temp_5yr_citations$citation_id = as.character(temp_5yr_citations$citation_id)
n$num_inventors = as.double(n$num_inventors)
n$num_assignees = as.double(n$num_assignees)
temp_5yr_citations$num_citations_in_5yrs = as.double(temp_5yr_citations$num_citations_in_5yrs)
temp_patent_level_all <- n %>% 
                          left_join(temp_5yr_citations, by = c("patent_id" = "citation_id")) %>% 
                          mutate(num_inventors = if_else(is.na(num_inventors), 0, num_inventors),
                                 num_assignees = if_else(is.na(num_assignees), 0, num_assignees),
                                 weighted_cites_5yrs = ifelse(is.na(weighted_cites_5yrs), 0, weighted_cites_5yrs),
                                 num_citations_in_5yrs = if_else(is.na(num_citations_in_5yrs), 0, num_citations_in_5yrs))
fwrite(temp_patent_level_all, file = "temp_patent_level_all_subset.csv", sep = ",")

############################################################################
##  Government Interest Patents
############################################################################

  
## table of just GI patents
## each row is a patent and each patent appears only once

govint_distinct_id <- patent_govintorg %>% distinct(patent_id)

temp_patent_level_gi <- temp_patent_level_all %>% 
                          filter(patent_id %in% govint_distinct_id$patent_id)

write.csv(temp_patent_level_gi, file = "temp_patent_level_gi_subset.csv")

## government-interest level table of just GI Patents
temp_gi_level_gi <- patent_govintorg %>%
                      left_join(government_organization, by ="organization_id")

############################################################################
##  Non Government Interest Patents
############################################################################

## patent-level data for just non-GI patents
temp_patent_level_nongi <- temp_patent_level_all %>% 
  filter(!(patent_id %in% govint_distinct_id$patent_id))

fwrite(temp_patent_level_nongi, file = "temp_patent_level_nongi_subset.csv", sep = ",")
