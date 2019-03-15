library(dplyr)
library(tidyr)
library(data.table)
#read in table
temp_gi_assignee_type <- fread(file = "temp_gi_assignee_type.csv", header=TRUE, sep=",")
temp_patent_level_gi <- read.csv(file = "temp_patent_level_gi.csv", header=TRUE, sep=",")
patent_assignee <- fread(file = "patent_assignee.tsv", header=TRUE, sep="\t")
assignee <- read.csv(file = "assignee.tsv", header=TRUE, sep="\t")
rawassignee <- fread(file = "rawassignee.tsv", header=TRUE, sep="\t")
## create table with assignee type data
## this uses the new (Mar 9th) thesaurus

a <- assignee %>% 
      select(id, type, organization) %>% 
      rename(assignee_id = id, assignee_type = type) 
      
temp_patent_level_gi$patent_id = as.character(temp_patent_level_gi$patent_id)
pa <- temp_patent_level_gi %>% 
        left_join(patent_assignee, by = "patent_id") %>% 
        select(patent_id, assignee_id)

pa$assignee_id = as.character(pa$assignee_id)
a$assignee_id = as.character(a$assignee_id)
temp_gi_assignee_type <- pa %>% 
                            left_join(a, by = "assignee_id") %>% 
                            select(patent_id, assignee_type, organization)
write.csv(temp_gi_assignee_type, file = "temp_gi_assignee_type_subset.csv")
 
## create table with assignee information for every patent
temp_a <- assignee %>% rename(assignee_id = id)
patent_assignee$assignee_id = as.character(patent_assignee$assignee_id)
temp_a$assignee_id = as.character(temp_a$assignee_id)
all_assignees <- patent_assignee %>% 
                  left_join(temp_a, by ="assignee_id") %>% 
                  rename(id = assignee_id)

fwrite(all_assignees, file = "all_assignees_subset.csv", sep = ",")


assignee_type <- rawassignee %>% select(patent_id, type ,organization)
fwrite(assignee_type, file = "assignee_type.csv", sep = ",")