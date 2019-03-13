library(dplyr)
library(tidyr)
library(data.table)
#read in table
temp_gi_assignee_type <- read.csv(file = "temp_gi_assignee_type.csv", header=TRUE, sep=",")
temp_patent_level_gi <- read.csv(file = "temp_patent_level_gi.csv", header=TRUE, sep=",")
patent_assignee <- fread(file = "patent_assignee.tsv", header=TRUE, sep="\t")
assignee <- read.csv(file = "assignee.tsv", header=TRUE, sep="\t")
rawassignee <- fread(file = "rawassignee.tsv", header=TRUE, sep="\t")
## create table with assignee type data
## this uses the new (Mar 9th) thesaurus

a <- assignee %>% 
      select(id, type, organization) %>% 
      rename(assignee_id = id, assignee_type = type) 
      
pa <- temp_patent_level_gi %>% 
        left_join(patent_assignee, by = "patent_id") %>% 
        select(patent_id, assignee_id)

temp_gi_assignee_type <- pa %>% 
                            left_join(a, by = "assignee_id") %>% 
                            select(patent_id, assignee_type, organization)
write.csv(temp_gi_assignee_type, file = "temp_gi_assignee_type.csv")
 
## create table with assignee information for every patent
temp_a <- assignee %>% rename(assignee_id = id)
all_assignees <- patent_assignee %>% 
                  left_join(temp_a, by ="assignee_id") %>% 
                  rename(id = assignee_id)

write.csv(all_assignees, file = "all_assignees.csv")


assignee_type <- rawassignee %>% select(patent_id, type ,organization)
write.csv(assignee_type, file = "assignee_type.csv")