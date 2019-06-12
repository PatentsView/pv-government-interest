source("requirements.R")

#read in table
temp_gi_assignee_type <- fread(file = str_c(input_folder, "temp_gi_assignee_type.csv"), header=TRUE, sep=",")
temp_patent_level_gi <- read.csv(file = str_c(input_folder, "temp_patent_level_gi.csv"), header=TRUE, sep=",")
patent_assignee <- fread(file = str_c(input_folder, "patent_assignee.tsv"), header=TRUE, sep="\t")
assignee <- read.csv(file = str_c(input_folder, "assignee.tsv"), header=TRUE, sep="\t")
rawassignee <- fread(file = str_c(input_folder, "rawassignee.tsv"), header=TRUE, sep="\t")

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
write.csv(temp_gi_assignee_type, file = str_c(output_folder, "temp_gi_assignee_type.csv"))
 
## create table with assignee information for every patent
temp_a <- assignee %>% rename(assignee_id = id)
patent_assignee$assignee_id = as.character(patent_assignee$assignee_id)
temp_a$assignee_id = as.character(temp_a$assignee_id)
all_assignees <- patent_assignee %>% 
                  left_join(temp_a, by ="assignee_id") %>% 
                  rename(id = assignee_id)

fwrite(all_assignees, file = str_c(output_folder, "all_assignees.csv"), sep = ",")


assignee_type <- rawassignee %>% select(patent_id, type ,organization)
fwrite(assignee_type, file = str_c(output_folder, "assignee_type.csv"), sep = ",")