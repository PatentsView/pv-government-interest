source("requirements.R")

# input and output folder paths
input_folder = "G:/PatentsView/cssip/govtint_testing/"
output_folder = "full_testing/"


# read in table
temp_inventor_gender <- fread(file = str_c(input_folder, "inventor_gender.tsv"), header=TRUE, sep="\t")
temp_patent_level_gi <- read.csv(file = str_c(output_folder, "temp_patent_level_gi.csv"), header=TRUE, sep=",")
patent_inventor <- fread(file = str_c(input_folder, "patent_inventor.tsv"), header=TRUE, sep="\t")
#temp_govt_associated_inventors_clean <- read.csv(file = "temp_govt_associated_inventors_clean.csv", header=TRUE, sep=",")

## Inventor gender data 
## the tables you want are temp_gi_inventor_gender (which has gender and wipo sector etc)
##and temp_gi_has_female_inv which has an indicator for whether the patent has any female inventors

## the existing temp_inventor_gender is the results the italians produced, uploaded ot mysql 

a <- temp_patent_level_gi %>% select(patent_id, inventor_id)
g <- temp_inventor_gender %>% rename(inventor_id = disamb_inventor_id_20181127) %>% select(inventor_id, male) %>%
      filter(!is.na(inventor_id))

a$inventor_id = a$inventor_id %>% as.character()
g$inventor_id = g$inventor_id %>% as.character()
patent_id_filter_lst <- a %>% 
                          left_join(g, by = "inventor_id") %>% 
                          select(patent_id)


patent_inventor_gender = g %>% left_join(patent_inventor, by = c("inventor_id" = "inventor_id")) 


temp_govt_associated_inventors_clean <- patent_inventor_gender %>% 
                                            filter(patent_id %in% patent_id_filter_lst$patent_id) %>% 
                                            select(patent_id, inventor_id, male)

write.csv(temp_govt_associated_inventors_clean, file = str_c(output_folder, "temp_govt_associated_inventors_clean.csv"))

## create table temp_gi_inventor_gender
tg <- temp_govt_associated_inventors_clean %>% filter(!is.na(male)) %>%  select(patent_id, inventor_id, male)
tg$patent_id = tg$patent_id %>% as.character()
temp_patent_level_gi$patent_id = temp_patent_level_gi$patent_id %>% as.character()
temp_gi_inventor_gender <- temp_patent_level_gi %>% 
                             select(patent_id, num_inventors, date, wipo_sector, wipo_field) %>% 
                             left_join(tg, by ="patent_id")
fwrite(temp_gi_inventor_gender, file = str_c(output_folder, "temp_gi_inventor_gender.csv"), sep=",")

## create table 
temp_gi_has_female_inv <- temp_gi_inventor_gender %>% 
                            select(patent_id, male) %>% 
                            group_by(patent_id) %>% 
                            summarise(gender_min = min(male)) %>% 
                            mutate(has_fem_inv = ifelse(gender_min == 0, 1, 0)) %>% 
                            select(patent_id, has_fem_inv)

write.csv(temp_gi_has_female_inv, file = str_c(output_folder,"temp_gi_has_female_inv.csv"))