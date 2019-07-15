source("requirements.R")

# read in table
temp_inventor_gender <- read_tsv(file = str_c(input_folder, "inventor_gender.tsv"), col_names=TRUE, 
                                 col_types = list(col_character(), col_character(),col_character(), col_character()),
                                 quote = "", na=c("",'"'))

temp_patent_level_gi <- read.csv(file = str_c(input_folder, "temp_patent_level_gi.csv"), header=TRUE, sep=",")
patent_inventor <- fread(file = str_c(input_folder, "patent_inventor.tsv"), header=TRUE, sep="\t")



## Inventor gender data 
## the tables you want are temp_gi_inventor_gender (which has gender and wipo sector etc)
##and temp_gi_has_female_inv which has an indicator for whether the patent has any female inventors


a <- temp_patent_level_gi %>% select(patent_id)


g <- temp_inventor_gender %>% rename(inventor_id = disamb_inventor_id_20190312) %>% select(inventor_id, male) %>%
       filter(!is.na(inventor_id))

# need inventor ids to make a filter list
#merge a with patent_inventor to get patents with inventor_ids
a$patent_id = a$patent_id %>% as.character()
patent_inventor$patent_id = patent_inventor$patent_id %>% as.character()
a_patinv = a %>% left_join(patent_inventor, by = 'patent_id') %>% select(patent_id, inventor_id)

# only keep gi patents who have inventor ids with gender information
temp_govt_associated_inventors_clean <- a_patinv %>% 
                          left_join(g, by = "inventor_id")




write.csv(temp_govt_associated_inventors_clean, file = str_c(output_folder, "temp_govt_associated_inventors_clean.csv"))

# create table temp_gi_inventor_gender
temp_govt_associated_inventors_clean$patent_id = temp_govt_associated_inventors_clean$patent_id %>% as.character()
temp_patent_level_gi$patent_id = temp_patent_level_gi$patent_id %>% as.character()


temp_gi_inventor_gender <- temp_patent_level_gi %>% 
  select(patent_id, num_inventors, date, wipo_sector, wipo_field) %>% 
  left_join(temp_govt_associated_inventors_clean, by ="patent_id")



temp_gi_inventor_gender_clean = temp_gi_inventor_gender %>% rename(male = male) %>% filter(!is.na(male)) %>% filter(male %in% c("0","1"))
temp_gi_inventor_gender_clean$male = temp_gi_inventor_gender_clean$male %>% as.double()



fwrite(temp_gi_inventor_gender_clean, file = str_c(output_folder, "temp_gi_inventor_gender.csv"), sep=",")

# create table 
temp_gi_has_female_inv <- temp_gi_inventor_gender_clean %>% 
                            select(patent_id, male) %>% 
                            group_by(patent_id) %>% 
                            summarise(gender_min = min(male)) %>% 
                            mutate(has_fem_inv = ifelse(gender_min == 0, 1, 0)) %>% 
                            select(patent_id, has_fem_inv)

write.csv(temp_gi_has_female_inv, file = str_c(output_folder,"temp_gi_has_female_inv.csv"))
