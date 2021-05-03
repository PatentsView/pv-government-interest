start = Sys.time()

setwd("G:/pv-government-interest/1_R_Data_Prep")
source("requirements.R")

academic = c()
government= c()
corporate = c()
hospital = c()

cat = list(academic, government, corporate, hospital)
names(cat) = c("Academic", "Government", "Corporate", "Hospital")

# process first thesaurus
# encoding default is UTF-8, specifying here for clarity
acg_txt_con = file(str_c(input_folder, "AcadCorpGovIndiv.txt"), encoding = 'UTF-8')

acg_text = readLines(acg_txt_con)
category = NA

for (line in acg_text){
  if (startsWith(line, "*")){
    category = str_remove_all(line, "\r\n") %>% str_remove_all( pattern= fixed("**"))
    
    if (category == "People"){
      break
      
    }
  }else{
    
    line_cleaned = str_remove_all(line, "\r\n") %>% str_replace_all(regex("\\d"), "") %>% 
      str_replace_all(regex("\\s"), "") %>% str_replace_all( pattern=fixed("\\b"), replacement="")
    cat[category][[1]] = append(cat[category][[1]], line_cleaned)

  }

  
} # end for loop


# close textfile connection
close(acg_txt_con)


# add in additional items 
add_gov = c("represented", "Department of","United States of America", "The National Institute of Standards and Technology",
               "National Institutes of Health", "Tennessee Valley Authority")
add_acad= c('Massachusetts Institute of Technology', 'California Institute of Technology','Rensselaer Polytechnic Institute')
add_hospital = c('Sloan-Kettering Institute for Cancer Research')
add_corp = c('Battelle Memorial Institute', 'The Scripps Research Institute','The Salk Institute for Biological Studies','Dana-Farber Cancer Institute, Inc.', 'Midwest Research Institute', 'Southwest Research Institute', 'Whitehead Institute for Biomedical Research', 'SRI International', 'International Business Machines', 'General Dynamics')



cat["Academic"][[1]] = append(cat["Academic"][[1]], add_acad)
cat["Government"][[1]] = append(cat["Government"][[1]], add_gov)
cat["Corporate"][[1]] = append(cat["Corporate"][[1]] , add_corp)
cat["Hospital"][[1]] = append(cat["Hospital"][[1]], add_hospital)


# process second thesaurus
# encoding default is UTF-8, specifying latin1 to ensure special chars are okay
new_thes_con = file(str_c(input_folder, "new_thesaurus.txt"), encoding = "latin1")

new_thes_text = readLines(new_thes_con)

counter = 0
idx_empty_lines = grep("^$", new_thes_text)
new_thes_text = new_thes_text[-idx_empty_lines]
category = NA
for (line in new_thes_text){
   
  if (counter == 0){
    category = "Academic"
  } else if(counter == 1){
    category = "Government"
  } else{
    category = "Corporate"
  }

  items = str_split(line, ",")[[1]]  
  clean_items = map(items, trimws, which = c("both")) %>% unlist()
  cat[category][[1]] = append(cat[category][[1]], clean_items)
  counter = counter + 1
} # end for 


# close textfile connection
close(new_thes_con)

re_acad = str_c(cat["Academic"][[1]], collapse="|")
re_gov = str_c(cat["Government"][[1]], collapse="|")
re_hosp = str_c(cat["Hospital"][[1]], collapse="|")
re_corp = str_c(cat["Corporate"][[1]], collapse = "|")
re_institute = "institute"


rawassignee = fread(str_c(input_folder, "rawassignee.tsv"), sep = "\t", verbose = FALSE, header= TRUE)
# keep only fields we need
assignee = rawassignee %>% select(patent_id, type, organization)
 

assignee$thes_types = NA

idx_list = c(1:nrow(assignee))
idx_to_run = c()

# ~ 66,000: any Null organizations = Persons
null_idx = which(grepl("NULL", assignee$organization))
assignee$thes_types[null_idx] = "Person"

# acad ~ 211,000: set type of Academic orgs 
acad_idx = which(grepl(regex(re_acad), assignee$organization))
assignee$thes_types[acad_idx] = "Academic"

# ~ 4,000,000: set type of Corp orgs
corp_idx = which(grepl(re_corp, assignee$organization))
assignee$thes_types[corp_idx] = "Corporate"

# set type of corporation institutes
corp_institute_idx = intersect(c(which(grepl(re_institute, assignee$organization))), acad_idx) %>% intersect(corp_idx)
assignee$thes_types[corp_institute_idx] = "Corporate"


# gov ~ 47,000: set type of Gov orgs
gov_idx = which(grepl(re_gov, assignee$organization))
assignee$thes_types[gov_idx] = "Government"

# ~ 2300: set type of Hospital orgs
hosp_idx = which(grepl(re_hosp, assignee$organization))
assignee$thes_types[hosp_idx] = "Hospital"

# set type of orgs not falling into other categories - ambiguous
ambig_idx = which(is.na(assignee$thes_types))
assignee$thes_types[ambig_idx] = "Ambiguous"


fwrite(assignee, str_c(output_folder, "assignees_lookedup_types.csv"), sep = ",")

# temp_gi_assignee_type.csv generation for script 3
############################################################################
##  Government Interest Patents
############################################################################
patent_govintorg <- fread(file = str_c(input_folder, "patent_govintorg.tsv"), header=TRUE, sep="\t")

## table of just GI patents
## each row is a patent and each patent appears only once
govint_distinct_id <- patent_govintorg %>% distinct(patent_id)

temp_gi_assignee_type <- assignee %>% 
  filter(patent_id %in% govint_distinct_id$patent_id)

fwrite(temp_gi_assignee_type, str_c(output_folder, "temp_gi_assignee_type.csv"), sep = ",")




end = Sys.time()

print(end-start)
start = Sys.time()


source("requirements.R")

# 1. read in foreigncitation, usapplicationcitation, and uspatentcitation tables separately
# 2. prepare the counts
# 3. merge into one file

# foreign citation count
foreigncitation <- fread(file = str_c(input_folder, "foreigncitation.tsv"), header=TRUE, sep="\t")
foreigncit_tf = foreigncitation %>% select(patent_id) %>% group_by(patent_id) %>% 
  mutate(num_foreign_documents_cited = n()) %>% unique()

fwrite(foreigncit_tf, str_c(output_folder, "temp_num_foreign_documents_cited.csv"), sep=",")

# remove from memory to free up space
rm(foreigncitation)

# usapplicationcitation count
usappcitation <- fread(str_c(input_folder, "usapplicationcitation.tsv"),header=TRUE, sep="\t")

usappcit_tf = usappcitation %>% select(patent_id) %>% group_by(patent_id) %>% 
  mutate(num_us_applications_cited=n()) %>% unique()

fwrite(usappcit_tf, str_c(output_folder, "temp_num_us_applications_cited.csv"), sep=",")

# remove from memory to free up space
rm(usappcitation)

# uspatentcitation count:
# both num_us_patents_cited & num_times_cited_by_us_patents
uspatentcitation <- fread(file = str_c(input_folder, "uspatentcitation.tsv"), header=TRUE, sep="\t")

uspatcit_tf = uspatentcitation %>% select(patent_id) %>% group_by(patent_id) %>% 
  mutate(num_us_patents_cited = n()) %>% unique()

cit_by_uspat_tf = uspatentcitation %>% select(citation_id) %>% group_by(citation_id) %>% 
  mutate(num_times_cited_by_us_patents = n()) %>% unique()

fwrite(uspatcit_tf, str_c(output_folder, "temp_num_us_patents_cited.csv"), sep=",")
fwrite(cit_by_uspat_tf, str_c(output_folder, "temp_num_times_cited_by_us_patents.csv"), sep=",")


# remove from memory to free up space
rm(uspatentcitation)


# merge at end - one table foreign citation count, uspatcit count,cit_byuspat count, usappcit count
merge_foreign_app = merge(foreigncit_tf, usappcit_tf, by = "patent_id", all = TRUE)
merge_patcit_citbyuspat = merge(uspatcit_tf, cit_by_uspat_tf, by.x = "patent_id", by.y = "citation_id", all = TRUE)
patent_counts_final = merge(merge_foreign_app, merge_patcit_citbyuspat, by="patent_id", all=TRUE)

fwrite(patent_counts_final, str_c(output_folder, "temp_patent_counts_fac_vfinal.csv"), sep=",")

end = Sys.time()

print(end-start)
start = Sys.time()
source("requirements.R")


#read in table
patent_inventor <- fread(file = str_c(input_folder, "patent_inventor.tsv"), header=TRUE, sep="\t")
patent_assignee <- fread(file = str_c(input_folder, "patent_assignee.tsv"), header=TRUE, sep="\t")
nber <- fread(file = str_c(input_folder,"nber.tsv"), header=TRUE, sep="\t")
wipo <- fread(file = str_c(input_folder,"wipo.tsv"), header=TRUE, sep="\t")
wipo_field <- read.csv(file = str_c(input_folder,"wipo_field.tsv"), header=TRUE, sep="\t") 

patent_govintorg <- fread(file = str_c(input_folder, "patent_govintorg.tsv"), header=TRUE, sep="\t")
government_organization <- read.csv(file = str_c(input_folder, "government_organization.tsv"), header=TRUE, sep="\t")

# patent counts and patent merged table
patent = fread(file=str_c(input_folder, "patent.tsv"), header = TRUE, sep="\t")
patent_counts = fread(file=str_c(input_folder,"temp_patent_counts_fac_vfinal.csv"), header=TRUE, sep=",", verbose = FALSE)

patent$id <- as.character(patent$id)
patent_merged = patent %>% left_join(patent_counts, by = c("id" = "patent_id" ))
patent_merged$year = patent_merged$date %>% ymd() %>% year()
fwrite(patent_merged, file = str_c(output_folder, "temp_patent_merged.csv"), sep = ",")

## Create the main Patent Level and Government Interest Level tables
## These tables have a lot of details around the patents including information from the database
## and  also from the citation count tables generated in the previous steps

############################################################################
##  All Patents
############################################################################
c <- patent_inventor %>% 
  group_by(patent_id) %>% 
  summarise(num_inventors = n())

#rm(patent_inventor)

d <- patent_assignee %>%
  group_by(patent_id) %>% 
  summarise(num_assignees = n())

d$patent_id <- as.character(d$patent_id)

e <- c %>% left_join(d, by = "patent_id")

wipo$patent_id <- as.character(wipo$patent_id)
w <- wipo %>% 
  filter(sequence == 0) %>% 
  right_join(e, by = "patent_id") %>% 
  rename(id = field_id)

w$id <- as.character(w$id)

wf <- w %>% 
  left_join(wipo_field, by = "id") %>% 
  rename(wipo_sector = sector_title, wipo_field = field_title)

p <- wf %>% 
  left_join(patent_merged, by = c("patent_id" ="id")) %>% 
  select(patent_id, date, num_us_patents_cited, num_us_applications_cited, num_foreign_documents_cited, kind, type,
         num_inventors, num_assignees, wipo_sector, wipo_field, year)

n <- nber %>% 
  select(patent_id, category_id, subcategory_id) %>% 
  rename(nber_category = category_id, nber_subcategory = subcategory_id) %>%  
  right_join(p, by = "patent_id")

n$num_inventors = as.double(n$num_inventors)
n$num_assignees = as.double(n$num_assignees)
temp_patent_level_all <- n %>%
  mutate(num_inventors = if_else(is.na(num_inventors), 0, num_inventors),
         num_assignees = if_else(is.na(num_assignees), 0, num_assignees)) #,

fwrite(temp_patent_level_all, file = str_c(output_folder, "temp_patent_level_all.csv"), sep = ",")

############################################################################
##  Government Interest Patents
############################################################################

## table of just GI patents
## each row is a patent and each patent appears only once

govint_distinct_id <- patent_govintorg %>% distinct(patent_id)

temp_patent_level_gi <- temp_patent_level_all %>% 
  filter(patent_id %in% govint_distinct_id$patent_id)

fwrite(temp_patent_level_gi, file = str_c(output_folder,"temp_patent_level_gi.csv"), sep = ",")

## government-interest level table of just GI Patents
temp_gi_level_gi <- patent_govintorg %>%
  left_join(government_organization, by ="organization_id")
fwrite(temp_gi_level_gi, file=str_c(output_folder, "temp_gi_level_gi.csv"), sep = ",")
############################################################################
##  Non Government Interest Patents
############################################################################

## patent-level data for just non-GI patents
temp_patent_level_nongi <- temp_patent_level_all %>% 
  filter(!(patent_id %in% govint_distinct_id$patent_id))

fwrite(temp_patent_level_nongi, file = str_c(output_folder, "temp_patent_level_nongi.csv"), sep = ",")

end = Sys.time()

print(end-start)
start = Sys.time()


#read in table
temp_gi_assignee_type <- fread(file = str_c(output_folder, "temp_gi_assignee_type.csv"), header=TRUE, sep=",")
temp_patent_level_gi <- read.csv(file = str_c(output_folder, "temp_patent_level_gi.csv"), header=TRUE, sep=",")
patent_assignee <- fread(file = str_c(input_folder, "patent_assignee.tsv"), header=TRUE, sep="\t")
assignee <- read.csv(file = str_c(input_folder, "assignee.tsv"), header=TRUE, sep="\t")
rawassignee <- fread(file = str_c(input_folder, "rawassignee.tsv"), header=TRUE, sep="\t")

## create table with assignee type data
## this uses the new (Mar 9th) thesaurus

a <- assignee %>% 
  select(id, type, organization) %>% 
  rename(assignee_id = id, assignee_type = type) 

temp_patent_level_gi$patent_id = as.character(temp_patent_level_gi$patent_id)

patent_assignee$patent_id <- as.character(patent_assignee$patent_id)
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

end = Sys.time()

print(end-start)
start = Sys.time()

source("requirements.R")

# read in table
# temp_inventor_gender <- read_tsv(file = str_c(input_folder, "inventor_gender.tsv"), col_names=TRUE, 
#                                  col_types = list(col_character(), col_character(),col_character(), col_character()),
#                                  na=c("",'"'))
temp_inventor_gender <- read_tsv(file = str_c(input_folder, "inventor_gender.tsv"), col_names=TRUE, na=c("",'"'))

temp_patent_level_gi <- read.csv(file = str_c(output_folder, "temp_patent_level_gi.csv"), header=TRUE, sep=",")
patent_inventor <- fread(file = str_c(input_folder, "patent_inventor.tsv"), header=TRUE, sep="\t")

## Inventor gender data 
## the tables you want are temp_gi_inventor_gender (which has gender and wipo sector etc)
##and temp_gi_has_female_inv which has an indicator for whether the patent has any female inventors

a <- temp_patent_level_gi %>% select(patent_id)

g <- temp_inventor_gender %>% rename(inventor_id = id) %>% select(inventor_id, male_flag) %>%
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

write.csv(temp_govt_associated_inventors_clean, file = str_c(output_folder, "temp_govt_associated_inventors_clean.csv"))

temp_gi_inventor_gender <- temp_patent_level_gi %>% 
  select(patent_id, num_inventors, date, wipo_sector, wipo_field) %>% 
  left_join(temp_govt_associated_inventors_clean, by ="patent_id")

temp_gi_inventor_gender_clean = temp_gi_inventor_gender %>% rename(male = male_flag) %>% filter(!is.na(male)) %>% filter(male %in% c("0","1"))
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




end = Sys.time()

print(end-start)
start = Sys.time()


#read in table
uspatentcitation <- fread(file = str_c(input_folder,"uspatentcitation.tsv"), header=TRUE, sep="\t")
government_interest <- read.csv(file = str_c(input_folder, "government_interest.tsv"), header=TRUE, sep="\t")

# patent counts and patent merged table
patent = fread(file=str_c(output_folder,"temp_patent_merged.csv"), header=TRUE, sep=",", verbose = FALSE)

patent = patent %>% rename(patent_id = id)

## table with each government interest patent and any citations within 5 years -- for each year 1 thru 5 (changed by ska)
## patent_20180528.temp_updated_gi is the table with all the government interest and government assignee patents
distinct_patent_id <- government_interest %>% distinct(patent_id)
a <- uspatentcitation %>%  
  filter(citation_id %in% distinct_patent_id$patent_id) %>% 
  select(patent_id, citation_id)

patent$patent_id <- as.character(patent$patent_id)
b <- a %>% 
  left_join(patent, by="patent_id") %>% 
  select(citation_id, patent_id, date, num_times_cited_by_us_patents) %>% 
  rename(citing_patent_date = date)

c <- b %>% 
  left_join(patent, by = c("citation_id" = "patent_id")) %>% 
  rename(cited_patent_date = date, num_times_cited_by_us_patents = num_times_cited_by_us_patents.x) %>% 
  select(citation_id, patent_id, cited_patent_date, citing_patent_date, num_times_cited_by_us_patents)

fwrite(c, file = str_c(output_folder,"temp_joined_gi_uspatcit_pat.csv"))

## year 5
temp_5yr_citations_by_cite_yr5 <- c %>% 
  mutate(date_diff = as.integer(difftime(citing_patent_date,cited_patent_date))) %>% 
  filter(date_diff <= 365 * 5 & date_diff >365 * 4) %>% 
  select(citation_id, patent_id, cited_patent_date, 
         citing_patent_date, num_times_cited_by_us_patents)

temp_5yr_citations_yr5_1 <- temp_5yr_citations_by_cite_yr5 %>% 
  group_by(citation_id) %>% 
  dplyr::summarize(num_citations_5 = n()) %>%
  rename(patent_id = citation_id)

temp_5yr_citations_yr5_2 <- temp_5yr_citations_by_cite_yr5 %>% 
  group_by(citation_id) %>% 
  dplyr::summarize(weighted_cites_5yrs = sum(num_times_cited_by_us_patents, na.rm = TRUE)) %>% 
  rename(patent_id = citation_id)

temp_5yr_citations_yr5 <- temp_5yr_citations_yr5_1 %>% 
  inner_join(temp_5yr_citations_yr5_2, by = "patent_id")

## year 4
temp_5yr_citations_by_cite_yr4 <- c %>% 
  mutate(date_diff = as.integer(difftime(citing_patent_date,cited_patent_date))) %>% 
  filter(date_diff <= 365 * 4 & date_diff > 365 * 3) %>% 
  select(citation_id, patent_id, cited_patent_date, 
         citing_patent_date, num_times_cited_by_us_patents)

temp_5yr_citations_yr4_1 <- temp_5yr_citations_by_cite_yr4 %>% 
  group_by(citation_id) %>% 
  dplyr::summarize(num_citations_4 = n()) %>%
  rename(patent_id = citation_id)

temp_5yr_citations_yr4_2 <- temp_5yr_citations_by_cite_yr4 %>% 
  group_by(citation_id) %>% 
  dplyr::summarize(weighted_cites_5yrs = sum(num_times_cited_by_us_patents, na.rm = TRUE)) %>% 
  rename(patent_id = citation_id)

temp_5yr_citations_yr4 <- temp_5yr_citations_yr4_1 %>% 
  inner_join(temp_5yr_citations_yr4_2, by = "patent_id")

## year 3
temp_5yr_citations_by_cite_yr3 <- c %>% 
  mutate(date_diff = as.integer(difftime(citing_patent_date,cited_patent_date))) %>% 
  filter(date_diff <= 365 * 3 & date_diff > 365 * 2) %>% 
  select(citation_id, patent_id, cited_patent_date, 
         citing_patent_date, num_times_cited_by_us_patents)

temp_5yr_citations_yr3_1 <- temp_5yr_citations_by_cite_yr3 %>% 
  group_by(citation_id) %>% 
  dplyr::summarize(num_citations_3 = n()) %>% 
  rename(patent_id = citation_id)

temp_5yr_citations_yr3_2 <- temp_5yr_citations_by_cite_yr3 %>% 
  group_by(citation_id) %>% 
  dplyr::summarize(weighted_cites_5yrs = sum(num_times_cited_by_us_patents, na.rm = TRUE)) %>% 
  rename(patent_id = citation_id)

temp_5yr_citations_yr3 <- temp_5yr_citations_yr3_1 %>% 
  inner_join(temp_5yr_citations_yr3_2, by = "patent_id")

## year 2
temp_5yr_citations_by_cite_yr2 <- c %>% 
  mutate(date_diff = as.integer(difftime(citing_patent_date,cited_patent_date))) %>% 
  filter(date_diff <= 365 * 2 & date_diff >365 * 1) %>% 
  select(citation_id, patent_id, cited_patent_date, 
         citing_patent_date, num_times_cited_by_us_patents)

temp_5yr_citations_yr2_1 <- temp_5yr_citations_by_cite_yr2 %>% 
  group_by(citation_id) %>% 
  dplyr::summarize(num_citations_2 = n()) %>% 
  rename(patent_id = citation_id)

temp_5yr_citations_yr2_2 <- temp_5yr_citations_by_cite_yr2 %>% 
  group_by(citation_id) %>% 
  dplyr::summarize(weighted_cites_5yrs = sum(num_times_cited_by_us_patents, na.rm=TRUE)) %>% 
  rename(patent_id = citation_id)

temp_5yr_citations_yr2 <- temp_5yr_citations_yr2_1 %>% 
  inner_join(temp_5yr_citations_yr2_2, by = "patent_id")

## year 1
temp_5yr_citations_by_cite_yr1 <- c %>% 
  mutate(date_diff = as.integer(difftime(citing_patent_date,cited_patent_date))) %>% 
  filter(date_diff < 365 * 2) %>% 
  select(citation_id, patent_id, cited_patent_date, 
         citing_patent_date, num_times_cited_by_us_patents)

temp_5yr_citations_yr1_1 <- temp_5yr_citations_by_cite_yr1 %>% 
  group_by(citation_id) %>% 
  dplyr::summarize(num_citations_1 = n()) %>% 
  rename(patent_id = citation_id)

temp_5yr_citations_yr1_2 <- temp_5yr_citations_by_cite_yr1 %>% 
  group_by(citation_id) %>% 
  dplyr::summarize(weighted_cites_5yrs = sum(num_times_cited_by_us_patents, na.rm=TRUE)) %>% 
  rename(patent_id = citation_id) %>% 
  select(patent_id, weighted_cites_5yrs)

temp_5yr_citations_yr1 <- temp_5yr_citations_yr1_1 %>% 
  inner_join(temp_5yr_citations_yr1_2, by = "patent_id")

write.csv(temp_5yr_citations_yr5, file = str_c(output_folder,"temp_5yr_citations_yr5.csv"))
write.csv(temp_5yr_citations_yr4, file = str_c(output_folder,"temp_5yr_citations_yr4.csv"))
write.csv(temp_5yr_citations_yr3, file = str_c(output_folder,"temp_5yr_citations_yr3.csv"))
write.csv(temp_5yr_citations_yr2, file = str_c(output_folder,"temp_5yr_citations_yr2.csv"))
write.csv(temp_5yr_citations_yr1, file = str_c(output_folder,"temp_5yr_citations_yr1.csv"))


end = Sys.time()

print(end-start)