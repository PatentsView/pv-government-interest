library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(data.table)
library(chunked)
academic = c()
government= c()
corporate = c()
hospital = c()

cat = list(academic, government, corporate, hospital)
names(cat) = c("Academic", "Government", "Corporate", "Hospital")

# process first thesaurus
acg_txt_con = file("AcadCorpGovIndiv.txt")

acg_text = readLines(acg_txt_con)
category = NA

for (line in acg_text){
  print(line)
  if (startsWith(line, "*")){
    category = str_remove_all(line, "\r\n") %>% str_remove_all( pattern= fixed("**"))
    print(str_c("category is ", category, sep = ""))
    
    if (category == "People"){
      break
      
    }
  }else{
    
    line_cleaned = str_remove_all(line, "\r\n") %>% str_replace_all(regex("\\d"), "") %>% 
      str_replace_all(regex("\\s"), "") %>% str_replace_all( pattern=fixed("\\b"), replacement="")
    cat[category][[1]] = append(cat[category][[1]], line_cleaned)
    print(str_c("clean line is ", line_cleaned, sep = ""))
  }

  
} # end for loop


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
new_thes = file("new_thesaurus.txt")

new_thes_text = readLines(new_thes)
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
  
  print(str_c("category is: ", category, sep = ""))
  items = str_split(line, ",")[[1]]  
  clean_items = map(items, trimws, which = c("both")) %>% unlist()
  cat[category][[1]] = append(cat[category][[1]], clean_items)
  counter = counter + 1
} # end for 


re_acad = str_c(cat["Academic"][[1]], collapse="|")
re_gov = str_c(cat["Government"][[1]], collapse="|")
re_hosp = str_c(cat["Hospital"][[1]], collapse="|")
re_corp = str_c(cat["Corporate"][[1]], collapse = "|")
re_institute = "institute"

#assignee_script = fread("F:/Govt_Int/2018_Update/2018_Update_gov_int/data_to_read/assignee_type.csv", sep = ",", verbose = TRUE, header= TRUE)


rawassignee = fread("G:/PatentsView/cssip/govtint_testing/rawassignee.tsv", sep = "\t", verbose = TRUE, header= TRUE)
# keep only fields we need
assignee = rawassignee %>% select(patent_id, type, organization)
 
#check = assignee[1:15, "organization"]

rm(rawassignee)

assignee$thes_type = NA

idx_list = c(1:nrow(assignee))
idx_to_run = c()

# ~ 66,797: any Null organizations = Persons
null_idx = which(grepl("NULL", assignee$organization))
assignee$thes_type[null_idx] = "Person"

# acad ~ 211,604: set type of Academic orgs 
acad_idx = which(grepl(re_acad, assignee$organization))
assignee$thes_type[acad_idx] = "Academic"

# ~ 4,354,349: set type of Corp orgs
corp_idx = which(grepl(re_corp, assignee$organization))
assignee$thes_type[corp_idx] = "Corporate"

# set type of corporation institutes
corp_institute_idx = intersect(c(which(grepl(re_institute, assignee$organization[idx_to_run]))), acad_idx) %>% intersect(corp_idx)
assignee$thes_type[corp_institute_idx] = "Corporate"


# gov ~ 47,428: set type of Gov orgs
gov_idx = which(grepl(re_gov, assignee$organization))
assignee$thes_type[gov_idx] = "Government"

# ~ 2300: set type of Hospital orgs
hosp_idx = which(grepl(re_hosp, assignee$organization))
assignee$thes_type[hosp_idx] = "Hospital"

# set type of orgs not falling into other categories - ambiguous
idx_to_run = setdiff(idx_to_run, hosp_idx)
ambig_idx = which(is.na(assignee$thes_type))
assignee$thes_type[ambig_idx] = "Ambiguous"

assignee_prev = fread("G:/PatentsView/cssip/govtint_testing/assignees_lookedup_types_prev.csv", sep=",")
assignee_prev = assignee_prev %>% select(patent_id, type, organization, thes_types)
assignee_match_idx = match(assignee_prev$patent_id, assignee$patent_id)

check = assignee[assignee_match_idx]

assignee_prev = assignee_prev %>% arrange(organization)
check = check %>% arrange(organization)

all.equal(assignee_prev, check)


fwrite(assignee, "assignees_lookedup_types_r_v3_testing.csv", sep = ",")

# temp_gi_assignee_type.csv generation for script 3
############################################################################
##  Government Interest Patents
############################################################################
patent_govintorg <- fread(file = "G:/PatentsView/cssip/govtint_testing/patent_govintorg.tsv", header=TRUE, sep="\t")

## table of just GI patents
## each row is a patent and each patent appears only once
govint_distinct_id <- patent_govintorg %>% distinct(patent_id)

temp_patent_level_gi <- assignee %>% 
  filter(patent_id %in% govint_distinct_id$patent_id)






