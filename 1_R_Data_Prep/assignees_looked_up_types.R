source("requirements.R")

# Fill in filepaths
input_folder = ""
output_folder = ""

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


rawassignee = fread(str_c(input_folder, "rawassignee.tsv"), sep = "\t", verbose = TRUE, header= TRUE)
# keep only fields we need
assignee = rawassignee %>% select(patent_id, type, organization)
 
#rm(rawassignee)

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




