library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(data.table)

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

rawassignee = fread("G:/PatentsView/cssip/govtint_testing/rawassignee.tsv", sep = "\t", verbose = TRUE, header= TRUE)
# keep only fields we need
assignee = rawassignee %>% select(patent_id, type, organization)
 
rm(rawassignee)

assignee$thes_type = NA
counter = 0
for(x in 1:nrow(assignee)){
  text = assignee$organization[x]
  if(counter %% 100000 == 0){
    print(str_c("passed next 100,000 lines - counter: ", counter,sep=""))
  }
  re_type = NA
  acad = grepl(re_acad, text)
  gov = grepl(re_gov, text)
  corp = grepl(re_corp, text)
  hosp = grepl(re_hosp, text)
  corp_institute = grepl(re_institute, text) & corp & acad
  
  if(text == "NULL"){
    re_type = "Person"
  }else if(corp_institute){
    re_type = "Corporate"
  }else if (acad){
    re_type = "Academic"
  }else if (gov){
    re_type = "Government"
  }else if (corp){
    re_type = "Corporate"
  }else if (hosp){
    re_type = "Hospital"
  }else{
    re_type = "Ambiguous"
  }
  
  assignee$thes_type[x] = re_type
  counter = counter + 1
}


fwrite(assignee, "assignees_lookedup_types_r.csv", sep = ",")

