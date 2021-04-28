
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
