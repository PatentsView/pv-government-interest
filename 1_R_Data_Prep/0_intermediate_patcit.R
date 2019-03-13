library(data.table)
library(dplyr)
library(readr)
#library(microbenchmark)

# 1. read in foreigncitation, usapplicationcitation, and uspatentcitation tables separately
# 2. prepare the counts
# 3. merge into one file

# foreign citation count
foreigncitation <- fread(file = "foreigncitation.tsv", header=TRUE, sep="\t", quote = "")
foreigncit_tf = foreigncitation %>% select(patent_id) %>% group_by(patent_id) %>% 
  mutate(num_foreign_documents_cited = n()) %>% unique()

fwrite(foreigncit_tf, "temp_num_foreign_documents_cited.csv", sep=",")

# remove from memory to free up space
rm(foreigncitation)

# usapplicationcitation count
usappcitation <- fread("usapplicationcitation.tsv",header=TRUE, sep="\t",quote="")

usappcit_tf = usappcitation %>% select(patent_id) %>% group_by("patent_id") %>% 
  mutate(num_us_applications_cited=n()) %>% unique()

fwrite(usappcit_tf, "temp_num_us_applications_cited.csv", sep=",")

# remove from memory to free up space
rm(usapplicationcitation)

# uspatentcitation count
uspatentcitation <- fread(file = "uspatentcitation.tsv", header=TRUE, sep="\t", quote="", nThread=24)

uspatcit_tf = uspatentcitation %>% select(patent_id) %>% group_by("patent_id") %>% 
  mutate(num_us_patents_cited = n()) %>% unique()

fwrite(uspatcit_tf, "temp_num_us_patents_cited.csv", sep=",")

# remove from memory to free up space
rm(uspatentcitation)


# merge at end - one table foreign citation counts, uspatcit count, usappcit count
merge_foreign_app = merge(foreigncit_tf, usappcit_tf, by=c("patent_id" = "patent_id"), all = TRUE)
patent_counts_final = merge(merge_foreign_app, uspatcit_tf, by=c("patent_id" = "patent_id"), all=TRUE)

fwrite(patent_counts_final, "temp_patent_counts_fac.csv", sep=",")

#merge1 = microbenchmark( merge(tf_uniq, appcit_tf_uniq, by=c("patent_id" = "patent_id"), all = TRUE), times=2)
#merge1$time[1] / 10^-9
