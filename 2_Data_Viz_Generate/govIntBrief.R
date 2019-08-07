# create visualizations for government interest issue brief
# sarora@air.org
# march 2017
# check for supporting docs and code in github or on the patentsview shared drive 
source("requirements.R")
#########################################################################################################
in.patent_level <- read.csv("data_to_read/temp_patent_level_gi.csv", header = TRUE, stringsAsFactors = FALSE)
in.gov_level <- read.csv("data_to_read/temp_gi_level_gi.csv", header = TRUE, stringsAsFactors = FALSE)
in.all <- fread("data_to_read/temp_patent_level_all.csv", verbose = TRUE)
in.assignees.all <- fread("data_to_read/all_assignees.csv", verbose=TRUE)
in.cite_1 <- read.csv("data_to_read/temp_5yr_citations_yr1.csv", header = TRUE, stringsAsFactors = FALSE)
in.cite_2 <- read.csv("data_to_read/temp_5yr_citations_yr2.csv", header = TRUE, stringsAsFactors = FALSE)
in.cite_3 <- read.csv("data_to_read/temp_5yr_citations_yr3.csv", header = TRUE, stringsAsFactors = FALSE)
in.cite_4 <- read.csv("data_to_read/temp_5yr_citations_yr4.csv", header = TRUE, stringsAsFactors = FALSE)
in.cite_5 <- read.csv("data_to_read/temp_5yr_citations_yr5.csv", header = TRUE, stringsAsFactors = FALSE)


### read in tables from local file(not exist in db)
in.sector <- fread("data_to_read/assignees_lookedup_types.csv", header = TRUE, stringsAsFactors = FALSE)
in.fund <- read.csv("data_to_read/agencies.csv", header = TRUE, stringsAsFactors = FALSE)
in.size <- read.csv("data_to_read/government_interest_patents_1980-2018_returned.csv", header = TRUE, stringsAsFactors = FALSE)
#########################################################################################################

# these government interest patents have some full text fields that contain organizations; not fixing here
# process patent level data
in.patent_level <- in.patent_level %>% mutate(year = year(date)) %>% filter(year != "NULL" & year >= 1980 & year <= 2017) # filter on years 
patents.keep_ids <- in.patent_level$patent_id                    
#write.csv(patents.keep_ids, file="out/patents.keep_ids.csv")

# filter on gov_level ids to get the right data 
in.gov_level <- in.gov_level %>% filter(patent_id %in% patents.keep_ids)

# merge data two main data files
in.patent_level.merged <- merge(in.patent_level, in.gov_level, by="patent_id")
#write.csv (in.patent_level.merged, file="out/out.patent_level.merged.csv")

#########################################################################################################
# Plots

source("top6_technology_fields.R")

# top6_plot 
top6_plot

save_plot_png(top6_plot, "data_viz/longWipoFields.png")

CairoPDF(file= "data_viz/longWipoFields",  width = 9, height = 7)
top6_plot
dev.off()


share_gi_total_plot 

save_plot_png(share_gi_total_plot, "data_viz/longWipoFieldsPercent.png")

CairoPDF(file= "data_viz/longWipoFieldsPercent",  width = 9, height = 7)
share_gi_total_plot 
dev.off()


source("other_figures.R")

index_plot

save_plot_png(index_plot, "data_viz/indexed.png")

CairoPDF(file= "data_viz/indexed",  width = 9, height = 7)
index_plot
dev.off()


# mean_num_inv_plot save as pdf 
mean_num_inv_plot


save_plot_png(mean_num_inv_plot, "data_viz/longInventor.png")

CairoPDF(file= "data_viz/longInventor",  width = 9, height = 7)
mean_num_inv_plot
dev.off()

# funding_agencies_plot save as pdf
funding_agencies_plot

save_plot_png(funding_agencies_plot, "data_viz/funders-assignees.dodged.png")

CairoPDF(file= "data_viz/funders-assignees.dodged", width = 9, height = 7)
funding_agencies_plot
dev.off()

# firm_size_plot save as pdf 
firm_size_plot

save_plot_png(firm_size_plot, "data_viz/firmSize.png")
CairoPDF(file = "data_viz/firmSize", width = 9, height = 7)
firm_size_plot
dev.off()


source("patent_flow_sankey.R")
#patent_flow_plot

#Note: Will need to export manually from R as .png - R handles plotly sankey viz differently
#CairoPDF(file = paste0("data_viz/sankey_", script_v), width = 9, height = 7)
#patent_flow_plot
#dev.off()

source("citation_analysis.R")
citation_plot

save_plot_png(citation_plot,"data_viz/fiveYearCitationImpact.png")
# save citation_plot as pdf
CairoPDF(file = "data_viz/fiveYearCitationImpact", width = 9, height = 7)
citation_plot
dev.off() 


# create data table aligning sector, agency, and field, by year

# merge sector type for assignees and create dodged bar chart
in.sector$organization <- trimws(in.sector$organization)
sector_org.merged <- in.patent_level.merged %>% 
  select(patent_id, level_one, wipo_field, year) %>% 
  distinct() %>% # just patent_id, level_one org
  inner_join(in.sector, by="patent_id") %>% 
  select(patent_id, level_one, type, organization, thes_types, wipo_field, year)

# add a column weight = 1 / number of patent_id
sector_org.merged.ratio <- sector_org.merged %>% 
  group_by(patent_id) %>% 
  mutate(weight = 1/n())

# change the thes_types
sector_org.merged.ratio.clnd <- sector_org.merged.ratio %>% 
  mutate(thes_types = recode(thes_types, 'Ambiguous' = 'Corporate',
                             'Hospital' = "Academic or Hospital",
                             'Academic' = "Academic or Hospital",
                             'Person' = "Other"))
# distinct get top funders
level_one.clnd <- in.gov_level %>% 
  select(patent_id, level_one) %>% 
  distinct() %>% 
  group_by(patent_id) %>% 
  mutate(weight = 1/n())

sector_org_field.by_year.merged <- merge(sector_org.merged.ratio.clnd, in.patent_level, by="patent_id")
sector_cols = c("patent_id","level_one","thes_types", "wipo_field.x", "year.x", 
                "nber_subcategory", "wipo_sector")
sector_org_field.by_year.small <- sector_org_field.by_year.merged[, sector_cols] %>% rename(wipo_field = wipo_field.x, year = year.x)
sector_org_field.by_year.ratio <- sector_org_field.by_year.small %>% 
  group_by(patent_id) %>% 
  mutate(weight = 1/n())
  
sector_org_field.by_year.clnd <- sector_org_field.by_year.ratio
sector_org_field.by_year.clnd[which (! sector_org_field.by_year.clnd$level_one %in% top6 ), 2] <- "Other"
sector_org_field.by_year.clnd[which (sector_org_field.by_year.clnd$level_one == "United States Government"), 2] <- "Other"
  
sector_org_field.by_year.count <- sector_org_field.by_year.clnd %>% 
  group_by(level_one, thes_types, wipo_field, year) %>% 
  summarise(freq = sum(weight))
  
# create patent_level-gov_level merged dataset and weight by level_one org 
 
in.patent_level.ratio <-  in.patent_level.merged %>% 
  group_by(patent_id) %>% 
  mutate(weight = 1/n())
  
patents.by_agency <- in.patent_level.ratio %>% group_by (level_one) %>% summarize (count = sum(weight))
  
write.csv(sector_org_field.by_year.count, file="out/sector_org_field.by_year.count.csv")
  
fwrite(in.patent_level.ratio, file = "out/weighted_gi_patents.by_funding_agency.csv",sep=",")
  
