
source("requirements.R")

#########################################################################################################
# load patent data
in.patent_level <- read.csv("data_to_read/temp_patent_level_gi.csv")
in.patent_level <- in.patent_level %>% filter(year != "NULL")
in.patent_level$year <- as.numeric(as.character(in.patent_level$year))
in.patent_level$patent_id <- as.character(in.patent_level$patent_id)

# load assignee level data
in.assignee_level_types <- read.csv("data_to_read/assignees_lookedup_types.csv", header = TRUE, stringsAsFactors = FALSE)
in.assignee_level_types <- in.assignee_level_types %>% select(patent_id, type, organization, thes_types)


assignees_years_types <- in.assignee_level_types %>% 
  left_join(in.patent_level, by = 'patent_id') %>% 
  filter(year >= 1980 & year <= 2018) %>% 
  mutate(thes_types = case_when(thes_types == 'Academic' ~ "Academic or Hospital",
                                thes_types == 'Hospital' ~ "Academic or Hospital",
                                thes_types == 'Ambiguous' ~ "Corporate",
                                thes_types == 'Person' ~ "Other",
                                thes_types == NA ~ 'Government',
                                thes_types == "Corporate" ~ "Corporate",
                                thes_types == "Government" ~ "Government"))

assignees_years_types$thes_types[is.na(assignees_years_types$thes_types)] <- 'Government'


assignees_years_types.short <- assignees_years_types %>%  select(patent_id, year, thes_types)

assignees_years_types.ratio <- assignees_years_types.short %>% 
  group_by(patent_id) %>% 
  mutate(weight = 1/n()) 


grouped_assignees_types <- assignees_years_types.ratio %>% 
  group_by(year, thes_types) %>% 
  dplyr::summarize(type_count = sum(weight))

total_count_assignees_types <- assignees_years_types.ratio %>% 
  group_by(year) %>% 
  dplyr::summarize(total = sum(weight))

for_graph_types <- grouped_assignees_types %>% 
  left_join(total_count_assignees_types, by = 'year') %>% 
  mutate(PercentageofAssignees = type_count/total) %>% 
  filter(thes_types != "Other")


#make graph with looked up types **
graph1 <- ggplot(for_graph_types, aes(x=year,y=PercentageofAssignees,group=thes_types,linetype=thes_types, colour=thes_types)) +
  geom_line(size=1.5) + labs(y= "Percentage of Patents", x="Year") +
  scale_x_continuous(breaks=c(1980,1985,1990,1995,2000,2005,2010,2015)) +
  scale_colour_manual(values=c(darkPurple, cyan, darkGreen, darkGrey)) +
  scale_linetype_manual(values=c("dashed", "twodash", "solid")) +
  expand_limits(y = 0) +
  scale_y_continuous(labels = scales::percent) +
  theme_set(theme_gray(base_size = 16)) + 
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_blank(),
        legend.key.width = unit(3, 'lines'),
        legend.title=element_blank(),
        text=element_text(size=16,  family="Cambria")
  ) 

graph1
dev.copy(png, "data_viz/looked_up_gi_patent_assignees_over_time.png")
dev.off()
CairoPDF(file= "data_viz/looked_up_gi_patent_assignees_over_time",  width = 9, height = 7)
graph1
dev.off()


write.csv(for_graph_types, "out/sectors_over_time.csv")
