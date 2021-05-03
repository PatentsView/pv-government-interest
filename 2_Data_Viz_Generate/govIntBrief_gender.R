source("requirements.R")


#########################################################################################################

#read in helper data
in.patent_level <- read.csv(str_c(input_folder, "temp_patent_level_gi.csv"))

in.patent_level <- in.patent_level %>% 
  filter(year != "NULL" &  year >= 1980 & year <= 2018) 

#load inventor gender
in.inventor_has_fem <- read.csv(str_c(input_folder, "temp_gi_has_female_inv.csv"))
in.gov_level <- read.csv(str_c(input_folder,"temp_gi_level_gi.csv"))


in.patent_level$patent_id = in.patent_level$patent_id %>% as.character()
in.inventor_has_fem$patent_id = in.inventor_has_fem$patent_id %>% as.character()

#drop unknown gender and years outside range
gi_inventor_has_fem <- in.inventor_has_fem %>% 
  inner_join(in.patent_level, by ="patent_id") 

gi_inventor_has_fem.sub <- gi_inventor_has_fem %>% 
  filter(year >= 1980 & year <= 2018) %>% 
  filter(has_fem_inv==0 | has_fem_inv==1)

gi_inventor_has_fem$year <- as.numeric(gi_inventor_has_fem$year)
gi_inventor_has_fem$has_fem_inv <- as.numeric(gi_inventor_has_fem$has_fem_inv)

# organize the data to get the percentage of patents in each year with at least one woman
grouped_gi_inventor_has_fem <- gi_inventor_has_fem.sub %>% 
  group_by(year, has_fem_inv) %>% 
  summarise(count_inv = n())


total_count_gi_inventor_has_fem <- gi_inventor_has_fem %>% 
  group_by(year) %>% 
  summarise(total = length(year))
for_graph <- grouped_gi_inventor_has_fem %>% 
  inner_join(total_count_gi_inventor_has_fem, by = 'year') %>% 
  mutate(PercentageofPatents = count_inv/total)
for_graph$has_fem_inv <- as.character(for_graph$has_fem_inv)


graph1 <- ggplot(for_graph) +
  geom_line(aes(x=year,y=PercentageofPatents,color=has_fem_inv,linetype=has_fem_inv), size = 1.5) +
  ylab(label="Percentage of Patents") +  xlab("Year") +
  scale_colour_manual(values=c(cyan, darkPurple, darkGreen), labels=c("All Male Inventors", "At Least One Woman",  "All Unknown")) +
  scale_linetype_manual(values=c("solid", "dashed", "twodash"), labels=c("All Male Inventors",  "At Least One Woman", "All Unknown")) +
  scale_x_continuous(breaks=c(1980,1985,1990,1995,2000,2005,2010,2015)) +
  scale_y_continuous(labels = scales::percent) +
  expand_limits(y = 0) +
  theme_set(theme_gray(base_size = 16)) + 
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_blank(),
        legend.key.width = unit(3, 'lines'),
        legend.title=element_blank(),
        text=element_text(size=16,  family="Cambria")
  )
graph1
dev.copy(png, "data_viz/gi_teams_with_female_inventors_over_time.png")
dev.off()
CairoPDF(file= "data_viz/gi_teams_with_female_inventors_over_time_cairo",  width = 9, height = 7)
graph1
dev.off()

write.csv(for_graph, file=str_c(output_folder,"teams_with_at_least_one_woman.csv"))

##################################################
# percent of teams with at least one woman by team size

in.gender_wipo <- fread(str_c(input_folder,"temp_gi_inventor_gender.csv"))
in.gender_wipo$year = in.gender_wipo$date %>% as.character() %>% ymd() %>% year()
in.gender_wipo.sub <- in.gender_wipo %>% 
  filter(year >= 1980 & year <= 2018) %>% 
  select(patent_id, num_inventors, male) %>%  # patent_id, team_size, dumale
  filter(male==0 | male==1) 
in.gender_wipo.sub$male <- as.numeric(in.gender_wipo.sub$male)

women_in_teams <- in.gender_wipo.sub %>% 
  group_by(patent_id, num_inventors) %>% 
  mutate(num_men = sum(male)) %>% 
  mutate(percentWomen = (1 - num_men/num_inventors))

grouped_by.num_inventors <- women_in_teams %>% 
  filter(percentWomen > 0) %>% 
  group_by(num_inventors) %>% 
  dplyr::summarise(meanPercWomenOnTeam = mean(percentWomen), 
                   numPatents = length(patent_id))  

for_graph.num_inventors <- grouped_by.num_inventors %>% 
  filter(num_inventors <= 15)

graph2 <- ggplot(data=for_graph.num_inventors, aes(x=num_inventors, y=meanPercWomenOnTeam)) +
  geom_point(aes(colour = numPatents, size=numPatents)) + 
  labs(y= "Mean Percentage of Women\non Teams with At Least One Woman", x="Inventor Team Size", colour="Number of\nPatents", size="Number of\nPatents") +
  theme_set(theme_gray(base_size = 16)) + 
  scale_size_continuous(range = c(2, 16), labels = comma) +
  scale_colour_gradient(low = "blue", labels = comma) +
  expand_limits(y = 0) +
  scale_y_continuous(labels = scales::percent) + 
  guides(color=guide_legend(), size = guide_legend()) + 
  scale_x_continuous(breaks = rep(1:15), minor_breaks = rep(1:15)) +
  theme(
    legend.direction = 'horizontal', 
    legend.position = 'bottom',
    legend.key = element_blank(),
    legend.title.align = 0.5,
    legend.key.width = unit(3, 'lines'),
    text=element_text(size=16,  family="Cambria"),
    legend.box.background = element_rect(colour = lightGrey), 
    legend.background = element_blank()
  ) 
graph2
dev.copy(png, "data_viz/percent_with_female_inventor_by_team_size.png")
dev.off()
CairoPDF(file= "data_viz/percent_with_female_inventor_by_team_size",  width = 9, height = 7)
graph2
dev.off()

write.csv(for_graph.num_inventors, "out/mean_percent_of_teams_with_a_female_inventor.csv") 

