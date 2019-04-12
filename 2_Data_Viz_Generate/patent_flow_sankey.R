
# Top Gov Interest to assignee Patenting Flows
# Figure A in gov interest data brief 

  
  # process assignees table
  in.assignees <- in.assignees.all %>% 
    filter(patent_id %in% patents.keep_ids)
  
  in.assignees$name <- paste (in.assignees$name_first, in.assignees$name_last, sep = " ")
  in.assignees$entity <- in.assignees$organization
  in.assignees$entity[which(in.assignees$entity == "")] <- in.assignees$name[which(in.assignees$entity == "")]
  
  assignees.clnd <- in.assignees[,c(1,2,8)]
  
  #calculate the weight for each patent_id
  assignees.clnd <- assignees.clnd %>%  
    group_by(patent_id) %>% 
    mutate(weight = 1/n())
  
  assignees.merged <- assignees.clnd %>%  inner_join(in.patent_level.merged, by="patent_id")
  assignees.merged$entity <- toTitleCase(tolower(assignees.merged$entity))
  
  
  # sankey viz for funders --> assignees to do the network viz
  
  assignees.merged.sub <- assignees.merged[,c(1, 3, 23)] # patent_id, entity, level_one
  ass_org.merged.ratio <- assignees.merged.sub %>%  
    distinct() %>% 
    group_by(patent_id) %>% 
    mutate(weight = 1/n())
  
  count.ass_org.srtd <- ass_org.merged.ratio %>% 
    group_by(entity, level_one) %>% 
    summarise(freq = sum(weight)) %>% 
    arrange(desc(freq))
  
  top.rows <- 30
  
  count.ass_org.srtd <- count.ass_org.srtd[1:top.rows,]
  count.ass_org.srtd <- count.ass_org.srtd[!grepl("The United States of America as Represented by the United States Department of Energy", count.ass_org.srtd$entity), ]
  count.ass_org.srtd <- count.ass_org.srtd[!grepl("The United States of America as Represented by the Administrator of the National Aeronautics and Space Administration", count.ass_org.srtd$entity), ]
  count.ass_org.srtd <- count.ass_org.srtd[!grepl("The United States of America as Represented by the Secretary of the Navy", count.ass_org.srtd$entity), ]
  
  # Added
  count.ass_org.srtd <- count.ass_org.srtd[!grepl("The United States of America as Represented by the Secretary of the Air Force", count.ass_org.srtd$entity), ]
  count.ass_org.srtd <- count.ass_org.srtd[!grepl("The United States of America as Represented by the Secretary of the Army", count.ass_org.srtd$entity), ]
  count.ass_org.srtd <- count.ass_org.srtd[!grepl("United States Government", count.ass_org.srtd$entity), ]
  count.ass_org.srtd <- count.ass_org.srtd[!grepl("The United States of America as Represented by the United States of Department of Energy"  , count.ass_org.srtd$entity), ]
  count.ass_org.srtd <- count.ass_org.srtd[!grepl("The United States of America as Represented by the Administrator of the National Aeronautics & Space Administration"  , count.ass_org.srtd$entity), ]
  
  
  
  # create nodes
  top.rows <- nrow(count.ass_org.srtd)
  top.funders <- unique(count.ass_org.srtd[1:top.rows,2])
  top.assignees <- unique(count.ass_org.srtd[1:top.rows,1])
  nodes = unlist(c(top.funders, top.assignees))
  
  # create source 
  source <- list(count.ass_org.srtd$level_one)
  source_lst <- lapply(source, function(x) match(x,nodes))
  count.ass_org.srtd$source <- unlist(source_lst)
  
  # create target 
  target <- list(count.ass_org.srtd$entity)
  target_lst <- lapply(target, function(x) match(x,nodes))
  count.ass_org.srtd$target <- unlist(target_lst)
  
  # offset by 1 - so continous list of nodes starts at 0
  count.ass_org.srtd$source <- count.ass_org.srtd$source - 1
  count.ass_org.srtd$target <- count.ass_org.srtd$target - 1
 
  patent_flow_plot <- plot_ly(
    type = "sankey",
    orientation = "h",
    
    node = list(
      label = nodes,
      # color = json_data$data[[1]]$node$color,
      pad = 15,
      thickness = 15,
      line = list(
        color = "black",
        width = 0.5
      )
    ), 
    
    link = list(
      source = count.ass_org.srtd$source,
      target = count.ass_org.srtd$target,
      value =  count.ass_org.srtd$freq
      # color =  json_data$data[[1]]$link$color,
      # label =  json_data$data[[1]]$link$label
    )
  ) %>% 
    layout(
      title = "Top Government Interest Organizations to Assignees, 1980 - 2017",
      font = list(
        size = 10
      ),
      xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
      yaxis = list(showgrid = F, zeroline = F, showticklabels = F)
    )
  
  patent_flow_plot
  
  

