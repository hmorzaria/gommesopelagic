#' @title plot diet matrix
#' @description  Plot availability matrix as a food web network diagram
#' @details INPUT: 1) prey names, 2) group names, 3) availability matrix from the prm
#' @details OUTPUT: 1) network of predation on mesopelagics
#' @details Used https://briatte.github.io/ggnet/ 
#' @details Used ggraph https://www.jessesadler.com/post/network-analysis-with-r/
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com



diet_plot <- function(preynames,groupnames,availabilitymatrix,filename,network.type,fishguilds) {
  
  prey.names <- fread(preynames, header=TRUE) %>% 
    dplyr::select(prey) %>% 
    .$prey %>% 
    as.character() %>% 
    tolower()

  fish.guildnames <- fread(fishguilds, header = TRUE) %>% 
    mutate(`predator name` = tolower(long_name))
  
  functional.groups <- fread(groupnames, header=TRUE) %>% tbl_df %>% 
    mutate(`predator name`=tolower(`Long Name`)) %>% 
    dplyr::select(`predator name`, Code, guild) %>% 
    setNames(c("predator name", "predator","guild")) %>% 
    left_join(fish.guildnames, by="predator name") %>% 
    mutate(habitat_classification=if_else(is.na(habitat_classification), guild.x,habitat_classification)) %>%
    dplyr::select(`predator name`, predator, habitat_classification)
    

  functional.groups.names <- functional.groups %>% 
    dplyr::select(-habitat_classification) 
  
  prm.data <-  scan(availabilitymatrix, character(0), sep = "\n")
  
  # separate each line
  
  group.no <- "94"
  
  prey.test <- regexpr(pattern = "pPREY", text = prm.data) %>% 
    gsub("-1",FALSE,.) %>% 
    gsub("1",TRUE,.) %>% 
    gsub("2",TRUE,.) %>% 
    gsub("3",TRUE,.) %>% 
    as.logical() 
  
  prey.headers <- prm.data[prey.test] %>% 
    as.data.frame() %>% tbl_df %>% 
    na.omit %>% 
    setNames("prey") %>% 
    filter(!grepl("is the availability",prey)) %>% 
    separate(prey,c("name","groups")," ") %>% 
    mutate(name=gsub(",",'',name)) %>% 
    mutate(name=gsub("94",'',name)) %>% 
    dplyr::select(name)
  
  numeric.test <- regexpr(pattern = "pPREY", text = prm.data) %>% 
    gsub("-1",TRUE,.) %>% 
    gsub("1",FALSE,.) %>% 
    gsub("2",FALSE,.) %>% 
    gsub("3",FALSE,.) %>% 
    as.logical() 
  
  all.prey.data <- prm.data[numeric.test] %>% 
    as.data.frame() %>% tbl_df %>% 
    na.omit %>% 
    setNames("prey") %>% 
    separate(prey,prey.names," ") %>% 
    mutate_each(funs(as.numeric)) %>% 
    na.omit %>% 
    bind_cols(prey.headers) %>% 
    dplyr::select(name, everything()) %>% 
    mutate(group = name, group = gsub("pPREY","",group), group = if_else(nchar(group)==3,paste(2,group,2,sep=""),group)) %>% 
    mutate(group = if_else(nchar(group)==2,paste(2,group,92,sep=""),group)) %>% 
    separate(group,c("prey type","predator"),1) %>% 
    separate(predator,c("predator","predator type"),3) %>% 
    mutate(predator = gsub("9","",predator)) %>% 
    mutate(`prey type`=ifelse(`prey type`==1,"juvenile","adult")) %>% 
    mutate(`predator type`=ifelse(`predator type`==1,"juvenile","adult")) %>% 
    left_join(functional.groups.names,by="predator") %>% 
    mutate(index= as.factor(1:nrow(.))) %>% 
    dplyr::select(index, name,predator,`predator name`,`predator type`,`prey type`, everything())

  demersal.sp <- c("deep water fish","other demersal fish","small demersal fish")
  
  prey.table <- all.prey.data %>% 
  gather(`prey name`,availability, -index,-name,-predator,-`predator name`,-`predator type`,-`prey type`) %>% 
  group_by(`predator name`,`prey name`) %>% 
  summarise(tot_availability = sum(availability)) %>% 
    #mutate(tot_availability = if_else(tot_availability!=0,1,0)) %>% 
    ungroup %>% 
    filter(!grepl("sediment",`prey name`)) %>% 
    filter(`prey name`%in% demersal.sp | `predator name`%in% demersal.sp) %>% 
    filter(tot_availability!=0) %>% 
    complete(`predator name`,`prey name`, fill = list(tot_availability=0))
  
  preys <- prey.table %>% distinct(`prey name`) 
  
  preds <- prey.table %>% distinct(`predator name`) 
  
  ps.preymatrix.missing <- preys %>% 
    rename(`predator name` = `prey name`) %>% 
    bind_rows(preds) %>% 
    distinct(`predator name`)
    
  ps.preydata <- preds %>% 
    rename(`prey name` = `predator name`) %>% 
    bind_rows(preys) %>% 
    distinct(`prey name`) %>% 
    bind_cols(ps.preymatrix.missing) %>% 
    mutate(tot_availability = 0) %>% 
    bind_rows(prey.table) %>% 
    group_by(`predator name`,`prey name`) %>% 
    summarise(availability = sum(tot_availability)) %>% 
    ungroup
  
  if(network.type=="prey"){
  
    ps.preymatrix <- ps.preydata %>% 
      mutate(availability=if_else(`predator name` %in% demersal.sp,0,availability)) %>%  
      complete(`predator name`,`prey name`, fill = list(availability=0)) %>% 
      spread(`prey name`,availability) 
    
    ps.matrix.test <- ps.preymatrix %>% 
      dplyr::select(-`predator name`) %>% 
      as.matrix
    
    test.matrix <- tibble(species=ps.preymatrix$`predator name`,row.tot=rowSums(ps.matrix.test), col.tot = colSums(ps.matrix.test)) %>% 
      filter(row.tot == 0 & col.tot == 0) %>% 
      pull(species)
    
    ps.matrix <- ps.preymatrix %>% 
      filter(!`predator name` %in% test.matrix) %>% 
      dplyr::select(-`predator name`,-all_of(test.matrix)) %>% 
      as.matrix
    
    prey.groupnames <- ps.preymatrix %>% 
      filter(!`predator name` %in% test.matrix) %>% 
      dplyr::select(-all_of(test.matrix)) %>% 
      dplyr::select(`predator name`) %>% 
      left_join(functional.groups, by="predator name")
  
    #used this to get mesopelagic prey
    # ps.preymatrix %>% 
    #  dplyr::select(`predator name`,`deep water fish`,`small demersal fish`,`other demersal fish`) %>% 
    #  group_by(`predator name`) %>% 
    #  mutate(tot_prey = sum(`deep water fish`,`small demersal fish` ,`other demersal fish`)) %>% 
    #  filter(tot_prey>0) %>% 
    #  write_csv("mesopelagic_pred.csv")
    
    colnames(ps.matrix) <- NULL
    
    nodes <- prey.groupnames %>% 
      mutate(id=1:nrow(.)) %>% 
      rename(label=`predator name`) %>% 
      dplyr::select(id, label, habitat_classification) %>% 
      mutate(label = str_to_sentence(label))
    
    write_csv(nodes,"mesopelagic_predators.csv")
    
    prey.grouplabels <- prey.groupnames %>% 
      pull(`predator name`)
    
    edges <- ps.matrix %>% 
      as.data.frame() %>% 
      mutate(from=paste0("V",1:nrow(.))) %>% 
      gather(key="to",value="weight",-from) %>% 
      filter(weight!=0) %>% 
      mutate(from=as.numeric(gsub("V","",from))) %>% 
      mutate(to=as.numeric(gsub("V","",to))) %>% 
      as_tibble
    
    routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
      
  }
  
  # habitat classification
  # 1 Reef-Associated       
  # 2 Pelagic-Oceanic       
  # 3 Demersal              
  # 4 Mesopelagic           
  # 5 Seabirds              
  # 6 Pelagic-Neritic       
  # 7 Bathydemersal  
  
  mycols2 <- redmonder.pal(8,"qMSOSlp")[c(1,3,8,4:7)]
  
ggraph.avail <- ggraph(routes_tidy, layout = "graphopt") + 
    geom_node_point(aes(colour=habitat_classification), size=4) +
    geom_edge_link(aes(width = weight), alpha = 0.8, edge_colour = "grey66", show.legend = F) + 
    scale_color_manual(values = mycols2, name = "Habitat classification")+
    scale_edge_width(range = c(0.5, 2)) +
    geom_node_text(aes(label = label), repel = TRUE) +
    labs(edge_width = "Availability") +
    theme_graph()

  #see https://mran.microsoft.com/snapshot/2017-03-06/web/packages/ggCompNet/vignettes/examples-from-paper.html
#creates network with nodes and edges
# net.prey  <- network(ps.matrix, directed = FALSE)
# 
# # vertex names
# network.vertex.names(net.prey) = ps.preymatrix$`predator name`
# 
# guild.names <- prey.groupnames %>% distinct(habitat_classification) %>% pull(habitat_classification)
# 
# pred.labels <- c("Reef fish",
#                  "Demersal",
#                  "Pelagic",
#                  "Mesopelagic" ,
#                  "Seabirds")   
# 
# pred.names <- prey.groupnames %>% 
#   arrange(`predator name`) %>% 
#   .$guild
# 
# pred.factors <- factor(pred.names, levels=pred.labels)
# 
# #prednames are "Reef fish"   "Pelagic"     "Demersal"    "Mesopelagic" "Seabirds" 
# 
# #create a divergent color palette
# #https://rgbcolorcode.com
# #almond, dark sienna, 
# #apple green, alloy orange,brown,saddle brown,chocolate
# #opera mauve,
# #moonstone blue, cornflower blue, lapizlazuli, crimson glory
# #dark goldenrod, cool grey, feldgrau
# col.palette <- c("#77B300",
#                  "#B284BE",
#                  "#72A0C1",
#                  "#6699FF",
#                  "#afa517")
# 
# 
# names(col.palette) <- levels(pred.factors)

  # net.graph <- ggnet2(net.prey, color = pred.factors, color.legend = "Guild", palette = col.palette, size = 5, 
  #                     alpha=0.8, edge.alpha = 0.7,vjust = -0.6, mode = "kamadakawai")
   

  ggsave(filename, plot = ggraph.avail, device = "png", width = 11, height = 6)
  
  return(ggraph.avail)
}


