#' @title plot diet matrix
#' @description  Plot availability matrix as a food web network diagram
#' @details INPUT: 1) prey names, 2) group names, 3) availability matrix from the prm
#' @details OUTPUT: 1) Map of Atlantis polygons
#' @details Used https://briatte.github.io/ggnet/
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com



diet_plot <- function(preynames,groupnames,availabilitymatrix,filename) {
  
  prey.names <- fread(preynames, header=TRUE) %>% 
    dplyr::select(prey) %>% 
    .$prey %>% 
    as.character() %>% 
    tolower()
  
  functional.groups <- fread(groupnames, header=TRUE) %>% tbl_df %>% 
    mutate(`predator name`=tolower(`Long Name`)) %>% 
    dplyr::select(`predator name`, Code, guild) %>% 
    setNames(c("predator name", "predator","guild"))
  
  functional.groups.names <- functional.groups %>% 
    dplyr::select(-guild)
  
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
  
  prey.table <- all.prey.data %>% 
  gather(`prey name`,availability, -index,-name,-predator,-`predator name`,-`predator type`,-`prey type`) %>% 
  group_by(`predator name`,`prey name`) %>% 
  summarise(tot_availability = sum(availability)) %>% 
    mutate(tot_availability = if_else(tot_availability!=0,1,0)) %>% 
    ungroup %>% 
    filter(!grepl("sediment",`prey name`))
  
  preys <- prey.table %>% distinct(`prey name`) 
  
  preds <- prey.table %>% distinct(`predator name`) 
  
  ps.preymatrix.missing <- preys %>% 
    rename(`predator name` = `prey name`) %>% 
    bind_rows(preds) %>% 
    distinct(`predator name`)
    
  ps.preymatrix <- preds %>% 
    rename(`prey name` = `predator name`) %>% 
    bind_rows(preys) %>% 
    distinct(`prey name`) %>% 
    bind_cols(ps.preymatrix.missing) %>% 
    mutate(tot_availability = 0) %>% 
    bind_rows(prey.table) %>% 
    group_by(`predator name`,`prey name`) %>% 
    summarise(availability = sum(tot_availability)) %>% 
    ungroup %>%
    complete(`predator name`,`prey name`, fill = list(availability=0)) %>% 
    spread(`prey name`,availability)
    
    
    ps.matrix <- ps.preymatrix %>% 
    dplyr::select(-`predator name`) %>% 
    as.matrix
    
    ps.preymatrix %>% 
      dplyr::select(`predator name`,`deep water fish`,`small demersal fish`,`other demersal fish`) %>% 
      group_by(`predator name`) %>% 
      mutate(tot_prey = sum(`deep water fish`,`small demersal fish` ,`other demersal fish`)) %>% 
      filter(tot_prey>0) %>% 
      write_csv("mesopelagic_pred.csv")
  
  colnames(ps.matrix) <- NULL
  
  #creates network with nodes and edges
  net.prey  <- network(ps.matrix, directed = FALSE)
  
  # vertex names
  network.vertex.names(net.prey) = ps.preymatrix$`predator name`
  
  #order of colors is
  #bacteria
  #primary producers
  #zooplankton
  #invertebrates
  #forage fish
  #salmon
  #demersal fish
  #elasmobranchs
  #birds
  #marine mammals
  #detritus
  
  pred.labels <- c("Bacteria","Detritus",
                   "Primary producers","Plankton","Infauna","Filter feeders","Epibenthos",
                   "Squid",
                   "Reef fish" ,"Demersal","Pelagic","Mesopelagic" ,
                   "Seabirds","Marine mammals", "Sea turtles")   
                    
  
  pred.names <- functional.groups %>% 
    arrange(`predator name`) %>% 
    .$guild
  
  pred.factors <- factor(pred.names, levels=pred.labels)
  
  #create a divergent color palette
  #https://rgbcolorcode.com
  #almond, dark sienna, 
  #apple green, alloy orange,brown,saddle brown,chocolate
  #opera mauve,
  #moonstone blue, cornflower blue, lapizlazuli, crimson glory
  #dark goldenrod, cool grey, feldgrau
  col.palette <- c("#EFDECD","#4D2600",
                   "#77B300","#C46210","#9c4e0d","#85420b","#783c00",
                   "#B284BE",
                   "#72A0C1","#6699FF","#3e57a8","#AF002A",
                   "#afa517","#8597af","#48614e")
                  
  
  names(col.palette) <- levels(pred.factors)
  
  #see https://mran.microsoft.com/snapshot/2017-03-06/web/packages/ggCompNet/vignettes/examples-from-paper.html
  
  net.graph <- ggnet2(net.prey, color = pred.factors, color.legend = "Guild", palette = col.palette, size = 5, 
                      alpha=0.8, edge.alpha = 0.7,vjust = -0.6, mode = "kamadakawai")
  
  #net.graph <- ggnet2(net.prey, color = pred.factors, color.legend = "Guild", palette = col.palette, size = 6, alpha=0.75, edge.alpha = 0.75,
  #edge.color = c("color", "grey50"), label = c("Mesopelagic"), label.size = 4)


  ggsave(filename, plot = net.graph, device = "png", width = 10, height = 6)
  
  return(net.graph)
}


